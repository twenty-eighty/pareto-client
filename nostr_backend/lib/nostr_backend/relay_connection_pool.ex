defmodule NostrBackend.RelayConnectionPool do
  use GenServer
  require Logger

  @type relay_url :: binary()
  @type relay_urls :: [relay_url()]
  @type subscription_id :: binary()
  @type nostr_event :: map()
  @type event_list :: [nostr_event()]
  @type filters :: [map()]

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Fetches data from relays using the connection pool.
  Returns {:ok, relay_url, data} or {:error, reason}
  """
  @spec fetch_from_relays(relay_urls(), any(), atom()) :: {:ok, relay_url(), event_list()} | {:error, binary()}
  def fetch_from_relays(relay_urls, id, type) do
    GenServer.call(__MODULE__, {:fetch_from_relays, relay_urls, id, type}, :infinity)
  end

  # Server callbacks

  @impl true
  def init(_) do
    state = %{
      connections: %{},           # relay_url => [{pid, last_used, status}, ...]
      pending_requests: %{},      # subscription_id => {from, filters, connection_pid}
      active_subscriptions: %{},  # connection_pid => [subscription_id, ...]
      subscription_activity: %{}, # subscription_id => {connection_pid, last_event_time}
      timeout_timers: %{},        # subscription_id => timer_ref
      event_collectors: %{},      # subscription_id => collector_pid
      request_queue: %{},         # relay_url => [{subscription_id, from, filters, connection_pid}, ...]
      pagination_sessions: %{},   # subscription_id => {original_from, relay_url, all_events, last_timestamp, request_type}
      max_connections_per_relay: 3, # Increased from 1 to 3 to allow more concurrent requests
      connection_timeout: 60_000,    # 60 seconds
      cleanup_interval: 30_000,      # 30 seconds
      request_timeout: 15_000,       # 15 seconds
      activity_timeout: 5_000,       # 5 seconds after last event
      request_delay: 500,            # 500ms delay between requests to same relay
      max_pagination_requests: 10    # Maximum number of pagination requests to prevent infinite loops
    }

    # Schedule cleanup
    schedule_cleanup(state.cleanup_interval)

    {:ok, state}
  end

  @impl true
  def handle_call({:fetch_from_relays, relay_urls, id, type}, from, state) do
    # Try to fetch from relays in order
    case fetch_from_relays_impl(relay_urls, id, type, from, state) do
      {:ok, new_state} ->
        {:noreply, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

    @impl true
  def handle_info({:collect_events, subscription_id, events, relay_url}, state) do
    case Map.get(state.pending_requests, subscription_id) do
      {caller_from, _filters, connection_pid} ->
        # Check if this is a pagination session
        case Map.get(state.pagination_sessions, subscription_id) do
          {original_from, session_relay_url, all_events, last_timestamp, request_type, request_count} ->
            # This is a pagination request
            handle_pagination_response(subscription_id, events, relay_url, original_from, session_relay_url, all_events, last_timestamp, request_type, request_count, state)
          nil ->
            # This is a regular request - check if we need pagination
            handle_regular_response(subscription_id, events, relay_url, caller_from, connection_pid, state)
        end

      nil ->
        Logger.warning("Received collect_events for unknown subscription: #{subscription_id}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:event, subscription_id, event}, state) do
    # Check if this subscription is active for any connection
    is_active = Enum.any?(state.active_subscriptions, fn {_connection_pid, subscriptions} ->
      subscription_id in subscriptions
    end)

    if is_active do
      # Update activity timestamp for this subscription
      new_subscription_activity = case Map.get(state.subscription_activity, subscription_id) do
        {connection_pid, _old_time} ->
          Map.put(state.subscription_activity, subscription_id, {connection_pid, System.monotonic_time(:millisecond)})
        nil ->
          # If not in activity tracking, add it (this shouldn't happen but just in case)
          case Map.get(state.pending_requests, subscription_id) do
            {_from, _filters, connection_pid} ->
              Map.put(state.subscription_activity, subscription_id, {connection_pid, System.monotonic_time(:millisecond)})
            nil ->
              state.subscription_activity
          end
      end

      # Forward the event to the EventCollector process
      case Registry.lookup(NostrBackend.Registry, {:event_collector, subscription_id}) do
        [] ->
          Logger.warning("No EventCollector found for subscription: #{subscription_id}")
          {:noreply, %{state | subscription_activity: new_subscription_activity}}
        [{collector_pid, _}] ->
          send(collector_pid, {:event, subscription_id, event})
          {:noreply, %{state | subscription_activity: new_subscription_activity}}
      end
    else
      Logger.warning("Received event for unknown subscription: #{subscription_id}")
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:eose, subscription_id}, state) do
    # Check if this subscription is active for any connection
    is_active = Enum.any?(state.active_subscriptions, fn {_connection_pid, subscriptions} ->
      subscription_id in subscriptions
    end)

    if is_active do
      # Forward EOSE to the EventCollector process
      case Registry.lookup(NostrBackend.Registry, {:event_collector, subscription_id}) do
        [] ->
          Logger.warning("No EventCollector found for EOSE subscription: #{subscription_id}")
          {:noreply, state}
        [{collector_pid, _}] ->
          send(collector_pid, {:eose, subscription_id})
          {:noreply, state}
      end
    else
      Logger.warning("Received EOSE for unknown subscription: #{subscription_id}")
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:notice, subscription_id, notice}, state) do
    # Forward notice to the EventCollector process
    case Registry.lookup(NostrBackend.Registry, {:event_collector, subscription_id}) do
      [] ->
        Logger.warning("No EventCollector found for notice subscription: #{subscription_id}")
        {:noreply, state}
      [{collector_pid, _}] ->
        send(collector_pid, {:notice, subscription_id, notice})
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:notice, notice}, state) do
    # Handle notices without subscription_id (like "ERROR: too many concurrent REQs")
    Logger.warning("Relay notice: #{notice}")

    # If it's a "too many concurrent REQs" error, we should reduce our connection usage
    if String.contains?(notice, "too many concurrent REQs") do
      Logger.warning("Relay is rejecting requests due to too many concurrent connections")
      # Mark all connections as busy to prevent new requests temporarily
      new_connections = Enum.reduce(state.connections, state.connections, fn {relay_url, connections}, acc ->
        updated_connections = Enum.map(connections, fn {pid, last_used, _status} ->
          {pid, last_used, :busy}
        end)
        Map.put(acc, relay_url, updated_connections)
      end)
      {:noreply, %{state | connections: new_connections}}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:timeout, subscription_id}, state) do
    case Map.get(state.pending_requests, subscription_id) do
      {caller_from, _filters, connection_pid} ->
        # Use GenServer.reply for GenServer.call responses
        GenServer.reply(caller_from, {:error, "Request timeout"})

        # Mark connection as available
        new_connections = update_connection_status(state.connections, connection_pid, :available)

        # Remove from pending requests but keep in active_subscriptions
        new_pending_requests = Map.delete(state.pending_requests, subscription_id)

        # Track activity for this subscription
        new_subscription_activity = Map.put(state.subscription_activity, subscription_id, {connection_pid, System.monotonic_time(:millisecond)})

        # Clean up timer and EventCollector
        new_timeout_timers = case Map.get(state.timeout_timers, subscription_id) do
          nil -> state.timeout_timers
          timer_ref when is_reference(timer_ref) ->
            Process.cancel_timer(timer_ref)
            Map.delete(state.timeout_timers, subscription_id)
        end
        new_event_collectors = case Map.get(state.event_collectors, subscription_id) do
          nil -> state.event_collectors
          collector_pid ->
            Process.exit(collector_pid, :kill)
            Map.delete(state.event_collectors, subscription_id)
        end

        new_state = %{state |
          connections: new_connections,
          pending_requests: new_pending_requests,
          subscription_activity: new_subscription_activity,
          timeout_timers: new_timeout_timers,
          event_collectors: new_event_collectors
        }

        # Process any queued requests
        {new_request_queue, final_state} = process_queued_requests(get_relay_url_for_connection(state.connections, connection_pid), new_state)

        {:noreply, %{final_state | request_queue: new_request_queue}}

      nil ->
        Logger.warning("Timeout for unknown subscription: #{subscription_id}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_info(:cleanup, state) do
    # Clean up idle connections
    new_connections = cleanup_idle_connections(state.connections, state.connection_timeout)

    # Clean up inactive subscriptions
    now = System.monotonic_time(:millisecond)
    {new_active_subscriptions, new_subscription_activity} = cleanup_inactive_subscriptions(
      state.active_subscriptions,
      state.subscription_activity,
      now,
      state.activity_timeout
    )

    new_state = %{state |
      connections: new_connections,
      active_subscriptions: new_active_subscriptions,
      subscription_activity: new_subscription_activity
    }

    # Schedule next cleanup
    schedule_cleanup(state.cleanup_interval)

    {:noreply, new_state}
  end

  @impl true
  def handle_info({:disconnect, relay_url, reason}, state) do
    Logger.warning("Relay connection pool: Disconnected from #{relay_url} (reason: #{inspect(reason)})")

    # Remove the connection from the pool
    new_connections = Map.delete(state.connections, relay_url)

    # Log connection pool status
    total_connections = map_size(new_connections)
    Logger.info("Connection pool status after disconnect: #{total_connections} relay(s) connected")

    # If this was our only connection to this relay, log a warning
    case Map.get(state.connections, relay_url) do
      nil ->
        Logger.warning("Lost connection to #{relay_url} - no more connections available")
      _ ->
        Logger.info("Still have other connections to #{relay_url}")
    end

    {:noreply, %{state | connections: new_connections}}
  end

  @impl true
  def handle_info(message, state) do
    # Catch-all handler for unhandled messages
    Logger.warning("RelayConnectionPool received unhandled message: #{inspect(message)}")
    {:noreply, state}
  end

  # Private functions

  defp fetch_from_relays_impl([], _id, _type, _from, _state) do
    Logger.error("Failed to fetch data from all relays")
    {:error, "Failed to fetch data from all relays"}
  end

  defp fetch_from_relays_impl([relay_url | rest], id, type, from, state) do
    case get_or_create_connection(relay_url, state) do
      {:ok, connection_pid, new_state} ->
        # Send the request
        case send_request(connection_pid, id, type, from, new_state) do
          {:ok, final_state} ->
            {:ok, final_state}
          {:error, reason} ->
            Logger.warning("Failed to fetch from relay #{relay_url}: #{inspect(reason)}, trying next relay")
            fetch_from_relays_impl(rest, id, type, from, new_state)
        end

      {:error, reason} ->
        Logger.warning("Failed to get connection for relay #{relay_url}: #{inspect(reason)}, trying next relay")
        fetch_from_relays_impl(rest, id, type, from, state)
    end
  end

  defp get_or_create_connection(relay_url, state) do
    case Map.get(state.connections, relay_url) do
      nil ->
        # No connection exists, create one
        create_connection(relay_url, state)

      connections ->
        # Check if we can reuse an available connection
        case Enum.find(connections, fn {_pid, _last_used, status} -> status == :available end) do
          nil ->
            # No available connection, check if we can create a new one
            case can_create_new_connection(relay_url, state) do
              true -> create_connection(relay_url, state)
              false -> {:error, "Connection pool full for #{relay_url}"}
            end
          {pid, _last_used, :available} ->
            # Found an available connection, reuse it
            new_connections = update_connection_last_used(state.connections, relay_url, pid)
            {:ok, pid, %{state | connections: new_connections}}
        end
    end
  end

  defp create_connection(relay_url, state) do
    Logger.debug("Creating new connection to relay: #{relay_url}")

    case NostrBackend.RelayConnection.start_link(relay_url, self()) do
      {:ok, pid} ->
        new_connection = {pid, System.monotonic_time(:millisecond), :available}
        existing_connections = Map.get(state.connections, relay_url, [])
        new_connections = Map.put(state.connections, relay_url, [new_connection | existing_connections])
        {:ok, pid, %{state | connections: new_connections}}

      {:error, reason} ->
        Logger.error("Failed to create connection to relay #{relay_url}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp can_create_new_connection(relay_url, state) do
    # Count existing connections for this specific relay
    existing_count = case Map.get(state.connections, relay_url) do
      nil -> 0
      connections -> length(connections)
    end

    existing_count < state.max_connections_per_relay
  end

  defp send_request(connection_pid, id, type, from, state) do
    subscription_id = UUID.uuid4()
    filters = build_filters(id, type)
    relay_url = get_relay_url_for_connection(state.connections, connection_pid)

    # Check if there are already active subscriptions on this connection
    active_count = case Map.get(state.active_subscriptions, connection_pid) do
      nil -> 0
      subscriptions -> length(subscriptions)
    end

    if active_count >= 3 do
      # Too many active subscriptions, queue this request
      queue_item = {subscription_id, from, filters, connection_pid}
      existing_queue = Map.get(state.request_queue, relay_url, [])
      new_request_queue = Map.put(state.request_queue, relay_url, [queue_item | existing_queue])

      Logger.debug("Queuing request for relay #{relay_url} (active: #{active_count})")

      {:ok, %{state | request_queue: new_request_queue}}
    else
      # Send the request immediately
      send_queued_request(subscription_id, from, filters, connection_pid, state)
    end
  end

  defp send_queued_request(subscription_id, from, filters, connection_pid, state) do
    # Mark connection as busy
    new_connections = update_connection_status(state.connections, connection_pid, :busy)

    # Store the pending request
    new_pending_requests = Map.put(state.pending_requests, subscription_id, {from, filters, connection_pid})

    # Track this subscription for the connection
    existing_subscriptions = Map.get(state.active_subscriptions, connection_pid, [])
    new_active_subscriptions = Map.put(state.active_subscriptions, connection_pid, [subscription_id | existing_subscriptions])

    # Add delay to prevent overwhelming the relay
    Process.sleep(state.request_delay)

    # Send the request
    case NostrBackend.RelayConnection.send_request(connection_pid, subscription_id, filters) do
      :ok ->
        # Start event collection process
        relay_url = get_relay_url_for_connection(state.connections, connection_pid)
        {:ok, collector_pid} = NostrBackend.EventCollector.start_collecting(subscription_id, relay_url, self(), state.request_timeout)

        # Set up timeout and store the timer reference
        timer_ref = Process.send_after(self(), {:timeout, subscription_id}, state.request_timeout)
        new_timeout_timers = Map.put(state.timeout_timers, subscription_id, timer_ref)
        new_event_collectors = Map.put(state.event_collectors, subscription_id, collector_pid)

        {:ok, %{state |
          connections: new_connections,
          pending_requests: new_pending_requests,
          active_subscriptions: new_active_subscriptions,
          timeout_timers: new_timeout_timers,
          event_collectors: new_event_collectors
        }}

      {:error, reason} ->
        # Mark connection as available again
        _new_connections = update_connection_status(new_connections, connection_pid, :available)
        {:error, reason}
    end
  end

  defp update_connection_status(connections, target_connection_pid, status) do
    Enum.reduce(connections, connections, fn {relay_url, existing_connections}, acc ->
      new_connections = Enum.map(existing_connections, fn {pid, last_used, old_status} ->
        if pid == target_connection_pid do
          {pid, last_used, status}
        else
          {pid, last_used, old_status}
        end
      end)
      Map.put(acc, relay_url, new_connections)
    end)
  end

  defp update_connection_last_used(connections, relay_url, target_pid) do
    case Map.get(connections, relay_url) do
      nil ->
        connections
      existing_connections ->
        new_connections = Enum.map(existing_connections, fn {pid, last_used, status} ->
          if pid == target_pid do
            {pid, System.monotonic_time(:millisecond), status}
          else
            {pid, last_used, status}
          end
        end)
        Map.put(connections, relay_url, new_connections)
    end
  end

  defp cleanup_idle_connections(connections, timeout) do
    now = System.monotonic_time(:millisecond)

    Enum.reduce(connections, connections, fn {relay_url, existing_connections}, acc ->
      new_connections = Enum.filter(existing_connections, fn {pid, last_used, status} ->
        if status == :available and (now - last_used) > timeout do
          Logger.debug("Cleaning up idle connection to #{relay_url}")
          Process.exit(pid, :normal)
          false
        else
          true
        end
      end)
      Map.put(acc, relay_url, new_connections)
    end)
  end

  defp schedule_cleanup(interval) do
    Process.send_after(self(), :cleanup, interval)
  end

  defp get_relay_url_for_connection(connections, connection_pid) do
    Enum.find_value(connections, fn {relay_url, existing_connections} ->
      if Enum.any?(existing_connections, fn {pid, _last_used, _status} -> pid == connection_pid end) do
        relay_url
      else
        nil
      end
    end)
  end

  # Filter building functions (moved from NostrClient)

  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "authors" => [author],
        "#d" => [identifier]
      }
    ]
  end

  defp build_filters(%{kind: kind, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "#d" => [identifier]
      }
    ]
  end

  defp build_filters(%{kind: kind, pubkey: pubkey, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "authors" => [pubkey],
        "#d" => [identifier]
      }
    ]
  end

  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :community) do
    [
      %{
        "kinds" => [kind],
        "authors" => [author],
        "#d" => [identifier]
      }
    ]
  end

  defp build_filters(%{id: id}, :event) do
    [
      %{
        "ids" => [id]
      }
    ]
  end

  defp build_filters(note_id, :note) do
    [
      %{
        "ids" => [note_id]
      }
    ]
  end

  defp build_filters(event_id, :article), do: [%{"ids" => [event_id]}]

  defp build_filters(%{id: id}, :picture_post),
    do: [%{ "ids" => [id], "kinds" => [20], "limit" => 1}]

  defp build_filters(pubkey, :profile),
    do: [%{"authors" => [pubkey], "kinds" => [0], "limit" => 1}]

  defp build_filters(pubkey, :follow_list) do
    [
      %{
        "authors" => [pubkey],
        "kinds" => [3],
        "limit" => 1
      }
    ]
  end

  defp build_filters(pubkey, :author_articles) do
    [
      %{
        "authors" => [pubkey],
        "kinds" => [30023],  # Article kind
        "limit" => 1000  # Adjust this based on your needs
      }
    ]
  end

  defp build_filters(pubkeys, :multiple_authors_articles) do
    Logger.info("Building filters for multiple authors articles: #{length(pubkeys)} authors")
    filters = [
      %{
        "authors" => pubkeys,
        "kinds" => [30023],  # Article kind
        "limit" => 1000  # Adjust this based on your needs
      }
    ]
    Logger.info("Built filters: #{inspect(filters)}")
    filters
  end

  defp remove_subscription_from_connection(active_subscriptions, connection_pid, subscription_id) do
    case Map.get(active_subscriptions, connection_pid) do
      nil ->
        active_subscriptions
      subscriptions ->
        new_subscriptions = List.delete(subscriptions, subscription_id)
        if new_subscriptions == [] do
          Map.delete(active_subscriptions, connection_pid)
        else
          Map.put(active_subscriptions, connection_pid, new_subscriptions)
        end
    end
  end

  defp cleanup_inactive_subscriptions(active_subscriptions, subscription_activity, now, activity_timeout) do
    # Find inactive subscriptions
    inactive_subscriptions = Enum.filter(subscription_activity, fn {_subscription_id, {_connection_pid, last_event_time}} ->
      (now - last_event_time) >= activity_timeout
    end)
    |> Enum.map(fn {subscription_id, _} -> subscription_id end)

    # Remove inactive subscriptions from active_subscriptions
    new_active_subscriptions = Enum.reduce(active_subscriptions, active_subscriptions, fn {connection_pid, subscriptions}, acc ->
      new_subscriptions = Enum.filter(subscriptions, fn subscription_id ->
        subscription_id not in inactive_subscriptions
      end)

      if new_subscriptions == [] do
        Map.delete(acc, connection_pid)
      else
        Map.put(acc, connection_pid, new_subscriptions)
      end
    end)

    # Remove inactive subscriptions from subscription_activity
    new_subscription_activity = Enum.reduce(inactive_subscriptions, subscription_activity, fn subscription_id, acc ->
      Map.delete(acc, subscription_id)
    end)

    {new_active_subscriptions, new_subscription_activity}
  end

  defp process_queued_requests(relay_url, state) do
    case Map.get(state.request_queue, relay_url) do
      nil ->
        {state.request_queue, state}
      [] ->
        {state.request_queue, state}
      [{subscription_id, from, filters, connection_pid} | rest] ->
        # Mark connection as busy
        new_connections = update_connection_status(state.connections, connection_pid, :busy)

        # Store the pending request
        new_pending_requests = Map.put(state.pending_requests, subscription_id, {from, filters, connection_pid})

        # Track this subscription for the connection
        existing_subscriptions = Map.get(state.active_subscriptions, connection_pid, [])
        new_active_subscriptions = Map.put(state.active_subscriptions, connection_pid, [subscription_id | existing_subscriptions])

        # Add delay to prevent overwhelming the relay
        Process.sleep(state.request_delay)

        # Send the request
        case NostrBackend.RelayConnection.send_request(connection_pid, subscription_id, filters) do
          :ok ->
            # Start event collection process
            relay_url = get_relay_url_for_connection(state.connections, connection_pid)
            {:ok, collector_pid} = NostrBackend.EventCollector.start_collecting(subscription_id, relay_url, self(), state.request_timeout)

            # Set up timeout and store the timer reference
            timer_ref = Process.send_after(self(), {:timeout, subscription_id}, state.request_timeout)
            new_timeout_timers = Map.put(state.timeout_timers, subscription_id, timer_ref)
            new_event_collectors = Map.put(state.event_collectors, subscription_id, collector_pid)

            {Map.put(state.request_queue, relay_url, rest), %{state |
              connections: new_connections,
              pending_requests: new_pending_requests,
              active_subscriptions: new_active_subscriptions,
              timeout_timers: new_timeout_timers,
              event_collectors: new_event_collectors
            }}

          {:error, _reason} ->
            # Mark connection as available again
            _new_connections = update_connection_status(new_connections, connection_pid, :available)
            {Map.put(state.request_queue, relay_url, rest), state} # Retry the next queued request
        end
    end
  end

  # Pagination helper functions
  defp find_oldest_timestamp(events) do
    events
    |> Enum.map(fn event ->
      case Map.get(event, "created_at") do
        nil -> 0
        timestamp when is_integer(timestamp) -> timestamp
        _ -> 0
      end
    end)
    |> Enum.min(fn -> 0 end)
  end

  defp start_pagination_request(subscription_id, until_timestamp, state) do
    # Get the pagination session
    case Map.get(state.pagination_sessions, subscription_id) do
      {original_from, relay_url, _all_events, _last_timestamp, request_type, request_count} ->
        # Create pagination filters with until timestamp
        pagination_filters = build_pagination_filters(request_type, until_timestamp)

        # Get connection for the relay
        case get_or_create_connection(relay_url, state) do
          {:ok, connection_pid, new_state} ->
            # Create new subscription ID for this pagination request
            new_subscription_id = UUID.uuid4()

            # Send pagination request directly with filters
            case send_queued_request(new_subscription_id, original_from, pagination_filters, connection_pid, new_state) do
              {:ok, final_state} ->
                Logger.info("Started pagination request #{request_count + 1} for subscription #{subscription_id}")
                {:noreply, final_state}
              {:error, reason} ->
                Logger.warning("Failed to start pagination request: #{inspect(reason)}")
                # Return accumulated results so far
                cleanup_and_return_results(subscription_id, [], relay_url, original_from, connection_pid, state)
            end
          {:error, reason} ->
            Logger.warning("Failed to get connection for pagination: #{inspect(reason)}")
            # Return accumulated results so far
            cleanup_and_return_results(subscription_id, [], relay_url, original_from, nil, state)
        end
      nil ->
        Logger.warning("No pagination session found for subscription #{subscription_id}")
        {:noreply, state}
    end
  end

  defp build_pagination_filters(:multiple_authors_articles, until_timestamp) do
    # Return pagination filters directly
    [
      %{
        "kinds" => [30023],  # Article kind
        "limit" => 1000,
        "until" => until_timestamp
      }
    ]
  end

  defp cleanup_and_return_results(subscription_id, events, relay_url, caller_from, connection_pid, state) do
    # Use GenServer.reply for GenServer.call responses
    GenServer.reply(caller_from, {:ok, relay_url, events})

    # Mark connection as available if we have one
    new_connections = if connection_pid do
      update_connection_status(state.connections, connection_pid, :available)
    else
      state.connections
    end

    # Remove subscription from tracking
    new_pending_requests = Map.delete(state.pending_requests, subscription_id)
    new_active_subscriptions = if connection_pid do
      remove_subscription_from_connection(state.active_subscriptions, connection_pid, subscription_id)
    else
      state.active_subscriptions
    end
    new_subscription_activity = Map.delete(state.subscription_activity, subscription_id)
    new_pagination_sessions = Map.delete(state.pagination_sessions, subscription_id)

    # Cancel timeout and cleanup
    new_timeout_timers = case Map.get(state.timeout_timers, subscription_id) do
      nil -> state.timeout_timers
      timer_ref when is_reference(timer_ref) ->
        Process.cancel_timer(timer_ref)
        Map.delete(state.timeout_timers, subscription_id)
    end

    new_event_collectors = case Map.get(state.event_collectors, subscription_id) do
      nil -> state.event_collectors
      collector_pid ->
        Process.exit(collector_pid, :kill)
        Map.delete(state.event_collectors, subscription_id)
    end

    # Process queued requests for this relay
    {new_request_queue, new_state} = process_queued_requests(relay_url, %{state |
      connections: new_connections,
      pending_requests: new_pending_requests,
      active_subscriptions: new_active_subscriptions,
      subscription_activity: new_subscription_activity,
      timeout_timers: new_timeout_timers,
      event_collectors: new_event_collectors,
      pagination_sessions: new_pagination_sessions
    })

    {:noreply, %{new_state | request_queue: new_request_queue}}
  end

  defp handle_regular_response(subscription_id, events, relay_url, caller_from, connection_pid, state) do
    # Check if we got a full batch (indicating there might be more events)
    events_count = length(events)
    Logger.info("Received #{events_count} events from #{relay_url}")

    if events_count >= 500 do
      # We got a full batch, there might be more events - start pagination
      Logger.info("Starting pagination for subscription #{subscription_id} - got #{events_count} events")

      # Find the oldest event timestamp for pagination
      oldest_timestamp = find_oldest_timestamp(events)

      # Create pagination session
      new_pagination_sessions = Map.put(state.pagination_sessions, subscription_id, {
        caller_from,
        relay_url,
        events,
        oldest_timestamp,
        :multiple_authors_articles,
        1
      })

      # Clean up current request
      new_connections = update_connection_status(state.connections, connection_pid, :available)
      new_pending_requests = Map.delete(state.pending_requests, subscription_id)
      new_active_subscriptions = remove_subscription_from_connection(state.active_subscriptions, connection_pid, subscription_id)
      new_subscription_activity = Map.delete(state.subscription_activity, subscription_id)

      # Cancel timeout and cleanup
      new_timeout_timers = case Map.get(state.timeout_timers, subscription_id) do
        nil -> state.timeout_timers
        timer_ref when is_reference(timer_ref) ->
          Process.cancel_timer(timer_ref)
          Map.delete(state.timeout_timers, subscription_id)
      end

      new_event_collectors = case Map.get(state.event_collectors, subscription_id) do
        nil -> state.event_collectors
        collector_pid ->
          Process.exit(collector_pid, :kill)
          Map.delete(state.event_collectors, subscription_id)
      end

      # Make next pagination request
      new_state = %{state |
        connections: new_connections,
        pending_requests: new_pending_requests,
        active_subscriptions: new_active_subscriptions,
        subscription_activity: new_subscription_activity,
        timeout_timers: new_timeout_timers,
        event_collectors: new_event_collectors,
        pagination_sessions: new_pagination_sessions
      }

      # Start next pagination request
      start_pagination_request(subscription_id, oldest_timestamp, new_state)

    else
      # We got fewer events than the limit, we're done
      Logger.info("Received #{events_count} events (less than limit) - pagination complete")

      # Clean up and return results
      cleanup_and_return_results(subscription_id, events, relay_url, caller_from, connection_pid, state)
    end
  end

  defp handle_pagination_response(subscription_id, events, relay_url, original_from, session_relay_url, all_events, _last_timestamp, request_type, request_count, state) do
    events_count = length(events)
    Logger.info("Pagination request #{request_count}: received #{events_count} events from #{relay_url}")

    # Get connection_pid from pending_requests
    {_caller_from, _filters, connection_pid} = Map.get(state.pending_requests, subscription_id)

    # Add new events to accumulated list
    updated_all_events = all_events ++ events

    if events_count >= 500 and request_count < state.max_pagination_requests do
      # We got a full batch and haven't hit the limit - continue pagination
      oldest_timestamp = find_oldest_timestamp(events)

      # Update pagination session
      new_pagination_sessions = Map.put(state.pagination_sessions, subscription_id, {
        original_from,
        session_relay_url,
        updated_all_events,
        oldest_timestamp,
        request_type,
        request_count + 1
      })

      # Clean up current request
      new_connections = update_connection_status(state.connections, connection_pid, :available)
      new_pending_requests = Map.delete(state.pending_requests, subscription_id)
      new_active_subscriptions = remove_subscription_from_connection(state.active_subscriptions, connection_pid, subscription_id)
      new_subscription_activity = Map.delete(state.subscription_activity, subscription_id)

      # Cancel timeout and cleanup
      new_timeout_timers = case Map.get(state.timeout_timers, subscription_id) do
        nil -> state.timeout_timers
        timer_ref when is_reference(timer_ref) ->
          Process.cancel_timer(timer_ref)
          Map.delete(state.timeout_timers, subscription_id)
      end

      new_event_collectors = case Map.get(state.event_collectors, subscription_id) do
        nil -> state.event_collectors
        collector_pid ->
          Process.exit(collector_pid, :kill)
          Map.delete(state.event_collectors, subscription_id)
      end

      # Make next pagination request
      new_state = %{state |
        connections: new_connections,
        pending_requests: new_pending_requests,
        active_subscriptions: new_active_subscriptions,
        subscription_activity: new_subscription_activity,
        timeout_timers: new_timeout_timers,
        event_collectors: new_event_collectors,
        pagination_sessions: new_pagination_sessions
      }

      # Start next pagination request
      start_pagination_request(subscription_id, oldest_timestamp, new_state)

    else
      # We got fewer events or hit the limit - pagination complete
      Logger.info("Pagination complete: total #{length(updated_all_events)} events after #{request_count} requests")

      # Clean up and return accumulated results
      cleanup_and_return_results(subscription_id, updated_all_events, session_relay_url, original_from, connection_pid, state)
    end
  end
end
