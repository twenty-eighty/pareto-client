defmodule NostrBackend.RelayConnectionPoolV2 do
  use GenServer
  require Logger

  @type t :: %{
    pools: %{String.t() => pid()},
    pending_requests: %{String.t() => {pid(), map(), String.t()}},
    timeout_timers: %{String.t() => reference()},
    event_collectors: %{String.t() => pid()},
    pagination_sessions: %{String.t() => {pid(), String.t(), [map()], integer(), atom(), integer()}},
    request_timeout: integer(),
    max_pagination_requests: integer(),
    relay_failures: %{String.t() => {integer(), integer()}},  # {failure_count, last_failure_time}
    active_workers: %{String.t() => pid()},  # {relay_url => worker_pid}
    # Rate limiting
    request_queues: %{String.t() => :queue.queue()},  # {relay_url => queue of {subscription_id, filters, collector_pid, from}}
    active_requests: %{String.t() => integer()},  # {relay_url => count}
    last_request_time: %{String.t() => integer()},  # {relay_url => timestamp}
    max_concurrent_requests: integer(),
    min_request_interval: integer(),  # milliseconds between requests
    # Track active subscriptions per relay (to send CLOSE properly)
    active_subscriptions: %{String.t() => [String.t()]}, # {relay_url => [subscription_ids]}
    # Conservative subscription limit per relay
    max_subscriptions_per_relay: pos_integer()
  }

  @type relay_url :: String.t()
  @type relay_urls :: [relay_url()]
  @type pubkey :: String.t()
  @type pubkeys :: [pubkey()]
  @type subscription_id :: String.t()
  @type request_type :: :multiple_authors_articles | :follow_list | :profile
  @type filters :: [map()]
  @type events :: [map()]
  @type opts :: Keyword.t()

  @spec start_link(opts()) :: {:ok, pid()}
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec fetch_from_relays(relay_urls(), pubkeys() | pubkey(), request_type()) :: {:ok, relay_url(), events()} | {:error, String.t()}
  def fetch_from_relays(relay_urls, pubkeys, request_type) do
    GenServer.call(__MODULE__, {:fetch_from_relays, relay_urls, pubkeys, request_type}, :infinity)
  end

  @impl GenServer
  @spec init(opts()) :: {:ok, t()}
  def init(_opts) do
    # Trap exits so worker crashes don't crash the pool
    Process.flag(:trap_exit, true)

    state = %{
      pools: %{},
      pending_requests: %{},
      timeout_timers: %{},
      event_collectors: %{},
      pagination_sessions: %{},
      request_timeout: 30_000,
      max_pagination_requests: 10,
      relay_failures: %{},
      active_workers: %{},
      # Rate limiting initialization
      request_queues: %{},
      active_requests: %{},
      last_request_time: %{},
      max_concurrent_requests: 2,  # Max 2 concurrent requests per relay
      min_request_interval: 1000,  # Min 1 second between requests
      # Track active subscriptions
      active_subscriptions: %{},  # {relay_url => [subscription_ids]}
      # Conservative subscription limit - most relays allow only 1-2 concurrent subscriptions
      max_subscriptions_per_relay: 1
    }

    {:ok, state}
  end

  @impl GenServer
  @spec handle_call({:fetch_from_relays, relay_urls(), pubkeys() | pubkey(), request_type()}, {pid(), reference()}, t()) :: {:reply, {:error, String.t()}, t()} | {:noreply, t()}
  def handle_call({:fetch_from_relays, relay_urls, pubkeys, request_type}, from, state) do
    # Try each relay until one works
    case try_relays(relay_urls, pubkeys, request_type, from, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl GenServer
  @spec handle_info({:disconnect, relay_url(), term()} | {:collect_events, subscription_id(), events(), relay_url()} | {:timeout, subscription_id()}, t()) :: {:noreply, t()}
  def handle_info({:disconnect, relay_url, reason}, state) do
    Logger.warning("Relay connection pool: Disconnected from #{relay_url} (reason: #{inspect(reason)})")

    # Clean up the dead worker from active_workers so we don't try to reuse it
    new_state = %{state | active_workers: Map.delete(state.active_workers, relay_url)}

    {:noreply, new_state}
  end

  @impl GenServer
  @spec handle_info({:disconnect, relay_url(), term()} | {:collect_events, subscription_id(), events(), relay_url()} | {:timeout, subscription_id()} | {:process_queue, relay_url()} | {:decrement_active_request, relay_url()} | {:EXIT, pid(), term()} | {:debug_active_subscriptions, relay_url()}, t()) :: {:noreply, t()}
  def handle_info({:process_queue, relay_url}, state) do
    new_state = process_request_queue(relay_url, state)
    {:noreply, new_state}
  end

    def handle_info({:decrement_active_request, relay_url}, state) do
    current_count = Map.get(state.active_requests, relay_url, 0)

    # Only decrement if we actually have active requests
    if current_count > 0 do
      new_count = current_count - 1
      new_state = %{state |
        active_requests: Map.put(state.active_requests, relay_url, new_count)
      }

      Logger.info("Decremented active requests for #{relay_url} (early): #{current_count} -> #{new_count}")

      # Process any queued requests now that we have capacity
      final_state = process_request_queue(relay_url, new_state)
      {:noreply, final_state}
    else
      Logger.debug("Skipping early decrement for #{relay_url} - counter already at 0")
      {:noreply, state}
    end
  end

  def handle_info({:EXIT, worker_pid, reason}, state) do
    Logger.warning("RelayWorker #{inspect(worker_pid)} exited with reason: #{inspect(reason)}")

    # Find which relay this worker was associated with and clean it up
    relay_url = Enum.find_value(state.active_workers, fn {url, pid} ->
      if pid == worker_pid, do: url, else: nil
    end)

    if relay_url != nil do
      Logger.info("Cleaning up crashed worker for relay #{relay_url}")
      new_state = %{state | active_workers: Map.delete(state.active_workers, relay_url)}
      {:noreply, new_state}
    else
      # Worker not in our active_workers map, just ignore
      {:noreply, state}
    end
  end

  def handle_info({:debug_active_subscriptions, relay_url}, state) do
    active_subs = Map.get(state.active_subscriptions, relay_url, [])
    active_requests = Map.get(state.active_requests, relay_url, 0)
    worker_pid = Map.get(state.active_workers, relay_url, nil)

    Logger.error("🔍 DEBUG: 'too many concurrent REQs' on #{relay_url}")
    Logger.error("   Active subscriptions (#{length(active_subs)}): #{inspect(active_subs)}")
    Logger.error("   Active requests: #{active_requests}")
    Logger.error("   Worker PID: #{inspect(worker_pid)}")
    Logger.error("   Max allowed: #{state.max_subscriptions_per_relay}")

    {:noreply, state}
  end

  def handle_info({:collect_events, subscription_id, events, relay_url}, state) do
    case Map.get(state.pending_requests, subscription_id) do
      {caller_from, _filters, _relay_url} ->
        # Check if this is a pagination session (original subscription ID)
        case Map.get(state.pagination_sessions, subscription_id) do
          {original_from, session_relay_url, all_events, last_timestamp, request_type, request_count} ->
            # This is a pagination request continuation
            handle_pagination_response(subscription_id, events, relay_url, original_from, session_relay_url, all_events, last_timestamp, request_type, request_count, state)
          {original_subscription_id, :pagination} ->
            # This is a new pagination request - lookup the original session
            case Map.get(state.pagination_sessions, original_subscription_id) do
              {original_from, session_relay_url, all_events, last_timestamp, request_type, request_count} ->
                handle_pagination_response(original_subscription_id, events, relay_url, original_from, session_relay_url, all_events, last_timestamp, request_type, request_count, state)
              _ ->
                Logger.warning("Pagination request #{subscription_id} references unknown original subscription #{original_subscription_id}")
                {:noreply, state}
            end
          nil ->
            # This is a regular request - check if we need pagination
            handle_collect_events(subscription_id, events, relay_url, caller_from, state)
        end
      nil ->
        Logger.warning("Received collect_events for unknown subscription: #{subscription_id}")
        {:noreply, state}
    end
  end

  @impl GenServer
  @spec handle_info({:disconnect, relay_url(), term()} | {:collect_events, subscription_id(), events(), relay_url()} | {:timeout, subscription_id()}, t()) :: {:noreply, t()}
  def handle_info({:timeout, subscription_id}, state) do
    case Map.get(state.pending_requests, subscription_id) do
      {caller_from, _filters, relay_url} ->
        Logger.warning("Request timeout for subscription #{subscription_id} on #{relay_url}")
        GenServer.reply(caller_from, {:error, "Request timeout"})
        new_state = cleanup_subscription(subscription_id, state)
        {:noreply, new_state}
      nil ->
        Logger.warning("Timeout for unknown subscription: #{subscription_id}")
        {:noreply, state}
    end
  end

  @spec try_relays(relay_urls(), pubkeys() | pubkey(), request_type(), {pid(), reference()}, t()) :: {:ok, t()} | {:error, String.t()}
  defp try_relays(relay_urls, pubkeys, request_type, from, state) do
    # Filter out relays that have failed too many times recently
    available_relays = filter_available_relays(relay_urls, state)

    if available_relays == [] do
      Logger.warning("All relays are temporarily unavailable due to recent failures")
      GenServer.reply(from, {:error, "All relays temporarily unavailable"})
      {:error, "All relays temporarily unavailable"}
    else
      try_relays_internal(available_relays, pubkeys, request_type, from, state)
    end
  end

  @spec try_relays_internal(relay_urls(), pubkeys() | pubkey(), request_type(), {pid(), reference()}, t()) :: {:ok, t()} | {:error, String.t()}
  defp try_relays_internal([relay_url | rest], pubkeys, request_type, from, state) do
    case get_or_create_pool(relay_url, state) do
      {:ok, pool_pid, new_state} ->
        case send_request_via_pool(pool_pid, relay_url, pubkeys, request_type, from, new_state) do
          {:ok, final_state} -> {:ok, final_state}
          {:error, reason} ->
            Logger.warning("Failed to send request to #{relay_url}: #{inspect(reason)}")
            # Record the failure
            new_state_with_failure = record_relay_failure(relay_url, new_state)
            if rest == [] do
              # No more relays to try
              GenServer.reply(from, {:error, "All relays failed"})
              {:error, "All relays failed"}
            else
              # Try next relay
              try_relays_internal(rest, pubkeys, request_type, from, new_state_with_failure)
            end
        end
      {:error, reason} ->
        Logger.warning("Failed to get connection for relay #{relay_url}: #{inspect(reason)}")
        # Record the failure
        new_state_with_failure = record_relay_failure(relay_url, state)
        if rest == [] do
          # No more relays to try
          GenServer.reply(from, {:error, "All relays failed"})
          {:error, "All relays failed"}
        else
          # Try next relay
          try_relays_internal(rest, pubkeys, request_type, from, new_state_with_failure)
        end
    end
  end

  @spec try_relays_internal([], pubkeys() | pubkey(), request_type(), {pid(), reference()}, t()) :: {:error, String.t()}
  defp try_relays_internal([], _pubkeys, _request_type, from, _state) do
    GenServer.reply(from, {:error, "All relays failed"})
    {:error, "All relays failed"}
  end

  @spec filter_available_relays(relay_urls(), t()) :: relay_urls()
  defp filter_available_relays(relay_urls, state) do
    now = System.system_time(:second)
    max_failures = 3
    failure_window = 60  # 1 minute

    relay_urls
    |> Enum.filter(fn relay_url ->
      case Map.get(state.relay_failures, relay_url) do
        {failure_count, last_failure_time} ->
          if now - last_failure_time > failure_window do
            # Reset failure count if enough time has passed
            true
          else
            # Check if we haven't exceeded max failures
            failure_count < max_failures
          end
        nil ->
          # No failures recorded, relay is available
          true
      end
    end)
  end

  @spec record_relay_failure(relay_url(), t()) :: t()
  defp record_relay_failure(relay_url, state) do
    now = System.system_time(:second)
    {current_failures, _last_failure} = Map.get(state.relay_failures, relay_url, {0, 0})

    new_failures = Map.put(state.relay_failures, relay_url, {current_failures + 1, now})
    %{state | relay_failures: new_failures}
  end

  @spec get_or_create_pool(relay_url(), t()) :: {:ok, pid(), t()} | {:error, term()}
  defp get_or_create_pool(relay_url, state) do
    case Map.get(state.pools, relay_url) do
      nil ->
        case create_pool(relay_url) do
          {:ok, pool_pid} ->
            new_state = %{state | pools: Map.put(state.pools, relay_url, pool_pid)}
            {:ok, pool_pid, new_state}
          {:error, reason} ->
            {:error, reason}
        end
      pool_pid ->
        {:ok, pool_pid, state}
    end
  end

  @spec create_pool(relay_url()) :: {:ok, pid()} | {:error, term()}
  defp create_pool(relay_url) do
    pool_config = [
      name: {:local, String.to_atom("pool_#{relay_url |> String.replace("://", "_") |> String.replace("/", "_") |> String.replace(".", "_")}")},
      worker_module: NostrBackend.RelayWorker,
      size: 0,  # Start with 0 workers
      max_overflow: 5,  # Allow up to 5 workers when needed
      strategy: :lifo
    ]

    # Pass both relay_url and connection_pool_pid to the worker
    worker_spec = [relay_url: relay_url, connection_pool_pid: self()]

    case :poolboy.start_link(pool_config, worker_spec) do
      {:ok, pool_pid} ->
        # Don't pre-populate workers - let them be created on demand
        {:ok, pool_pid}
      {:error, reason} -> {:error, reason}
    end
  end

    @spec send_request_via_pool(pid(), relay_url(), pubkeys() | pubkey(), request_type(), {pid(), reference()}, t()) :: {:ok, t()} | {:error, term()}
  defp send_request_via_pool(_pool_pid, relay_url, pubkeys, request_type, from, state) do
    subscription_id = UUID.uuid4()
    filters = build_filters(pubkeys, request_type)

    # Start EventCollector
    {:ok, collector_pid} = NostrBackend.EventCollector.start_collecting(
      subscription_id,
      relay_url,
      self(),
      state.request_timeout
    )

    # Set up timeout
    timer_ref = Process.send_after(self(), {:timeout, subscription_id}, state.request_timeout)

    # Store request info
    new_state = %{state |
      pending_requests: Map.put(state.pending_requests, subscription_id, {from, filters, relay_url}),
      timeout_timers: Map.put(state.timeout_timers, subscription_id, timer_ref),
      event_collectors: Map.put(state.event_collectors, subscription_id, collector_pid)
    }

    # Use rate limiting and queuing
    case can_send_request_immediately?(relay_url, new_state) do
      true ->
        send_request_immediately(relay_url, subscription_id, filters, collector_pid, new_state)
      false ->
        queue_request(relay_url, subscription_id, filters, collector_pid, from, new_state)
    end
  end

  @spec handle_collect_events(subscription_id(), events(), relay_url(), {pid(), reference()}, t()) :: {:noreply, t()}
  defp handle_collect_events(subscription_id, events, relay_url, caller_from, state) do
    events_count = length(events)
    Logger.info("Received #{events_count} events from #{relay_url}")

    if events_count >= 500 do
      # Start pagination
      handle_pagination_start(subscription_id, events, relay_url, caller_from, state)
    else
      # Complete request
      GenServer.reply(caller_from, {:ok, relay_url, events})
      new_state = cleanup_subscription(subscription_id, state)
      {:noreply, new_state}
    end
  end

  @spec handle_pagination_start(subscription_id(), events(), relay_url(), {pid(), reference()}, t()) :: {:noreply, t()}
  defp handle_pagination_start(subscription_id, events, relay_url, caller_from, state) do
    Logger.info("Starting pagination for subscription #{subscription_id} - got #{length(events)} events")

    oldest_timestamp = find_oldest_timestamp(events)

    new_pagination_sessions = Map.put(state.pagination_sessions, subscription_id, {
      caller_from,
      relay_url,
      events,
      oldest_timestamp,
      :multiple_authors_articles,
      1
    })

    new_state = %{state | pagination_sessions: new_pagination_sessions}

    # Start next pagination request
    start_pagination_request(subscription_id, oldest_timestamp, new_state)
  end

  @spec start_pagination_request(subscription_id(), integer(), t()) :: {:noreply, t()}
  defp start_pagination_request(subscription_id, until_timestamp, state) do
    case Map.get(state.pagination_sessions, subscription_id) do
      {original_from, relay_url, all_events, _last_timestamp, request_type, request_count} ->
        pagination_filters = build_pagination_filters(request_type, until_timestamp)

        # Reuse existing WebSocket connection for pagination
        case Map.get(state.active_workers, relay_url) do
          worker_pid when is_pid(worker_pid) ->
            new_subscription_id = UUID.uuid4()

            # Start EventCollector for pagination
            {:ok, collector_pid} = NostrBackend.EventCollector.start_collecting(
              new_subscription_id,
              relay_url,
              self(),
              state.request_timeout
            )

            # Set up timeout
            timer_ref = Process.send_after(self(), {:timeout, new_subscription_id}, state.request_timeout)

            # Store mapping from new subscription ID to original subscription ID for pagination
            new_pagination_mapping = Map.put(state.pagination_sessions, new_subscription_id, {subscription_id, :pagination})

            # Store request info for the new pagination subscription
            updated_state = %{state |
              pending_requests: Map.put(state.pending_requests, new_subscription_id, {original_from, pagination_filters, relay_url}),
              timeout_timers: Map.put(state.timeout_timers, new_subscription_id, timer_ref),
              event_collectors: Map.put(state.event_collectors, new_subscription_id, collector_pid),
              pagination_sessions: Map.merge(state.pagination_sessions, new_pagination_mapping)
            }

            # Send request on existing connection - check if worker is still alive
            if Process.alive?(worker_pid) do
              try do
                NostrBackend.RelayWorker.send_request(worker_pid, new_subscription_id, pagination_filters)
                send(worker_pid, {:set_event_collector, collector_pid})
                Logger.info("Started pagination request #{request_count + 1} for subscription #{subscription_id} (reusing connection)")
                {:noreply, updated_state}
              catch
                :exit, reason ->
                  Logger.warning("Failed to send pagination request to #{relay_url}: #{inspect(reason)}")
                  # Clean up the dead worker and return current results
                  new_active_workers = Map.delete(state.active_workers, relay_url)
                  cleaned_state = %{updated_state | active_workers: new_active_workers}
                  cleanup_and_return_results(subscription_id, all_events, relay_url, original_from, cleaned_state)
              end
            else
              Logger.warning("Worker for #{relay_url} is no longer alive, stopping pagination")
              # Clean up the dead worker and return current results
              new_active_workers = Map.delete(state.active_workers, relay_url)
              cleaned_state = %{updated_state | active_workers: new_active_workers}
              cleanup_and_return_results(subscription_id, all_events, relay_url, original_from, cleaned_state)
            end

          nil ->
            Logger.warning("No active worker found for relay #{relay_url}")
            cleanup_and_return_results(subscription_id, [], relay_url, original_from, state)
        end
      nil ->
        Logger.warning("No pagination session found for subscription #{subscription_id}")
        {:noreply, state}
    end
  end



  @spec handle_pagination_response(subscription_id(), events(), relay_url(), {pid(), reference()}, relay_url(), events(), integer(), request_type(), integer(), t()) :: {:noreply, t()}
  defp handle_pagination_response(subscription_id, events, relay_url, original_from, session_relay_url, all_events, _last_timestamp, request_type, request_count, state) do
    Logger.info("Received pagination response for subscription #{subscription_id} from #{relay_url}")

    # Append new events to the existing list
    updated_all_events = all_events ++ events

    # If we received fewer events than expected, it means we've reached the end
    if length(events) < 1000 do
      Logger.info("Received fewer events than expected for pagination. Finishing pagination for subscription #{subscription_id}")
      GenServer.reply(original_from, {:ok, session_relay_url, updated_all_events})
      new_state = cleanup_subscription(subscription_id, state)
      {:noreply, new_state}
    else
      # Continue pagination
      Logger.info("Received 1000+ events for pagination. Starting next request for subscription #{subscription_id}")
      new_last_timestamp = find_oldest_timestamp(events)
      new_request_count = request_count + 1

      new_pagination_sessions = Map.put(state.pagination_sessions, subscription_id, {
        original_from,
        session_relay_url,
        updated_all_events,
        new_last_timestamp,
        request_type,
        new_request_count
      })

      new_state = %{state | pagination_sessions: new_pagination_sessions}

      start_pagination_request(subscription_id, new_last_timestamp, new_state)
    end
  end

  # Rate limiting functions
  @spec can_send_request_immediately?(relay_url(), t()) :: boolean()
  defp can_send_request_immediately?(relay_url, state) do
    current_requests = Map.get(state.active_requests, relay_url, 0)
    last_request = Map.get(state.last_request_time, relay_url, 0)
    current_time = System.system_time(:millisecond)

    current_requests < state.max_concurrent_requests and
    (current_time - last_request) >= state.min_request_interval
  end

  @spec send_request_immediately(relay_url(), subscription_id(), filters(), pid(), t()) :: {:ok, t()} | {:error, term()}
  defp send_request_immediately(relay_url, subscription_id, filters, collector_pid, state) do
    current_time = System.system_time(:millisecond)

    # Update rate limiting state
    new_state = %{state |
      active_requests: Map.update(state.active_requests, relay_url, 1, &(&1 + 1)),
      last_request_time: Map.put(state.last_request_time, relay_url, current_time)
    }

    Logger.info("Sending request immediately to #{relay_url} (#{Map.get(new_state.active_requests, relay_url)} active)")

    # Use direct WebSocket connection (reuse existing or create new)
    case Map.get(new_state.active_workers, relay_url) do
      worker_pid when is_pid(worker_pid) ->
        # Enforce subscription limit before sending new request
        state_after_limit = enforce_subscription_limit(relay_url, worker_pid, new_state)

        # Reuse existing connection
        Logger.info("Reusing existing WebSocket connection for #{relay_url}")

        # Check if worker is still alive before sending
        result = if Process.alive?(worker_pid) do
          try do
            NostrBackend.RelayWorker.send_request(worker_pid, subscription_id, filters)
            send(worker_pid, {:set_event_collector, collector_pid})
            :ok
          catch
            :exit, reason ->
              Logger.warning("Failed to send request to #{relay_url}: #{inspect(reason)}")
              {:error, "Connection failed"}
          end
        else
          Logger.warning("Worker for #{relay_url} is no longer alive")
          {:error, "Connection lost"}
        end

        case result do
          :ok ->
            # Track this new subscription
            final_state = track_active_subscription(relay_url, subscription_id, state_after_limit)

            # Decrement the active request counter after a longer delay (since relay is very sensitive)
            # This allows new requests to be processed while waiting for EOSE
            Process.send_after(self(), {:decrement_active_request, relay_url}, 3000)

            {:ok, final_state}
          {:error, reason} ->
            # Clean up dead worker
            new_active_workers = Map.delete(state_after_limit.active_workers, relay_url)
            _cleaned_state = %{state_after_limit | active_workers: new_active_workers}
            {:error, reason}
        end

      nil ->
        # Create new connection with retry
        case create_websocket_connection_with_retry(relay_url, subscription_id, filters, collector_pid, new_state, 3) do
          {:ok, intermediate_state} ->
            final_state = track_active_subscription(relay_url, subscription_id, intermediate_state)
            {:ok, final_state}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  # Helper functions for subscription tracking
  @spec enforce_subscription_limit(relay_url(), pid(), t()) :: t()
  defp enforce_subscription_limit(relay_url, worker_pid, state) do
    active_subs = Map.get(state.active_subscriptions, relay_url, [])
    current_count = length(active_subs)

    if current_count >= state.max_subscriptions_per_relay do
      # Close oldest subscription to make room for new one
      {oldest_sub, remaining_subs} = case active_subs do
        [oldest | rest] -> {oldest, rest}
        [] -> {nil, []}
      end

      if oldest_sub do
        Logger.info("Closing oldest subscription #{oldest_sub} on #{relay_url} to enforce limit (#{current_count}/#{state.max_subscriptions_per_relay})")
        NostrBackend.RelayWorker.close_subscription(worker_pid, oldest_sub)
        %{state | active_subscriptions: Map.put(state.active_subscriptions, relay_url, remaining_subs)}
      else
        state
      end
    else
      state
    end
  end

  @spec track_active_subscription(relay_url(), subscription_id(), t()) :: t()
  defp track_active_subscription(relay_url, subscription_id, state) do
    current_subs = Map.get(state.active_subscriptions, relay_url, [])
    new_subs = [subscription_id | current_subs]
    %{state | active_subscriptions: Map.put(state.active_subscriptions, relay_url, new_subs)}
  end

  @spec queue_request(relay_url(), subscription_id(), filters(), pid(), {pid(), reference()}, t()) :: {:ok, t()}
  defp queue_request(relay_url, subscription_id, filters, collector_pid, from, state) do
    queue = Map.get(state.request_queues, relay_url, :queue.new())
    new_queue = :queue.in({subscription_id, filters, collector_pid, from}, queue)

    new_state = %{state |
      request_queues: Map.put(state.request_queues, relay_url, new_queue)
    }

    Logger.info("Queued request for #{relay_url} (queue size: #{:queue.len(new_queue)})")

    # Schedule processing of the queue
    Process.send_after(self(), {:process_queue, relay_url}, state.min_request_interval)

    {:ok, new_state}
  end

  @spec process_request_queue(relay_url(), t()) :: t()
  defp process_request_queue(relay_url, state) do
    queue = Map.get(state.request_queues, relay_url, :queue.new())

    case :queue.out(queue) do
      {{:value, {subscription_id, filters, collector_pid, _from}}, new_queue} ->
        if can_send_request_immediately?(relay_url, state) do
          # Send the request
          case send_request_immediately(relay_url, subscription_id, filters, collector_pid, state) do
            {:ok, updated_state} ->
              final_state = %{updated_state |
                request_queues: Map.put(updated_state.request_queues, relay_url, new_queue)
              }

              # Schedule next queue processing if there are more items
              if not :queue.is_empty(new_queue) do
                Process.send_after(self(), {:process_queue, relay_url}, state.min_request_interval)
              end

              final_state
            {:error, _reason} ->
              # Keep the request in queue on error
              state
          end
        else
          # Can't send yet, reschedule
          Process.send_after(self(), {:process_queue, relay_url}, state.min_request_interval)
          state
        end
      {:empty, _} ->
        # Queue is empty
        state
    end
  end

  @spec create_websocket_connection_with_retry(relay_url(), subscription_id(), filters(), pid(), t(), integer()) :: {:ok, t()} | {:error, term()}
  defp create_websocket_connection_with_retry(relay_url, subscription_id, filters, collector_pid, state, retries_left) when retries_left > 0 do
    case NostrBackend.RelayWorker.start_link([relay_url: relay_url, connection_pool_pid: self()]) do
      {:ok, worker_pid} ->
        Logger.info("Successfully connected to #{relay_url}")

        # Link to the worker so we get EXIT messages when it crashes
        Process.link(worker_pid)

        # Wait a moment for connection to be ready
        :timer.sleep(1000)

        # Send request directly - check if worker is still alive
        if Process.alive?(worker_pid) do
          try do
            NostrBackend.RelayWorker.send_request(worker_pid, subscription_id, filters)
            send(worker_pid, {:set_event_collector, collector_pid})
          catch
            :exit, reason ->
              Logger.warning("Failed to send request to newly created worker #{relay_url}: #{inspect(reason)}")
              {:error, "Failed to send request: #{inspect(reason)}"}
          end
        else
          Logger.warning("Newly created worker for #{relay_url} died immediately")
          {:error, "Worker died immediately"}
        end

        # Track the worker for reuse
        final_state = %{state | active_workers: Map.put(state.active_workers, relay_url, worker_pid)}

        # Decrement the active request counter after a longer delay (since relay is very sensitive)
        Process.send_after(self(), {:decrement_active_request, relay_url}, 3000)

        {:ok, final_state}

      {:error, %WebSockex.ConnError{original: :timeout}} ->
        Logger.warning("Connection timeout for #{relay_url}, retrying... (#{retries_left - 1} attempts left)")
        :timer.sleep(2000)  # Wait 2 seconds before retry
        create_websocket_connection_with_retry(relay_url, subscription_id, filters, collector_pid, state, retries_left - 1)

      {:error, reason} ->
        Logger.error("Failed to create WebSocket connection to #{relay_url}: #{inspect(reason)}")
        if retries_left > 1 do
          Logger.info("Retrying connection to #{relay_url}... (#{retries_left - 1} attempts left)")
          :timer.sleep(2000)
          create_websocket_connection_with_retry(relay_url, subscription_id, filters, collector_pid, state, retries_left - 1)
        else
          cleanup_subscription(subscription_id, state)
          {:error, reason}
        end
    end
  end

  defp create_websocket_connection_with_retry(relay_url, subscription_id, _filters, _collector_pid, state, 0) do
    Logger.error("All retry attempts failed for #{relay_url}")
    cleanup_subscription(subscription_id, state)
    {:error, :max_retries_exceeded}
  end

  @spec cleanup_subscription(subscription_id(), t()) :: t()
  defp cleanup_subscription(subscription_id, state) do
    # Get the relay URL to decrement active request count
    relay_url = case Map.get(state.pending_requests, subscription_id) do
      {_from, _filters, url} -> url
      _ -> nil
    end

    # Cancel timeout timer
    case Map.get(state.timeout_timers, subscription_id) do
      nil -> :ok
      timer_ref when is_reference(timer_ref) ->
        Process.cancel_timer(timer_ref)
        :ok
    end

    # Stop EventCollector
    case Map.get(state.event_collectors, subscription_id) do
      nil -> :ok
      collector_pid ->
        Process.exit(collector_pid, :kill)
        :ok
    end

    # Decrement active request count for the relay
    new_active_requests = if relay_url != nil do
      current_count = Map.get(state.active_requests, relay_url, 0)
      new_count = max(0, current_count - 1)
      Logger.info("Decremented active requests for #{relay_url}: #{current_count} -> #{new_count}")
      Map.put(state.active_requests, relay_url, new_count)
    else
      state.active_requests
    end

    # Send CLOSE message to relay before removing from tracking
    # BUT skip if this subscription is being paginated
    pagination_in_progress = Map.has_key?(state.pagination_sessions, subscription_id)

    if relay_url != nil and not pagination_in_progress do
      case Map.get(state.active_workers, relay_url) do
        worker_pid when is_pid(worker_pid) ->
          Logger.debug("Sending CLOSE message for subscription #{subscription_id} to #{relay_url}")
          NostrBackend.RelayWorker.close_subscription(worker_pid, subscription_id)
        _ ->
          Logger.debug("No active worker found for #{relay_url}, skipping CLOSE message")
      end
    else
      if pagination_in_progress do
        Logger.debug("Skipping CLOSE message for subscription #{subscription_id} - pagination in progress")
      end
    end

    # Remove subscription from active subscriptions tracking
    new_active_subscriptions = if relay_url != nil do
      current_subs = Map.get(state.active_subscriptions, relay_url, [])
      updated_subs = List.delete(current_subs, subscription_id)
      Map.put(state.active_subscriptions, relay_url, updated_subs)
    else
      state.active_subscriptions
    end

    new_state = %{state |
      pending_requests: Map.delete(state.pending_requests, subscription_id),
      timeout_timers: Map.delete(state.timeout_timers, subscription_id),
      event_collectors: Map.delete(state.event_collectors, subscription_id),
      pagination_sessions: Map.delete(state.pagination_sessions, subscription_id),
      active_requests: new_active_requests,
      active_subscriptions: new_active_subscriptions
    }
    # Note: We keep active_workers for reuse across requests

    # Try to process queue for this relay if we freed up a slot
    if relay_url != nil do
      Process.send_after(self(), {:process_queue, relay_url}, 100)
    end

    new_state
  end

  @spec cleanup_and_return_results(subscription_id(), events(), relay_url(), {pid(), reference()}, t()) :: {:noreply, t()}
  defp cleanup_and_return_results(subscription_id, events, relay_url, caller_from, state) do
    GenServer.reply(caller_from, {:ok, relay_url, events})
    new_state = cleanup_subscription(subscription_id, state)
    {:noreply, new_state}
  end

  @spec find_oldest_timestamp(events()) :: integer()
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

  @spec build_filters(pubkeys(), :multiple_authors_articles) :: filters()
  defp build_filters(pubkeys, :multiple_authors_articles) do
    Logger.info("Building filters for multiple authors articles: #{length(pubkeys)} authors")
    [
      %{
        "authors" => pubkeys,
        "kinds" => [30023],  # Article kind
        "limit" => 1000
      }
    ]
  end

  @spec build_filters(pubkey(), :follow_list) :: filters()
  defp build_filters(pubkey, :follow_list) do
    Logger.info("Building filters for follow list: #{pubkey}")
    [
      %{
        "authors" => [pubkey],
        "kinds" => [3],  # Follow list kind
        "limit" => 1000
      }
    ]
  end

  @spec build_filters(pubkey(), :profile) :: filters()
  defp build_filters(pubkey, :profile) do
    Logger.info("Building filters for profile: #{pubkey}")
    [
      %{
        "authors" => [pubkey],
        "kinds" => [0],  # Profile kind
        "limit" => 1000
      }
    ]
  end

  @spec build_filters(%{author: String.t(), identifier: String.t(), kind: integer()}, :address) :: filters()
  defp build_filters(%{author: author, identifier: identifier, kind: kind}, :address) do
    Logger.info("Building filters for address: #{author}/#{identifier} (kind #{kind})")
    [
      %{
        "authors" => [author],
        "kinds" => [kind],
        "#d" => [identifier],
        "limit" => 1000
      }
    ]
  end

  @spec build_pagination_filters(:multiple_authors_articles, integer()) :: filters()
  defp build_pagination_filters(:multiple_authors_articles, until_timestamp) do
    [
      %{
        "kinds" => [30023],  # Article kind
        "limit" => 1000,
        "until" => until_timestamp
      }
    ]
  end

  @spec build_pagination_filters(:follow_list, integer()) :: filters()
  defp build_pagination_filters(:follow_list, until_timestamp) do
    [
      %{
        "kinds" => [3],  # Follow list kind
        "limit" => 1000,
        "until" => until_timestamp
      }
    ]
  end
end
