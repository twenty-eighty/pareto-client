defmodule NostrBackend.EventCollector do
  use GenServer
  require Logger

  @type t :: %{
    subscription_id: subscription_id(),
    relay_url: relay_url(),
    pool_pid: pid(),
    events: event_list(),
    timeout: timeout_ms()
  }

  @type subscription_id :: String.t()
  @type relay_url :: String.t()
  @type nostr_event :: map()
  @type event_list :: [nostr_event()]
  @type timeout_ms :: integer()

  # Client API

  @spec start_collecting(subscription_id(), relay_url(), pid(), timeout_ms()) :: {:ok, pid()} | {:error, term()}
  def start_collecting(subscription_id, relay_url, pool_pid, timeout) do
    GenServer.start_link(__MODULE__, {subscription_id, relay_url, pool_pid, timeout}, name: {:via, Registry, {NostrBackend.Registry, {:event_collector, subscription_id}}})
  end

  # Server callbacks

  @impl true
  @spec init({subscription_id(), relay_url(), pid(), timeout_ms()}) :: {:ok, t()}
  def init({subscription_id, relay_url, pool_pid, timeout}) do
    state = %{
      subscription_id: subscription_id,
      relay_url: relay_url,
      pool_pid: pool_pid,
      events: [],
      timeout: timeout
    }

    # Set up timeout
    Process.send_after(self(), :timeout, timeout)

    {:ok, state}
  end

  @impl true
  @spec handle_info({:event, subscription_id(), nostr_event()}, t()) :: {:noreply, t()}
  def handle_info({:event, subscription_id, event}, %{subscription_id: subscription_id, events: events} = state) do
    Logger.debug("EventCollector: Received event for subscription_id: #{subscription_id}")

    # Extract the event data from the tuple/list format
    event_data = case event do
      [{"EVENT", _sub_id, data}] -> data
      {"EVENT", _sub_id, data} -> data
      data when is_map(data) -> data
      _ -> event
    end

    Logger.debug("EventCollector: Extracted event data: #{inspect(Map.take(event_data, ["id", "pubkey", "kind"]))}")

    new_state = %{state | events: [event_data | events]}
    {:noreply, new_state}
  end

  @impl true
  @spec handle_info({:eose, subscription_id()}, t()) :: {:stop, :normal, t()}
  def handle_info({:eose, subscription_id}, %{subscription_id: subscription_id, events: events, relay_url: relay_url, pool_pid: pool_pid} = state) do
    Logger.info("EventCollector: Received EOSE for subscription_id: #{subscription_id}, collected #{length(events)} events")

    # Send the collected events back to the pool
    send(pool_pid, {:collect_events, subscription_id, Enum.reverse(events), relay_url})

    # Stop this process
    {:stop, :normal, state}
  end

  @impl true
  @spec handle_info({:notice, subscription_id(), term()}, t()) :: {:stop, :normal, t()}
  def handle_info({:notice, subscription_id, notice}, %{subscription_id: subscription_id, relay_url: relay_url, pool_pid: pool_pid} = state) do
    Logger.warning("EventCollector: Received notice for subscription_id: #{subscription_id}: #{inspect(notice)}")

    # Send error back to the pool
    send(pool_pid, {:collect_events, subscription_id, [], relay_url})

    # Stop this process
    {:stop, :normal, state}
  end

  @impl true
  @spec handle_info(:timeout, t()) :: {:stop, :normal, t()}
  def handle_info(:timeout, %{subscription_id: subscription_id, relay_url: relay_url, pool_pid: pool_pid} = state) do
    Logger.error("EventCollector: Timeout while collecting events for subscription_id: #{subscription_id}")

    # Send timeout error back to the pool
    send(pool_pid, {:collect_events, subscription_id, [], relay_url})

    # Stop this process
    {:stop, :normal, state}
  end

  @impl true
  @spec handle_info(term(), t()) :: {:noreply, t()}
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
