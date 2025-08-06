defmodule NostrBackend.RelayWorker do
  use WebSockex
  require Logger

  @type t :: %{
    relay_url: String.t(),
    subscription_id: String.t() | nil,
    filters: map() | nil,
    event_collector_pid: pid() | nil,
    connection_pool_pid: pid() | nil
  }

  @type relay_url :: String.t()
  @type subscription_id :: String.t()
  @type filters :: [map()] | map()
  @type opts :: Keyword.t()
  @type start_link_args :: {module(), relay_url()} | {relay_url(), opts()} | relay_url() | [relay_url: relay_url()]

  @spec start_link(start_link_args()) :: {:ok, pid()} | {:error, term()}
  def start_link(args) do
    case args do
      {NostrBackend.RelayWorker, relay_url} when is_binary(relay_url) ->
        start_websocket(relay_url, [])
      {relay_url, opts} when is_binary(relay_url) ->
        start_websocket(relay_url, opts)
      [relay_url: relay_url] when is_binary(relay_url) ->
        start_websocket(relay_url, [])
      [relay_url: relay_url, connection_pool_pid: pool_pid] when is_binary(relay_url) ->
        start_websocket(relay_url, [connection_pool_pid: pool_pid])
      relay_url when is_binary(relay_url) ->
        start_websocket(relay_url, [])
      _ ->
        {:error, :invalid_args}
    end
  end

      @spec start_websocket(relay_url(), opts()) :: {:ok, pid()} | {:error, term()}
  defp start_websocket(relay_url, opts) do
    # Get connection pool PID from opts or fallback to process dictionary
    connection_pool_pid = Keyword.get(opts, :connection_pool_pid) || Process.get(:connection_pool_pid)

    # Use longer timeout for WebSocket connection
    websocket_opts = [
      connect_timeout: 30_000,  # 30 seconds
      timeout: 60_000           # 60 seconds
    ]

    WebSockex.start_link(relay_url, __MODULE__, %{
      relay_url: relay_url,
      subscription_id: nil,
      filters: nil,
      event_collector_pid: nil,
      connection_pool_pid: connection_pool_pid
    }, websocket_opts)
  end

  @spec send_request(pid(), subscription_id(), filters()) :: :ok
  def send_request(pid, subscription_id, filters) when is_list(filters) do
    # Convert filters array to individual filter objects in the REQ message
    req_message = ["REQ", subscription_id] ++ filters
    WebSockex.send_frame(pid, {:text, Jason.encode!(req_message)})
  end

  @spec send_request(pid(), subscription_id(), map()) :: :ok
  def send_request(pid, subscription_id, filter) when is_map(filter) do
    # Single filter object
    req_message = ["REQ", subscription_id, filter]
    WebSockex.send_frame(pid, {:text, Jason.encode!(req_message)})
  end

  @spec close_subscription(pid(), subscription_id()) :: :ok
  def close_subscription(pid, subscription_id) do
    WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
  end

  @spec init(t()) :: {:ok, t()}
  def init(state) do
    # Don't connect immediately - wait for first request
    {:ok, state}
  end

    @impl WebSockex
  @spec handle_connect(WebSockex.Conn.t(), t()) :: {:ok, t()}
  def handle_connect(_conn, state) do
    Logger.info("Connected to Nostr relay #{state.relay_url}")
    {:ok, state}
  end

  @impl WebSockex
  @spec handle_frame({:text, String.t()} | {:binary, binary()}, t()) :: {:ok, t()}
  def handle_frame({:text, msg}, state) do
    case Jason.decode(msg) do
      {:ok, data} -> handle_nostr_message(data, state)
      {:error, error} ->
        Logger.warning("Failed to parse message from #{state.relay_url}: #{inspect(error)}")
        {:ok, state}
    end
  end

  @impl WebSockex
  @spec handle_frame({:text, String.t()} | {:binary, binary()}, t()) :: {:ok, t()}
  def handle_frame({:binary, msg}, state) do
    Logger.info("Received binary message from #{state.relay_url}: #{inspect(msg)}")
    {:ok, state}
  end

  @impl WebSockex
  @spec handle_frame({:ping, binary()}, t()) :: {:ok, t()}
  def handle_frame({:ping, _data}, state) do
    Logger.info("Received ping from #{state.relay_url}")
    {:ok, state}
  end

  @impl WebSockex
  @spec handle_frame({:pong, binary()}, t()) :: {:ok, t()}
  def handle_frame({:pong, _data}, state) do
    Logger.info("Received pong from #{state.relay_url}")
    {:ok, state}
  end

  @impl WebSockex
  @spec handle_disconnect(%{attempt_number: integer(), conn: WebSockex.Conn.t(), reason: term()}, t()) :: {:reconnect, t()}
  def handle_disconnect(%{reason: reason, attempt_number: attempt}, state) do
    Logger.warning("Disconnected from Nostr relay #{state.relay_url} (attempt #{attempt}, reason: #{inspect(reason)})")

    # Notify connection pool about disconnection
    if state.connection_pool_pid do
      send(state.connection_pool_pid, {:disconnect, state.relay_url, reason})
    end

    # Don't auto-reconnect - let the connection pool manage reconnections
    # This prevents endless reconnection attempts that create phantom connections
    {:ok, state}
  end

  @impl WebSockex
  @spec terminate(term(), t()) :: :ok
  def terminate(reason, state) do
    Logger.warning("WebSocket terminating for #{state.relay_url}: #{inspect(reason)}")
    :ok
  end

  @impl WebSockex
  @spec handle_info({:send_request, subscription_id(), filters()} | {:set_event_collector, pid()} | {:close_subscription, subscription_id()} | :websocket_ready, t()) :: {:ok, t()}
  def handle_info({:send_request, subscription_id, filters}, state) do
    send_request(self(), subscription_id, filters)
    {:ok, %{state | subscription_id: subscription_id, filters: filters}}
  end

  @impl WebSockex
  @spec handle_info({:set_event_collector, pid()}, t()) :: {:ok, t()}
  def handle_info({:set_event_collector, pid}, state) do
    {:ok, %{state | event_collector_pid: pid}}
  end

  @impl WebSockex
  @spec handle_info({:close_subscription, subscription_id()}, t()) :: {:ok, t()}
  def handle_info({:close_subscription, subscription_id}, %{subscription_id: subscription_id} = state) do
    Logger.info("Closing subscription #{subscription_id} on #{state.relay_url}")
    WebSockex.send_frame(self(), {:text, Jason.encode!(["CLOSE", subscription_id])})
    {:ok, %{state | subscription_id: nil, filters: nil}}
  end

  @impl WebSockex
  @spec handle_info(:websocket_ready, t()) :: {:ok, t()}
  def handle_info(:websocket_ready, state) do
    Logger.info("WebSocket ready for #{state.relay_url}")
    {:ok, state}
  end

  @impl WebSockex
  @spec handle_info(term(), t()) :: {:ok, t()}
  def handle_info(msg, state) do
    Logger.warning("Unhandled message in RelayWorker: #{inspect(msg)}")
    {:ok, state}
  end

  @spec handle_nostr_message([String.t()], t()) :: {:ok, t()}
  defp handle_nostr_message(["EVENT", subscription_id, event], state) do
    Logger.debug("Received EVENT with subscription_id: #{subscription_id}")

    # Forward to EventCollector if available
    if state.event_collector_pid do
      Logger.debug("Forwarding EVENT to EventCollector #{inspect(state.event_collector_pid)}")
      send(state.event_collector_pid, {:event, subscription_id, event})
    else
      Logger.warning("No EventCollector PID available for subscription #{subscription_id}")
    end

    {:ok, state}
  end

  @spec handle_nostr_message([String.t()], t()) :: {:ok, t()}
  defp handle_nostr_message(["EOSE", subscription_id], state) do
    Logger.debug("Received EOSE for subscription_id: #{subscription_id}")

    # Forward to EventCollector if available
    if state.event_collector_pid do
      Logger.debug("Forwarding EOSE to EventCollector #{inspect(state.event_collector_pid)}")
      send(state.event_collector_pid, {:eose, subscription_id})
    else
      Logger.warning("No EventCollector PID available for subscription #{subscription_id}")
    end

    {:ok, state}
  end

  @spec handle_nostr_message([String.t()], t()) :: {:ok, t()}
  defp handle_nostr_message(["NOTICE", message], state) do
    Logger.warning("Relay notice from #{state.relay_url}: #{message}")

    # If it's a "too many concurrent REQs" error, request debugging info from connection pool
    if String.contains?(message, "too many concurrent REQs") and state.connection_pool_pid do
      send(state.connection_pool_pid, {:debug_active_subscriptions, state.relay_url})
    end

    # Forward to EventCollector if available
    if state.event_collector_pid do
      send(state.event_collector_pid, {:notice, state.subscription_id, message})
    end
    {:ok, state}
  end

  @spec handle_nostr_message([String.t()], t()) :: {:ok, t()}
  defp handle_nostr_message(["OK", event_id, success, message], state) do
    if success do
      Logger.debug("Event #{event_id} accepted by #{state.relay_url}")
    else
      Logger.warning("Event #{event_id} rejected by #{state.relay_url}: #{message}")
    end
    {:ok, state}
  end

  @spec handle_nostr_message(term(), t()) :: {:ok, t()}
  defp handle_nostr_message(unknown_message, state) do
    Logger.debug("Received unknown message from #{state.relay_url}: #{inspect(unknown_message)}")
    {:ok, state}
  end
end
