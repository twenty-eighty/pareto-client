defmodule NostrBackend.RelayConnection do
  use WebSockex
  require Logger

  @type relay_url :: binary()
  @type subscription_id :: binary()
  @type filters :: [map()]

  # Client API

  def start_link(relay_url, pool_pid) do
    WebSockex.start(relay_url, __MODULE__, %{pool_pid: pool_pid, relay_url: relay_url},
      handle_initial_conn_failure: true
    )
  end

  @doc """
  Sends a request to the relay and returns :ok or {:error, reason}
  """
  @spec send_request(pid(), subscription_id(), filters()) :: :ok | {:error, binary()}
  def send_request(pid, subscription_id, filters) do
    request = ["REQ", subscription_id | filters]
    Logger.debug("Sending request to relay: #{inspect(request)}")

    case WebSockex.send_frame(pid, {:text, Jason.encode!(request)}) do
      :ok -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  # WebSockex callbacks

  @impl true
  def handle_connect(conn, %{pool_pid: _pool_pid, relay_url: _relay_url} = state) do
    Logger.info("Connected to Nostr relay #{conn.host}")
    {:ok, state}
  end

  @impl true
  def handle_frame({:text, msg}, %{pool_pid: pool_pid} = state) do
    Logger.debug("Received frame: #{msg}")

    case Jason.decode(msg) do
      {:ok, ["EVENT", subscription_id, event]} ->
        Logger.debug("Received EVENT with subscription_id: #{subscription_id}")
        send(pool_pid, {:event, subscription_id, event})
        {:ok, state}

      {:ok, ["EOSE", subscription_id]} ->
        Logger.debug("Received EOSE for subscription_id: #{subscription_id}")
        send(pool_pid, {:eose, subscription_id})
        {:ok, state}

      {:ok, ["NOTICE", notice]} ->
        Logger.debug("Received NOTICE from relay: #{notice}")
        # We don't have subscription_id for notices, so we'll handle it differently
        send(pool_pid, {:notice, notice})
        {:ok, state}

      {:ok, other} ->
        Logger.debug("Received other message: #{inspect(other)}")
        {:ok, state}

      {:error, reason} ->
        Logger.error("Failed to decode message: #{reason}")
        {:ok, state}
    end
  end

  @impl true
  def handle_disconnect(%{conn: conn, reason: reason} = _disconnect_map, %{pool_pid: pool_pid, relay_url: relay_url} = state) do
    relay = if conn && Map.has_key?(conn, :host), do: conn.host, else: relay_url
    level = if reason == :normal or reason == :closed, do: :info, else: :error
    Logger.log(level, "Disconnected from Nostr relay #{relay} (reason: #{inspect(reason)})")

    # Notify the pool about the disconnection
    send(pool_pid, {:disconnect, relay_url, reason})

    {:ok, state}
  end
end
