defmodule NostrBackend.NostrClient do
  use WebSockex

  require Logger

  # List of relay URLs
  @relay_urls [
    "wss://nostr.pareto.space",
    "wss://pareto.nostr1.com",
    "wss://nos.lol"
    #   "wss://nostr.wine",
    #   "wss://relay.damus.io",
    #   "wss://relay.snort.social"
    # Add more relay URLs as needed
  ]

  def fetch_article_by_address(kind, author, identifier) do
    fetch_article_by_address(kind, author, identifier, @relay_urls)
  end

  def fetch_article_by_address(kind, author, identifier, relay_urls) do
    address_info = %{kind: kind, author: author, identifier: identifier}
    fetch_from_relays(relay_urls, address_info, :address)
  end

  def fetch_article_by_address(kind, identifier) do
    address_info = %{kind: kind, identifier: identifier}
    fetch_from_relays(@relay_urls, address_info, :address)
  end

  @spec fetch_article(String.t()) :: {:ok, map()} | {:error, String.t()}
  def fetch_article(article_hex_id) do
    fetch_from_relays(@relay_urls, article_hex_id, :article)
  end

  @spec fetch_community(String.t(), list()) :: {:ok, map()} | {:error, String.t()}
  def fetch_community(community_data) do
    fetch_community(community_data, @relay_urls)
  end

  def fetch_community(community_data, relays) do
    fetch_from_relays(relays, community_data, :community)
  end

  @spec fetch_profile(String.t(), list()) :: {:ok, map()} | {:error, String.t()}
  def fetch_profile(profile_hex_id, []) do
    fetch_profile(profile_hex_id, @relay_urls)
  end

  def fetch_profile(profile_hex_id, relays) do
    fetch_from_relays(relays, profile_hex_id, :profile)
  end

  defp fetch_from_relays([], _id, _type) do
    {:error, "Failed to fetch data from all relays"}
  end

  defp fetch_from_relays([relay | rest], id, type) do
    case fetch_from_relay(relay, id, type) do
      {:ok, data} -> {:ok, data}
      {:error, _reason} -> fetch_from_relays(rest, id, type)
    end
  end

  defp fetch_from_relay(relay_url, id, type) do
    case start_link(relay_url, self()) do
      {:ok, pid} ->
        filters = build_filters(id, type)

        case request_event(pid, filters) do
          {:ok, data} ->
            {:ok, data}

          {:error, reason} ->
            Logger.error("Failed to fetch data from relay #{relay_url}: #{reason}")
            {:error, reason}
        end

      {:error, reason} ->
        Logger.error("Failed to connect to relay #{relay_url}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp start_link(relay_url, caller_pid) do
    IO.inspect(relay_url, label: "Relay URL")

    WebSockex.start_link(relay_url, __MODULE__, %{caller_pid: caller_pid},
      handle_initial_conn_failure: true
    )
  end

  defp request_event(pid, filters) do
    subscription_id = UUID.uuid4()
    request = ["REQ", subscription_id | filters]

    Logger.debug("Sending request: #{inspect(request)}")

    WebSockex.send_frame(pid, {:text, Jason.encode!(request)})

    receive do
      {:event, ^subscription_id, event} ->
        Logger.debug("Received event for subscription_id: #{subscription_id}")
        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        {:ok, event}

      {:eose, ^subscription_id} ->
        Logger.debug("Received EOSE for subscription_id: #{subscription_id}, no events found")
        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        {:error, "No events found"}

      {:notice, notice} ->
        Logger.debug(
          "Received notice for subscription_id: #{subscription_id}: #{inspect(notice)}"
        )

        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        {:error, "No events found"}
    after
      15_000 ->
        Logger.error("Timeout while fetching event")
        Process.exit(pid, :normal)
        {:error, "Timeout while fetching event"}
    end
  end

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

  defp build_filters(event_id, :article), do: [%{"ids" => [event_id]}]

  defp build_filters(pubkey, :profile),
    do: [%{"authors" => [pubkey], "kinds" => [0], "limit" => 1}]

  @impl true
  def handle_frame({:text, msg}, %{caller_pid: caller_pid} = state) do
    Logger.debug("Received frame: #{msg}")

    case Jason.decode(msg) do
      {:ok, ["EVENT", subscription_id, event]} ->
        Logger.debug("Received EVENT with subscription_id: #{subscription_id}")
        send(caller_pid, {:event, subscription_id, event})
        {:ok, state}

      {:ok, ["EOSE", subscription_id]} ->
        Logger.debug("Received EOSE for subscription_id: #{subscription_id}")
        send(caller_pid, {:eose, subscription_id})
        {:ok, state}

      {:ok, ["NOTICE", notice]} ->
        Logger.debug("Received NOTICE from relay: #{notice}")
        send(caller_pid, {:notice, notice})
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
  def handle_connect(conn, state) do
    Logger.info("Connected to Nostr relay " <> conn.host)
    {:ok, state}
  end

  @impl true
  def handle_disconnect(%{reason: _reason} = _disconnect_map, state) do
    Logger.error("Disconnected from Nostr relay")
    {:ok, state}
  end
end
