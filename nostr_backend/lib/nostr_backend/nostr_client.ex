defmodule NostrBackend.NostrClient do
  use WebSockex

  require Logger

  # Type definitions
  @type relay_url :: binary()
  @type relay_urls :: [relay_url()]
  @type subscription_id :: binary()
  @type nostr_event :: map()
  @type event_list :: [nostr_event()]
  @type pubkey :: binary()
  @type event_id :: binary()
  @type address_info :: %{kind: integer(), author: binary(), identifier: binary()}
                     | %{kind: integer(), identifier: binary()}
                     | %{kind: integer(), pubkey: binary(), identifier: binary()}
  @type event_info :: %{id: event_id()}
  @type picture_post_info :: %{id: event_id()}
  @type fetch_result :: {:ok, nostr_event() | event_list()} | {:error, binary()}
  @type fetch_result_with_relay :: {:ok, relay_url(), nostr_event() | event_list()} | {:error, binary()}
  @type filter_map :: map()
  @type filters :: [filter_map()]

  # List of relay URLs
  @relay_urls [
    "wss://nostr.pareto.space",
    "wss://nostr.pareto.town",
    "wss://pareto.nostr1.com",
    "wss://nos.lol",
    "wss://relay.nostr.band",
    "wss://relay.damus.io"
    #   "wss://nostr.wine",
    #   "wss://relay.snort.social"
    # Add more relay URLs as needed
  ]

  @spec fetch_article_by_address(integer(), binary(), binary()) :: fetch_result()
  def fetch_article_by_address(kind, author, identifier) do
    fetch_article_by_address(kind, author, identifier, @relay_urls)
  end

  @spec fetch_article_by_address(integer(), binary(), binary(), relay_urls()) :: fetch_result_with_relay()
  def fetch_article_by_address(kind, author, identifier, []) do
    fetch_article_by_address(kind, author, identifier, @relay_urls)
  end

  @spec fetch_article_by_address(integer(), binary(), binary(), relay_urls()) :: fetch_result_with_relay()
  def fetch_article_by_address(kind, author, identifier, relay_urls) do
    address_info = %{kind: kind, author: author, identifier: identifier}
    fetch_from_relays(relay_urls, address_info, :address)
  end

  @spec fetch_article_by_address(integer(), binary()) :: fetch_result_with_relay()
  def fetch_article_by_address(kind, identifier) do
    address_info = %{kind: kind, identifier: identifier}
    fetch_from_relays(@relay_urls, address_info, :address)
  end

  @spec fetch_article_by_id(event_id(), relay_urls()) :: fetch_result_with_relay()
  def fetch_article_by_id(id, []) do
    fetch_article_by_id(id, @relay_urls)
  end

  @spec fetch_article_by_id(event_id(), relay_urls()) :: fetch_result_with_relay()
  def fetch_article_by_id(id, relay_urls) do
    event_info = %{id: id}
    fetch_from_relays(relay_urls, event_info, :event)
  end

  @spec fetch_article(String.t()) :: {:ok, map()} | {:error, String.t()}
  def fetch_article(article_hex_id) do
    case fetch_from_relays(@relay_urls, article_hex_id, :article) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_community(String.t()) :: {:ok, map()} | {:error, String.t()}
  def fetch_community(community_data) do
    case fetch_community(community_data, @relay_urls) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_community(String.t(), relay_urls()) :: fetch_result_with_relay()
  def fetch_community(community_data, relays) do
    fetch_from_relays(relays, community_data, :community)
  end

  @spec fetch_note(String.t()) :: {:ok, map()} | {:error, String.t()}
  def fetch_note(note_id) do
    case fetch_from_relays(@relay_urls, note_id, :note) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_picture_post(event_id(), relay_urls()) :: fetch_result()
  def fetch_picture_post(id, relays) do
    case fetch_from_relays(relays, %{id: id}, :picture_post) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_profile(String.t(), list()) :: {:ok, map()} | {:error, String.t()}
  def fetch_profile(profile_hex_id, []) do
    fetch_profile(profile_hex_id, @relay_urls)
  end

  @spec fetch_profile(pubkey(), relay_urls()) :: fetch_result_with_relay()
  def fetch_profile(profile_hex_id, relays) do
    fetch_from_relays(relays, profile_hex_id, :profile)
  end

  @spec fetch_follow_list(String.t(), list()) :: {:ok, any()} | {:error, String.t()}
  def fetch_follow_list(pubkey, []) do
    fetch_follow_list(pubkey, @relay_urls)
  end

  @spec fetch_follow_list(pubkey(), relay_urls()) :: fetch_result()
  def fetch_follow_list(pubkey, relay_urls) do
    # Fetch the follow list event(s) and strip out the relay
    case fetch_from_relays(relay_urls, pubkey, :follow_list) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_author_articles(String.t(), list()) :: {:ok, list(map())} | {:error, String.t()}
  def fetch_author_articles(pubkey, []) do
    fetch_author_articles(pubkey, @relay_urls)
  end

  @spec fetch_author_articles(pubkey(), relay_urls()) :: fetch_result()
  def fetch_author_articles(pubkey, relay_urls) do
    # Return raw events – strip out the relay and return only events for caching
    case fetch_from_relays(relay_urls, pubkey, :author_articles) do
      {:ok, _relay, events} -> {:ok, events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_multiple_authors_articles(list(String.t()), list()) :: {:ok, list(map())} | {:error, String.t()}
  def fetch_multiple_authors_articles(pubkeys, []) do
    fetch_multiple_authors_articles(pubkeys, @relay_urls)
  end

  @spec fetch_multiple_authors_articles([pubkey()], relay_urls()) :: fetch_result()
  def fetch_multiple_authors_articles(pubkeys, relay_urls) do
    Logger.info("Fetching articles for #{length(pubkeys)} authors from #{length(relay_urls)} relays")
    case fetch_from_relays(relay_urls, pubkeys, :multiple_authors_articles) do
      {:ok, _relay, events} -> {:ok, events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_from_relays(relay_urls(), any(), atom()) :: fetch_result_with_relay()
  defp fetch_from_relays([], _id, _type) do
    Logger.error("Failed to fetch data from all relays")
    {:error, "Failed to fetch data from all relays"}
  end

  @spec fetch_from_relays(relay_urls(), any(), atom()) :: fetch_result_with_relay()
  defp fetch_from_relays([relay | rest], id, type) do
    Logger.info("Fetching from relay: #{relay}")
    case fetch_from_relay(relay, id, type) do
      {:ok, data} ->
        Logger.info("Successfully fetched data from relay #{relay}: #{length(data)} items")
        {:ok, relay, data}
      {:error, reason} ->
        Logger.warning("Failed to fetch from relay #{relay}: #{inspect(reason)}, trying next relay")
        fetch_from_relays(rest, id, type)
    end
  end

  @spec fetch_from_relay(relay_url(), any(), atom()) :: fetch_result()
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

  @spec start_link(relay_url(), pid()) :: {:ok, pid()} | {:error, any()}
  defp start_link(relay_url, caller_pid) do
    Logger.debug("Relay URL: #{relay_url}")

    WebSockex.start(relay_url, __MODULE__, %{caller_pid: caller_pid},
      handle_initial_conn_failure: true
    )
  end

  @spec request_event(pid(), filters()) :: fetch_result()
  defp request_event(pid, filters) do
    subscription_id = UUID.uuid4()
    request = ["REQ", subscription_id | filters]

    Logger.info("Sending request to WebSocket: #{inspect(request)}")

    WebSockex.send_frame(pid, {:text, Jason.encode!(request)})

    Logger.info("Waiting for events...")
    collect_events(subscription_id, pid, [])
  end

  @spec collect_events(subscription_id(), pid(), event_list()) :: fetch_result()
  defp collect_events(subscription_id, pid, events) do
    receive do
      {:event, ^subscription_id, event} ->
        Logger.debug("Received event for subscription_id: #{subscription_id}")
        # Extract the event data from the tuple/list format
        event_data = case event do
          [{"EVENT", _sub_id, data}] -> data
          {"EVENT", _sub_id, data} -> data
          data when is_map(data) -> data
          _ -> event
        end
        Logger.debug("Extracted event data: #{inspect(Map.take(event_data, ["id", "pubkey", "kind"]))}")
        collect_events(subscription_id, pid, [event_data | events])

      {:eose, ^subscription_id} ->
        Logger.info("Received EOSE for subscription_id: #{subscription_id}, collected #{length(events)} events")
        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        if events == [] do
          Logger.warning("No events found for subscription_id: #{subscription_id}")
          {:error, "No events found"}
        else
          Logger.info("Returning #{length(events)} events")
          {:ok, Enum.reverse(events)}
        end

      {:notice, notice} ->
        Logger.warning("Received notice for subscription_id: #{subscription_id}: #{inspect(notice)}")
        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        {:error, "No events found"}
    after
      15_000 ->
        Logger.error("Timeout while fetching event for subscription_id: #{subscription_id}")
        WebSockex.send_frame(pid, {:text, Jason.encode!(["CLOSE", subscription_id])})
        Process.exit(pid, :normal)
        {:error, "Timeout while fetching event"}
    end
  end

  @spec build_filters(address_info(), :address) :: filters()
  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "authors" => [author],
        "#d" => [identifier]
      }
    ]
  end

  @spec build_filters(address_info(), :address) :: filters()
  defp build_filters(%{kind: kind, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "#d" => [identifier]
      }
    ]
  end

  @spec build_filters(address_info(), :address) :: filters()
  defp build_filters(%{kind: kind, pubkey: pubkey, identifier: identifier}, :address) do
    [
      %{
        "kinds" => [kind],
        "authors" => [pubkey],
        "#d" => [identifier]
      }
    ]
  end

  @spec build_filters(address_info(), :community) :: filters()
  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :community) do
    [
      %{
        "kinds" => [kind],
        "authors" => [author],
        "#d" => [identifier]
      }
    ]
  end

  @spec build_filters(event_info(), :event) :: filters()
  defp build_filters(%{id: id}, :event) do
    [
      %{
        "ids" => [id]
      }
    ]
  end

  @spec build_filters(event_id(), :note) :: filters()
  defp build_filters(note_id, :note) do
    [
      %{
        "ids" => [note_id]
      }
    ]
  end

  @spec build_filters(event_id(), :article) :: filters()
  defp build_filters(event_id, :article), do: [%{"ids" => [event_id]}]

  @spec build_filters(picture_post_info(), :picture_post) :: filters()
  defp build_filters(%{id: id}, :picture_post),
    do: [%{ "ids" => [id], "kinds" => [20], "limit" => 1}]

  @spec build_filters(pubkey(), :profile) :: filters()
  defp build_filters(pubkey, :profile),
    do: [%{"authors" => [pubkey], "kinds" => [0], "limit" => 1}]

  @spec build_filters(pubkey(), :follow_list) :: filters()
  defp build_filters(pubkey, :follow_list) do
    [
      %{
        "authors" => [pubkey],
        "kinds" => [3],
        "limit" => 1
      }
    ]
  end

  @spec build_filters(pubkey(), :author_articles) :: filters()
  defp build_filters(pubkey, :author_articles) do
    [
      %{
        "authors" => [pubkey],
        "kinds" => [30023],  # Article kind
        "limit" => 1000  # Adjust this based on your needs
      }
    ]
  end

  @spec build_filters([pubkey()], :multiple_authors_articles) :: filters()
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

  @impl true
  @spec handle_frame({:text, binary()}, map()) :: {:ok, map()}
  def handle_frame({:text, msg}, %{caller_pid: caller_pid} = state) do
    Logger.debug("Received frame: #{msg}")

    case Jason.decode(msg) do
      {:ok, ["EVENT", subscription_id, event]} ->
        Logger.debug("Received EVENT with subscription_id: #{subscription_id}")
        send(caller_pid, {:event, subscription_id, {"EVENT", subscription_id, event}})
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
  @spec handle_connect(any(), map()) :: {:ok, map()}
  def handle_connect(conn, state) do
    Logger.info("Connected to Nostr relay " <> conn.host)
    {:ok, state}
  end

  @impl true
  @spec handle_disconnect(map(), map()) :: {:ok, map()}
  def handle_disconnect(%{conn: conn, reason: reason} = _disconnect_map, state) do
    relay = if conn && Map.has_key?(conn, :host), do: conn.host, else: "unknown"
    level = if reason == :normal or reason == :closed, do: :info, else: :error
    Logger.log(level, "Disconnected from Nostr relay #{relay} (reason: #{inspect(reason)})")
    {:ok, state}
  end
end
