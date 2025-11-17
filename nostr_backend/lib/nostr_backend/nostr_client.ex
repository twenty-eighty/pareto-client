defmodule NostrBackend.NostrClient do
  require Logger

  # Type definitions
  @type relay_url :: binary()
  @type relay_urls :: [relay_url()]
  @type subscription_id :: binary()
  @type nostr_event :: map()
  @type event_list :: [nostr_event()]
  @type pubkey :: binary()
  @type event_id :: binary()
  @type address_info ::
          %{kind: integer(), author: binary(), identifier: binary()}
          | %{kind: integer(), identifier: binary()}
          | %{kind: integer(), pubkey: binary(), identifier: binary()}
  @type event_info :: %{id: event_id()}
  @type picture_post_info :: %{id: event_id()}
  @type fetch_result :: {:ok, nostr_event() | event_list()} | {:error, binary()}
  @type fetch_result_with_relay ::
          {:ok, relay_url(), nostr_event() | event_list()} | {:error, binary()}
  @type filter_map :: map()
  @type filters :: [filter_map()]

  # List of relay URLs
  @relay_urls [
    "wss://nostr.pareto.town",
    "wss://nostr.pareto.space",
    "wss://pareto.nostr1.com",
    "wss://nos.lol",
    "wss://relay.nostr.band",
    "wss://relay.damus.io"
    #   "wss://nostr.wine",
    #   "wss://relay.snort.social"
    # Add more relay URLs as needed
  ]

  @spec fetch_article_by_address(integer(), binary(), binary()) :: fetch_result_with_relay()
  def fetch_article_by_address(kind, author, identifier) do
    fetch_article_by_address(kind, author, identifier, @relay_urls)
  end

  @spec fetch_article_by_address(integer(), binary(), binary(), relay_urls()) ::
          fetch_result_with_relay()
  def fetch_article_by_address(kind, author, identifier, []) do
    fetch_article_by_address(kind, author, identifier, @relay_urls)
  end

  @spec fetch_article_by_address(integer(), binary(), binary(), relay_urls()) ::
          fetch_result_with_relay()
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

  @spec fetch_article(String.t()) :: fetch_result()
  def fetch_article(article_hex_id) do
    case fetch_from_relays(@relay_urls, article_hex_id, :article) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_community(any()) :: fetch_result()
  def fetch_community(community_data) do
    case fetch_community(community_data, @relay_urls) do
      {:ok, _relay, event_or_events} -> {:ok, event_or_events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_community(any(), relay_urls()) :: fetch_result_with_relay()
  def fetch_community(community_data, relays) do
    fetch_from_relays(relays, community_data, :community)
  end

  @spec fetch_note(String.t()) :: fetch_result()
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

  @spec fetch_profile(String.t(), list()) :: fetch_result_with_relay()
  def fetch_profile(profile_hex_id, []) do
    fetch_profile(profile_hex_id, @relay_urls)
  end

  @spec fetch_profile(pubkey(), relay_urls()) :: fetch_result_with_relay()
  def fetch_profile(profile_hex_id, relays) do
    fetch_from_relays(relays, profile_hex_id, :profile)
  end

  @spec fetch_follow_list(String.t(), list()) :: fetch_result()
  def fetch_follow_list(pubkey, []) do
    fetch_follow_list(pubkey, @relay_urls)
  end

  @spec fetch_follow_list(pubkey(), relay_urls()) :: fetch_result()
  def fetch_follow_list(pubkey, relay_urls) do
    # Fetch the follow list event(s) and strip out the relay
    case fetch_from_relays(relay_urls, pubkey, :follow_list) do
      {:ok, _relay, event_or_events} ->
        {:ok, event_or_events}

      {:error, reason} ->
        Logger.error("NostrClient: Error fetching follow list: #{reason}")
        {:error, reason}
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

  @spec fetch_multiple_authors_articles(list(String.t()), list()) ::
          {:ok, list(map())} | {:error, String.t()}
  def fetch_multiple_authors_articles(pubkeys, []) do
    fetch_multiple_authors_articles(pubkeys, @relay_urls)
  end

  @spec fetch_multiple_authors_articles([pubkey()], relay_urls()) :: fetch_result()
  def fetch_multiple_authors_articles(pubkeys, relay_urls) do
    Logger.info(
      "Fetching articles for #{length(pubkeys)} authors from #{length(relay_urls)} relays"
    )

    case fetch_from_relays(relay_urls, pubkeys, :multiple_authors_articles) do
      {:ok, _relay, events} -> {:ok, events}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec fetch_from_relays(relay_urls(), any(), atom()) :: fetch_result_with_relay()
  defp fetch_from_relays(relay_urls, id, type) do
    filter = build_filters(id, type)

    base_opts = [overall_timeout: 60_000, cache?: false]

    opts =
      case type do
        :author_articles -> Keyword.put(base_opts, :paginate, true)
        :multiple_authors_articles -> Keyword.put(base_opts, :paginate, true)
        _ -> base_opts
      end

    case NostrAccess.fetch(relay_urls, filter, opts) do
      {:ok, events} ->
        relay_url = List.first(relay_urls) || "unknown"
        Logger.info("Successfully fetched data from relay #{relay_url}: #{length(events)} items")
        {:ok, relay_url, events}

      {:error, reason} ->
        Logger.error("Failed to fetch data from relays: #{inspect(reason)}")
        {:error, "No results from any relay"}
    end
  end

  # Filters moved here from the removed RelayConnectionPool
  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :address) do
    %{
      :"#d" => [identifier],
      kinds: [kind],
      authors: [author]
    }
  end

  defp build_filters(%{kind: kind, identifier: identifier}, :address) do
    %{
      :"#d" => [identifier],
      kinds: [kind]
    }
  end

  defp build_filters(%{kind: kind, pubkey: pubkey, identifier: identifier}, :address) do
    %{
      :"#d" => [identifier],
      kinds: [kind],
      authors: [pubkey]
    }
  end

  defp build_filters(%{kind: kind, author: author, identifier: identifier}, :community) do
    %{
      :"#d" => [identifier],
      kinds: [kind],
      authors: [author]
    }
  end

  defp build_filters(%{id: id}, :event) do
    %{
      ids: [id]
    }
  end

  defp build_filters(note_id, :note) do
    %{
      ids: [note_id]
    }
  end

  defp build_filters(event_id, :article), do: %{ids: [event_id]}

  defp build_filters(%{id: id}, :picture_post),
    do: %{ids: [id], kinds: [20], limit: 1}

  defp build_filters(pubkey, :profile),
    do: %{authors: [pubkey], kinds: [0], limit: 1}

  defp build_filters(pubkey, :follow_list) do
    %{
      authors: [pubkey],
      kinds: [3],
      limit: 1
    }
  end

  defp build_filters(pubkey, :author_articles) do
    %{
      authors: [pubkey],
      kinds: [30023],
      limit: 1000
    }
  end

  defp build_filters(pubkeys, :multiple_authors_articles) do
    Logger.info("Building filters for multiple authors articles: #{length(pubkeys)} authors")

    %{
      authors: pubkeys,
      kinds: [30023],
      limit: 1000
    }
  end
end
