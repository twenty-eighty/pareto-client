defmodule NostrBackend.CommunityCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  @cache_name :communities_cache
  # 24 hours
  @ttl_in_seconds 86_400

  def get_community(community_data) do
    case Cachex.get(@cache_name, community_data) do
      {:ok, nil} ->
        # Community not found in cache, load it
        with {:ok, community} <- load_community(community_data) do
          # Store the community in the cache with a TTL
          Cachex.put(@cache_name, community_data, community, ttl: @ttl_in_seconds)
          {:ok, community}
        else
          error -> error
        end

      {:ok, community} ->
        {:ok, community}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp load_community(community_data) do
    # Implement the logic to load the community from the Nostr network
    # For example:
    case NostrClient.fetch_community(community_data) do
      {:ok, event} -> {:ok, Content.parse_community_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end
end
