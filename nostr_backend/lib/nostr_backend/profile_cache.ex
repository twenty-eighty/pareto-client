defmodule NostrBackend.ProfileCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  @cache_name :profiles_cache
  # 24 hours
  @ttl_in_seconds 86_400

  def get_profile(pubkey) do
    case Cachex.get(@cache_name, pubkey) do
      {:ok, nil} ->
        # Profile not found in cache, load it
        with {:ok, profile} <- load_profile(pubkey) do
          # Store the profile in the cache with a TTL
          Cachex.put(@cache_name, pubkey, profile, ttl: @ttl_in_seconds)
          {:ok, profile}
        else
          error -> error
        end

      {:ok, profile} ->
        {:ok, profile}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp load_profile(pubkey) do
    # Implement the logic to load the profile from the Nostr network
    # For example:
    case NostrClient.fetch_profile(pubkey) do
      {:ok, event} -> {:ok, Content.parse_profile_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end
end
