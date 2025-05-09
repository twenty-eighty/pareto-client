defmodule NostrBackend.ProfileCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient
  require Logger

  @cache_name :profiles_cache
  # 24 hours
  @ttl_in_seconds 86_400

  def get_profile(pubkey, relays) do
    case Cachex.get(@cache_name, pubkey) do
      {:ok, nil} ->
        # Profile not found in cache, load it
        with {:ok, profile} <- load_profile(pubkey, relays) do
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

  defp load_profile(pubkey, relays) do
    # Implement the logic to load the profile from the Nostr network
    # For example:
    case NostrClient.fetch_profile(pubkey, relays) do
      {:ok, event} ->
        Logger.debug("Fetched profile event: #{inspect(event)}")
        # Handle both single event and list of events
        event_to_parse = case event do
          [single_event] -> single_event
          _ -> event
        end
        profile = Content.parse_profile_event(event_to_parse)
        Logger.debug("Parsed profile: #{inspect(profile)}")
        {:ok, profile}
      {:error, reason} ->
        Logger.error("Failed to fetch profile: #{inspect(reason)}")
        {:error, reason}
    end
  end
end
