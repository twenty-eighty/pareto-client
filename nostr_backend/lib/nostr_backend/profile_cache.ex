defmodule NostrBackend.ProfileCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient
  require Logger

  # Type definitions
  @type pubkey :: binary()
  @type relay_urls :: [binary()]
  @type profile :: Content.profile()
  @type cache_result :: {:ok, profile()} | {:error, binary()}

  @cache_name :profiles_cache
  # 24 hours
  @ttl_in_seconds 86_400

  @spec get_profile(pubkey(), relay_urls()) :: cache_result()
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

  @spec load_profile(pubkey(), relay_urls()) :: cache_result()
  defp load_profile(pubkey, relays) do
    # Implement the logic to load the profile from the Nostr network
    case NostrClient.fetch_profile(pubkey, relays) do
      {:ok, relay, event_or_events} ->
        Logger.debug("Fetched profile event(s) from relay #{relay}: #{inspect(event_or_events)}")
        # Handle both single event and list of events
        event_to_parse = case event_or_events do
          [single_event] -> single_event
          _ -> event_or_events
        end
        profile = Content.parse_profile_event(event_to_parse)

        # Only return the profile if it has meaningful data (more than just relays)
        if is_map(profile) and map_size(profile) > 1 do
          # Attach the relay used to fetch the profile
          profile = Map.put(profile, :relays, [relay])
          Logger.debug("Parsed profile: #{inspect(profile)}")
          {:ok, profile}
        else
          Logger.warning("Profile parsing returned incomplete data for pubkey #{pubkey}")
          {:error, "Incomplete profile data"}
        end

      {:error, reason} ->
        Logger.error("Failed to fetch profile: #{inspect(reason)}")
        {:error, reason}
    end
  end
end
