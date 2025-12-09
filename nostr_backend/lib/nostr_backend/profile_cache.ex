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
        {event_to_parse, raw_event} = extract_event_and_raw(event_or_events)

        profile = Content.parse_profile_event(event_to_parse)

        profile =
          profile
          |> maybe_put_relays(relay)
          |> maybe_put_raw_event(raw_event)

        # Only return the profile if it has meaningful data (more than just relays)
        if is_map(profile) and map_size(profile) > 1 do
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

  defp extract_event_and_raw([single_event]) do
    {single_event, normalize_event(single_event)}
  end

  defp extract_event_and_raw(event) do
    {event, normalize_event(event)}
  end

  defp maybe_put_relays(profile, relay) when is_map(profile) do
    Map.put(profile, :relays, [relay])
  end

  defp maybe_put_relays(profile, _relay), do: profile

  defp maybe_put_raw_event(profile, nil), do: profile

  defp maybe_put_raw_event(profile, raw_event) when is_map(profile) do
    Map.put(profile, :raw_event, raw_event)
  end

  defp normalize_event({_, _, %{} = event_map}), do: event_map
  defp normalize_event(%{} = event_map), do: event_map
  defp normalize_event(_), do: nil
end
