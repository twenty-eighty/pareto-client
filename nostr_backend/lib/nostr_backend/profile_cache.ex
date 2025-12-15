defmodule NostrBackend.ProfileCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient
  require Logger
  @dialyzer {:nowarn_function, cache_profile: 2}

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
        load_profile(pubkey, relays)
        |> cache_profile(pubkey)

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
        {event_to_parse, raw_event} = extract_event_and_raw(event_or_events)

        profile =
          event_to_parse
          |> Content.parse_profile_event()
          |> maybe_put_relays(relay)
          |> maybe_put_raw_event(raw_event)

        Logger.debug("Parsed profile: #{inspect(profile)}")
        {:ok, profile}

      {:error, reason} ->
        Logger.error("Failed to fetch profile: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp extract_event_and_raw(event_or_events) do
    case List.wrap(event_or_events) do
      [%{} = event | _] -> {event, normalize_event(event)}
      _ -> {%{}, nil}
    end
  end

  defp maybe_put_relays(profile, relay), do: Map.put(profile, :relays, [relay])

  defp maybe_put_raw_event(profile, raw_event) do
    if is_map(raw_event) do
      Map.put(profile, :raw_event, raw_event)
    else
      profile
    end
  end

  defp normalize_event(event), do: event

  defp cache_profile({:error, reason} = error, pubkey) do
    Logger.warning("Failed to load profile #{pubkey}: #{inspect(reason)}")
    error
  end

  defp cache_profile(result, pubkey) do
    profile = elem(result, 1)
    Cachex.put(@cache_name, pubkey, profile, ttl: @ttl_in_seconds)
    result
  end
end
