defmodule NostrBackend.FollowListCache do
  alias NostrBackend.NostrClient
  require Logger

  @cache_name :follow_lists_cache
  @event_cache_name :follow_list_events_cache
  # 1 hour
  @ttl_in_seconds 3_600

  @doc """
  Gets a follow list for a given pubkey. If not in cache, fetches it from relays.
  Returns {:ok, follow_list} or {:error, reason}
  """
  def get_follow_list(pubkey, relays \\ []) do
    Logger.info("FollowListCache: Getting follow list for #{pubkey}")

    case Cachex.get(@cache_name, pubkey) do
      {:ok, nil} ->
        Logger.info("FollowListCache: Follow list not found in cache, loading from relays")
        # Follow list not found in cache, load it
        with {:ok, follow_list} <- load_follow_list(pubkey, relays) do
          # Store the follow list in the cache with a TTL
          Logger.info("FollowListCache: Loaded follow list with #{length(follow_list)} authors")
          Cachex.put(@cache_name, pubkey, follow_list, ttl: @ttl_in_seconds)
          {:ok, follow_list}
        else
          error ->
            Logger.error("FollowListCache: Error loading follow list: #{inspect(error)}")
            error
        end

      {:ok, follow_list} ->
        Logger.info(
          "FollowListCache: Found cached follow list with #{length(follow_list)} authors"
        )

        {:ok, follow_list}

      {:error, reason} ->
        Logger.error("FollowListCache: Cache error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Gets the raw follow list event (kind 3 "contacts") for a given pubkey.

  This is used to embed the follow list into HTML so the frontend can avoid
  shipping a static author list.

  Returns `{:ok, event_map}` or `{:error, reason}`.
  """
  @spec get_follow_list_event(binary(), [binary()]) :: {:ok, map()} | {:error, any()}
  def get_follow_list_event(pubkey, relays \\ []) do
    Logger.debug("FollowListCache: Getting follow list event for #{pubkey}")

    case Cachex.get(@event_cache_name, pubkey) do
      {:ok, nil} ->
        with {:ok, event} <- load_follow_list_event(pubkey, relays) do
          Cachex.put(@event_cache_name, pubkey, event, ttl: @ttl_in_seconds)
          {:ok, event}
        end

      {:ok, %{} = event} ->
        {:ok, event}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Forces a refresh of the raw follow list event for a given pubkey.
  """
  @spec refresh_follow_list_event(binary(), [binary()]) :: {:ok, map()} | {:error, any()}
  def refresh_follow_list_event(pubkey, relays \\ []) do
    with {:ok, event} <- load_follow_list_event(pubkey, relays) do
      Cachex.put(@event_cache_name, pubkey, event, ttl: @ttl_in_seconds)
      {:ok, event}
    end
  end

  @doc """
  Forces a refresh of the follow list for a given pubkey.
  Returns {:ok, follow_list} or {:error, reason}
  """
  def refresh_follow_list(pubkey, relays \\ []) do
    with {:ok, follow_list} <- load_follow_list(pubkey, relays) do
      Cachex.put(@cache_name, pubkey, follow_list, ttl: @ttl_in_seconds)
      {:ok, follow_list}
    end
  end

  defp load_follow_list(pubkey, relays) do
    Logger.info("FollowListCache: Loading follow list for #{pubkey}")

    case NostrClient.fetch_follow_list(pubkey, relays) do
      {:ok, event} ->
        Logger.debug("FollowListCache: Received event: #{inspect(event)}")
        {:ok, parse_follow_list(event)}

      {:error, reason} ->
        Logger.error("FollowListCache: Error fetching follow list: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp load_follow_list_event(pubkey, relays) do
    Logger.info("FollowListCache: Loading follow list event for #{pubkey}")

    case NostrClient.fetch_follow_list(pubkey, relays) do
      {:ok, raw} ->
        case first_event_map(raw) do
          %{} = event ->
            {:ok, event}

          nil ->
            {:error, :empty_follow_list_event}
        end

      {:error, reason} ->
        Logger.error("FollowListCache: Error fetching follow list event: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp first_event_map(raw) do
    case List.wrap(raw) do
      [%{} = event | _] -> event
      _ -> nil
    end
  end

  defp parse_follow_list(raw) do
    case List.wrap(raw) do
      [%{} = event | _] ->
        Logger.debug("Parsing first event in list")
        parse_follow_list_map(event)

      [] ->
        Logger.debug("No follow list events found - user may not have a follow list")
        []
    end
  end

  defp parse_follow_list_map(event) do
    Logger.debug("Parsing event map: #{inspect(event)}")

    case event do
      %{"tags" => tags} when is_list(tags) ->
        Logger.debug("Parsing follow list with #{length(tags)} tags")

        pubkeys =
          tags
          |> Enum.filter(fn [tag | _] -> tag == "p" end)
          |> Enum.map(fn ["p", pubkey | _] -> pubkey end)
          |> Enum.uniq()

        Logger.debug("Extracted #{length(pubkeys)} unique pubkeys from follow list")
        pubkeys

      _ ->
        Logger.debug("No tags found in follow list event")
        []
    end
  end
end
