defmodule NostrBackend.ReadFeed do
  @moduledoc """
  Provides a cached slice of the latest articles for the /read page.
  """

  alias NostrBackend.{ArticleCache, FollowListCache, Nip05Cache, ProfileCache}
  alias NostrBackendWeb.EventPayload

  require Logger

  @cache_name :read_feed_cache
  @cache_ttl_seconds 300
  @fetch_limit 50

  @doc """
  Returns the latest articles for the /read route along with their associated profiles.
  """
  @spec latest(integer()) ::
          {:ok, %{articles: list(), profiles: list(), events: list(), authors: list()}}
          | {:error, term()}
  def latest(limit \\ 4) do
    key = {:latest, limit}

    case Cachex.get(@cache_name, key) do
      {:ok, nil} ->
        with {:ok, feed} <- build_feed(limit) do
          Cachex.put(@cache_name, key, feed, ttl: @cache_ttl_seconds)
          {:ok, feed}
        end

      {:ok, feed} ->
        {:ok, feed}

      {:error, reason} ->
        Logger.error("ReadFeed cache error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Forces a refresh of the cached feed, replacing the cache entry immediately.
  """
  @spec refresh(integer()) :: {:ok, map()} | {:error, term()}
  def refresh(limit \\ 4) do
    key = {:latest, limit}

    with {:ok, feed} <- build_feed(limit) do
      case Cachex.put(@cache_name, key, feed, ttl: @cache_ttl_seconds) do
        {:ok, _} -> {:ok, feed}
        other -> other
      end
    end
  end

  defp build_feed(limit) do
    with {:ok, source_pubkey} <- follow_list_pubkey(),
         {:ok, follow_list} <- fetch_follow_list(source_pubkey),
         {:ok, articles} <- fetch_articles(follow_list, limit) do
      profiles = fetch_profiles(articles)

      events =
        Enum.map(profiles, fn profile ->
          EventPayload.raw_event(profile) || profile
        end) ++
          Enum.map(articles, fn article ->
            EventPayload.raw_event(article) || article
          end)

      authors = build_valid_authors(articles, profiles)

      {:ok, %{articles: articles, profiles: profiles, events: events, authors: authors}}
    end
  end

  defp follow_list_pubkey do
    case Application.get_env(:nostr_backend, :follow_list_pubkey) do
      pubkey when is_binary(pubkey) and byte_size(pubkey) > 0 ->
        {:ok, pubkey}

      _ ->
        Logger.error("FOLLOW_LIST_PUBKEY environment variable is not set")
        {:error, :missing_follow_list_pubkey}
    end
  end

  defp fetch_follow_list(pubkey) do
    case FollowListCache.get_follow_list(pubkey) do
      {:ok, []} ->
        Logger.warning("Follow list for #{pubkey} is empty")
        {:error, :empty_follow_list}

      {:ok, follow_list} ->
        {:ok, follow_list}

      {:error, reason} ->
        Logger.error("Failed to load follow list: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp fetch_articles([], _limit), do: {:ok, []}

  defp fetch_articles(follow_list, limit) do
    case ArticleCache.get_multiple_authors_articles(follow_list, [], limit: @fetch_limit) do
      {:ok, articles} ->
        articles =
          articles
          |> Enum.sort(fn article_a, article_b ->
            DateTime.compare(article_sort_key(article_a), article_sort_key(article_b)) == :gt
          end)
          |> Enum.take(limit)

        {:ok, articles}

      {:error, reason} ->
        Logger.error("Failed to load articles for /read feed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp fetch_profiles(articles) do
    articles
    |> Enum.map(&Map.get(&1, :author))
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.map(&get_profile/1)
    |> Enum.reject(&is_nil/1)
  end

  defp get_profile(pubkey) do
    case ProfileCache.get_profile(pubkey, []) do
      {:ok, profile} ->
        profile

      {:error, reason} ->
        Logger.debug("Skipping profile #{pubkey}: #{inspect(reason)}")
        nil
    end
  end

  defp build_valid_authors(articles, profiles) do
    profiles_by_pubkey =
      profiles
      |> Enum.map(fn prof -> {Map.get(prof, :profile_id), prof} end)
      |> Enum.reject(fn {pubkey, _} -> is_nil(pubkey) end)
      |> Map.new()

    articles
    |> Enum.map(&Map.get(&1, :author))
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.reduce([], fn pubkey, acc ->
      profile = Map.get(profiles_by_pubkey, pubkey)
      nip05 = if profile, do: Map.get(profile, :nip05), else: nil

      if valid_nip05_for_pubkey?(nip05, pubkey) do
        acc ++ [%{"nip-05" => nip05, "pubkey" => pubkey}]
      else
        acc
      end
    end)
  end

  defp valid_nip05_for_pubkey?(nip05, pubkey)
       when is_binary(nip05) and nip05 != "" and is_binary(pubkey) and pubkey != "" do
    case Nip05Cache.get_pubkey_and_relays(nip05) do
      {:ok, resolved_pubkey, _relays} -> resolved_pubkey == pubkey
      _ -> false
    end
  end

  defp valid_nip05_for_pubkey?(_, _), do: false

  defp article_sort_key(article) do
    Map.get(article, :published_at) ||
      Map.get(article, :created_at) ||
      DateTime.from_unix!(0)
  end
end
