defmodule NostrBackend.ArticleCache do
  require Logger
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  # Type definitions
  @type pubkey :: binary()
  @type relay_urls :: [binary()]
  @type article :: Content.article()
  @type article_list :: [article()]
  @type article_id :: binary()
  @type article_query :: %{
    kind: integer(),
    identifier: binary(),
    author: binary(),
    relays: relay_urls()
  } | %{
    kind: integer(),
    identifier: binary(),
    author: binary()
  } | %{
    kind: integer(),
    identifier: binary()
  } | %{
    id: article_id(),
    relays: relay_urls()
  } | %{
    id: article_id()
  } | binary()
  @type cache_result :: {:ok, article()} | {:error, binary()}
  @type cache_result_list :: {:ok, article_list()} | {:error, binary()}

  @cache_name :articles_cache
  # 24 hours
  @ttl_in_seconds 86_400

  @spec get_article(article_query()) :: cache_result()
  def get_article(article_id) do
    case Cachex.get(@cache_name, article_id) do
      {:ok, nil} ->
        # Article not found in cache, load it
        with {:ok, article} <- load_article(article_id) do
          # Store the article in the cache with a TTL
          Cachex.put(@cache_name, article_id, article, ttl: @ttl_in_seconds)
          {:ok, article}
        else
          error -> error
        end

      {:ok, article} ->
        Logger.debug("Article found in cache")
        {:ok, article}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Fetches all articles from a specific author.
  Returns {:ok, articles} or {:error, reason}
  """
  @spec get_author_articles(pubkey(), relay_urls()) :: cache_result_list()
  def get_author_articles(pubkey, relays \\ []) do
    case NostrClient.fetch_author_articles(pubkey, relays) do
      {:ok, events} ->
        articles = events
        |> Enum.map(&Content.parse_article_event/1)
        |> Enum.filter(&(&1 != %{}))
        |> Enum.sort_by(fn article -> article.published_at end, :desc)

        # Cache each article
        Enum.each(articles, fn article ->
          Cachex.put(@cache_name, article.article_id, article, ttl: @ttl_in_seconds)
        end)

        {:ok, articles}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Fetches articles from multiple authors in a single request.
  Returns {:ok, articles} or {:error, reason}
  """
  @spec get_multiple_authors_articles([pubkey()], relay_urls()) :: cache_result_list()
  def get_multiple_authors_articles(pubkeys, relays \\ []) do
    case NostrClient.fetch_multiple_authors_articles(pubkeys, relays) do
      {:ok, events} ->
        articles = events
        |> Enum.map(&Content.parse_article_event/1)
        |> Enum.filter(&(&1 != %{}))
        |> Enum.sort_by(fn article -> article.published_at end, :desc)

        # Cache each article
        Enum.each(articles, fn article ->
          Cachex.put(@cache_name, article.article_id, article, ttl: @ttl_in_seconds)
        end)

        {:ok, articles}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(%{kind: kind, identifier: identifier, author: author, relays: relays}) do
    case NostrClient.fetch_article_by_address(kind, author, identifier, relays) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(%{kind: kind, identifier: identifier, author: author}) do
    case NostrClient.fetch_article_by_address(kind, author, identifier) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(%{kind: kind, identifier: identifier}) do
    case NostrClient.fetch_article_by_address(kind, identifier) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(%{id: id, relays: relays}) do
    case NostrClient.fetch_article_by_id(id, relays) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(%{id: id}) do
    case NostrClient.fetch_article_by_id(id, []) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_article(article_query()) :: cache_result()
  defp load_article(article_id) do
    case NostrClient.fetch_article(article_id) do
      {:ok, relay, [event | _]} ->
        article = Content.parse_article_event(event)
        article = Map.put(article, :relays, [relay])
        {:ok, article}
      {:ok, _relay, []} ->
        {:error, "No events found for article"}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
