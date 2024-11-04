defmodule NostrBackend.ArticleCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  @cache_name :articles_cache
  # 24 hours
  @ttl_in_seconds 86_400

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
        IO.inspect("Article found in cache")
        {:ok, article}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp load_article(%{kind: kind, identifier: identifier, author: author, relays: relays}) do
    # Implement the logic to load the article from the Nostr network
    # For example:
    case NostrClient.fetch_article_by_address(kind, author, identifier, relays) do
      {:ok, event} -> {:ok, Content.parse_article_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp load_article(%{kind: kind, identifier: identifier, author: author}) do
    # Implement the logic to load the article from the Nostr network
    # For example:
    case NostrClient.fetch_article_by_address(kind, author, identifier) do
      {:ok, event} -> {:ok, Content.parse_article_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp load_article(%{kind: kind, identifier: identifier}) do
    # Implement the logic to load the article from the Nostr network
    # For example:
    case NostrClient.fetch_article_by_address(kind, identifier) do
      {:ok, event} -> {:ok, Content.parse_article_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp load_article(article_id) do
    # Implement the logic to load the article from the Nostr network
    # For example:
    case NostrClient.fetch_article(article_id) do
      {:ok, event} -> {:ok, Content.parse_article_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end
end
