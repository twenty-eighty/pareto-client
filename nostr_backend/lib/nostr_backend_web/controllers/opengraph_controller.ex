defmodule NostrBackendWeb.OpenGraphController do
  use NostrBackendWeb, :controller
  alias Req
  alias Floki
  alias NostrBackendWeb.SharedHttpClient
  require Logger

  def fetch_metadata_image(conn, %{"url" => url}) do
    Logger.debug("OpenGraph: Fetching metadata image for URL: #{url}")

    # Check cache first
    case Cachex.get(:opengraph_cache, "image:#{url}") do
      {:ok, nil} ->
        # Not in cache, fetch and cache it
        fetch_and_cache_image(conn, url)

      {:ok, cached_image_url} when is_binary(cached_image_url) ->
        # Serve cached successful result
        Logger.debug("OpenGraph: Serving cached image URL: #{cached_image_url}")
        conn
        |> put_resp_header("access-control-allow-origin", "pareto.space")
        |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
        |> redirect(external: cached_image_url)

      {:ok, :not_found} ->
        # Serve cached failure result
        Logger.debug("OpenGraph: Serving cached not found result")
        conn
        |> put_status(:not_found)
        |> text("OpenGraph image not found")

      {:ok, :fetch_error} ->
        # Serve cached fetch error result
        Logger.debug("OpenGraph: Serving cached fetch error result")
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch URL")

      {:error, reason} ->
        Logger.error("OpenGraph: Cache error: #{inspect(reason)}")
        conn
        |> put_status(:internal_server_error)
        |> text("Cache error: #{inspect(reason)}")
    end
  end

  defp fetch_and_cache_image(conn, url) do
    # Use the shared HTTP client
    case SharedHttpClient.fetch_url_with_cookies(url) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        case extract_image_url(body) do
          {:ok, image_url} ->
            Logger.debug("OpenGraph: Extracted image URL: #{image_url}")
            # Cache successful results for 24 hours
            Cachex.put(:opengraph_cache, "image:#{url}", image_url, ttl: :timer.hours(24))
                    conn
        |> put_resp_header("access-control-allow-origin", "pareto.space")
        |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
        |> redirect(external: image_url)

          :error ->
            Logger.debug("OpenGraph: Failed to extract image URL")
            # Cache not found errors for 1 hour
            Cachex.put(:opengraph_cache, "image:#{url}", :not_found, ttl: :timer.hours(1))
            conn
            |> put_status(:not_found)
            |> text("OpenGraph image not found")
        end

      {:error, reason} ->
        Logger.error("OpenGraph: Failed to fetch URL #{url}: #{inspect(reason)}")
        # Cache fetch errors for 30 minutes (shorter for network issues)
        Cachex.put(:opengraph_cache, "image:#{url}", :fetch_error, ttl: :timer.minutes(30))
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch URL: #{inspect(reason)}")
    end
  end

  defp extract_image_url(html) do
    html
    |> Floki.parse_document!()
    |> Floki.find("meta[property='og:image']")
    |> Floki.attribute("content")
    |> List.first()
    |> case do
      nil -> :error
      image_url -> {:ok, image_url}
    end
  end

  def fetch_metadata(conn, %{"url" => url}) do
    Logger.debug("OpenGraph: Fetching metadata for URL: #{url}")

    # Check cache first
    case Cachex.get(:opengraph_cache, "metadata:#{url}") do
      {:ok, nil} ->
        # Not in cache, fetch and cache it
        fetch_and_cache_metadata(conn, url)

      {:ok, cached_metadata} when is_map(cached_metadata) ->
        # Serve cached successful result
        Logger.debug("OpenGraph: Serving cached metadata")
        conn
        |> put_resp_header("access-control-allow-origin", "pareto.space")
        |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
        |> json(cached_metadata)

      {:ok, :fetch_error} ->
        # Serve cached fetch error result
        Logger.debug("OpenGraph: Serving cached fetch error result")
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Failed to fetch URL"})

      {:error, reason} ->
        Logger.error("OpenGraph: Cache error: #{inspect(reason)}")
        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Cache error: #{inspect(reason)}"})
    end
  end

  defp fetch_and_cache_metadata(conn, url) do
    # Use the shared HTTP client
    case SharedHttpClient.fetch_url_with_cookies(url) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        metadata = extract_opengraph_metadata(body)
        Logger.debug("OpenGraph: Extracted metadata: #{inspect(metadata)}")
        # Cache successful results for 24 hours
        Cachex.put(:opengraph_cache, "metadata:#{url}", metadata, ttl: :timer.hours(24))

        conn
        |> put_resp_header("access-control-allow-origin", "pareto.space")
        |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
        |> json(metadata)

      {:error, reason} ->
        Logger.error("OpenGraph: Failed to fetch URL #{url}: #{inspect(reason)}")
        # Cache fetch errors for 30 minutes (shorter for network issues)
        Cachex.put(:opengraph_cache, "metadata:#{url}", :fetch_error, ttl: :timer.minutes(30))
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Failed to fetch URL: #{inspect(reason)}"})
    end
  end

  defp extract_opengraph_metadata(html) do
    html
    |> Floki.parse_document!()
    |> Floki.find("meta[property^='og:']")
    |> Enum.reduce(%{}, fn tag, acc ->
      property =
        Floki.attribute(tag, "property") |> List.first() |> String.replace_prefix("og:", "")

      content = Floki.attribute(tag, "content") |> List.first()
      Map.put(acc, property, content)
    end)
  end
end
