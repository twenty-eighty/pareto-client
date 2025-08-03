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

      {:ok, cached_image_url} ->
        # Serve cached result
        Logger.debug("OpenGraph: Serving cached image URL: #{cached_image_url}")
        conn
        |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
        |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
        |> redirect(external: cached_image_url)

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
            # Cache the result
            Cachex.put(:opengraph_cache, "image:#{url}", image_url)
            conn
            |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
            |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
            |> redirect(external: image_url)

          :error ->
            Logger.debug("OpenGraph: Failed to extract image URL")
            conn
            |> put_status(:not_found)
            |> text("OpenGraph image not found")
        end

      {:error, reason} ->
        Logger.error("OpenGraph: Failed to fetch URL: #{inspect(reason)}")
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

      {:ok, cached_metadata} ->
        # Serve cached result
        Logger.debug("OpenGraph: Serving cached metadata")
        conn
        |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
        |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
        |> json(cached_metadata)

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
        # Cache the result
        Cachex.put(:opengraph_cache, "metadata:#{url}", metadata)

        conn
        |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
        |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
        |> json(metadata)

      {:error, reason} ->
        Logger.error("OpenGraph: Failed to fetch URL: #{inspect(reason)}")
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
