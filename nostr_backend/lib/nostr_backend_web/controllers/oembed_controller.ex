defmodule NostrBackendWeb.OembedController do
  use NostrBackendWeb, :controller
  alias Req

  @allowed_origins [
    "https://pareto.space"
    # , "http://localhost:1234"
  ]

  def fetch_oembed(conn, %{"url" => oembed_url}) do
    conn =
      conn
      |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
      # |> put_resp_header("Access-Control-Allow-Origin", "*")
      |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")

    # Validate Origin Header
    case get_req_header(conn, "origin") do
      [origin] when origin in @allowed_origins ->
        conn
        |> handle_oembed_request(oembed_url)

      _ ->
        conn
        |> put_status(:forbidden)
        |> text("Access denied")
    end
  end

  defp handle_oembed_request(conn, oembed_url) do
    case Cachex.get(:oembed_cache, oembed_url) do
      {:ok, nil} ->
        # If not in cache, fetch the oEmbed data
        fetch_and_cache_oembed(conn, oembed_url)

      {:ok, cached_result} ->
        # Serve cached result
        json(conn, cached_result)

      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> text("Cache error: #{inspect(reason)}")
    end
  end

  defp fetch_and_cache_oembed(conn, oembed_url) do
    case Req.get(oembed_url, headers: default_headers()) do
      {:ok, %Req.Response{status: 200, body: body}} ->
        # Cache the result and return it
        Cachex.put(:oembed_cache, oembed_url, body)
        json(conn, body)

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch oEmbed URL: #{inspect(reason)}")
    end
  end

  defp default_headers do
    [
      {"User-Agent",
       "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"}
    ]
  end
end
