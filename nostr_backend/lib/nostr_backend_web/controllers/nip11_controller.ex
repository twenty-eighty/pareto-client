defmodule NostrBackendWeb.Nip11Controller do
  use NostrBackendWeb, :controller
  alias Req

  @cache_name :nip11_cache
  @cache_ttl :timer.hours(1) # Cache TTL: 1 hour
  @timeout 5_000 # 5 seconds timeout for HTTP requests

  def fetch_nip11(conn, %{"url" => relay_url}) do
    conn =
      conn
      |> put_resp_header("Access-Control-Allow-Origin", "*")
      # |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
      |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")

    with :ok <- validate_url(relay_url),
         {:ok, nip11_data} <- get_cached_nip11(relay_url) do
      conn
      |> json(nip11_data)
    else
      {:error, :invalid_url} ->
        conn
        |> put_status(:bad_request)
        |> text("Invalid relay URL")

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch NIP-11 data: #{inspect(reason)}")
    end
  end

  # Fetches and caches NIP-11 data
  defp get_cached_nip11(relay_url) do
    case Cachex.get(@cache_name, relay_url) do
      {:ok, nil} ->
        # Not in cache, fetch from the relay
        case fetch_nip11_data(relay_url) do
          {:ok, nip11_data} ->
            # Cache the data with TTL and return it
            Cachex.put(@cache_name, relay_url, nip11_data, ttl: @cache_ttl)
            {:ok, nip11_data}

          error ->
            error
        end

      {:ok, cached_data} ->
        # Found in cache, return it
        {:ok, cached_data}

      {:error, reason} ->
        {:error, {:cache_error, reason}}
    end
  end

  # Makes the actual HTTP request to fetch NIP-11 data
  defp fetch_nip11_data(relay_url) do
    Req.new(url: relay_url, headers: default_headers(), receive_timeout: @timeout)
    |> Req.get()
    |> case do
      {:ok, %Req.Response{status: 200, body: body}} when is_map(body) ->
        {:ok, body}

      {:ok, %Req.Response{status: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, json_data} -> {:ok, json_data}
          {:error, decode_error} -> {:error, {:invalid_json, decode_error}}
        end

      {:ok, %Req.Response{status: status}} ->
        {:error, {:http_error, "Received HTTP status #{status}"}}

      {:error, error} ->
        {:error, {:http_request_failed, error}}
    end
  end

  defp validate_url(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and not is_nil(host) ->
        :ok

      _ ->
        {:error, :invalid_url}
    end
  end

  defp default_headers do
    [
      {"User-Agent", "Pareto"},
      {"Accept", "application/nostr+json"}
    ]
  end
end
