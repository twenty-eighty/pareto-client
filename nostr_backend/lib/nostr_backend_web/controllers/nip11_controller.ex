defmodule NostrBackendWeb.Nip11Controller do
  use NostrBackendWeb, :controller
  alias Req
  require Logger

  @cache_name :nip11_cache
  # Cache successful results for 24 hours
  @success_cache_ttl :timer.hours(24)
  # Cache errors for 30 minutes
  @error_cache_ttl :timer.minutes(30)
  # 5 seconds timeout for HTTP requests
  @timeout 5_000

  def fetch_nip11(conn, %{"url" => relay_url}) do
    Logger.debug("NIP-11: Fetching data for relay: #{relay_url}")

    conn =
      conn
      |> put_resp_header("access-control-allow-origin", "*")
      # |> put_resp_header("access-control-allow-origin", "pareto.space")
      |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")

    # Check cache first
    case Cachex.get(@cache_name, relay_url) do
      {:ok, nil} ->
        # Not in cache, validate and fetch
        validate_and_fetch_nip11(conn, relay_url)

      {:ok, cached_data} when is_map(cached_data) ->
        # Serve cached successful result
        Logger.debug("NIP-11: Serving cached data for: #{relay_url}")
        json(conn, cached_data)

      {:ok, :invalid_url} ->
        # Serve cached invalid URL result
        Logger.debug("NIP-11: Serving cached invalid URL result for: #{relay_url}")

        conn
        |> put_status(:bad_request)
        |> text("Invalid relay URL")

      {:ok, :http_error} ->
        # Serve cached HTTP error result
        Logger.debug("NIP-11: Serving cached HTTP error result for: #{relay_url}")

        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch NIP-11 data: HTTP error")

      {:ok, :invalid_json} ->
        # Serve cached JSON error result
        Logger.debug("NIP-11: Serving cached JSON error result for: #{relay_url}")

        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch NIP-11 data: Invalid JSON")

      {:ok, :request_failed} ->
        # Serve cached request failed result
        Logger.debug("NIP-11: Serving cached request failed result for: #{relay_url}")

        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch NIP-11 data: Request failed")

      {:error, reason} ->
        Logger.error("NIP-11: Cache error: #{inspect(reason)}")

        conn
        |> put_status(:internal_server_error)
        |> text("Cache error: #{inspect(reason)}")
    end
  end

  defp validate_and_fetch_nip11(conn, relay_url) do
    with :ok <- validate_url(relay_url),
         {:ok, nip11_data} <- fetch_and_cache_nip11(relay_url) do
      json(conn, nip11_data)
    else
      {:error, :invalid_url} ->
        # Cache invalid URL error for 1 hour
        Cachex.put(@cache_name, relay_url, :invalid_url, ttl: :timer.hours(1))

        conn
        |> put_status(:bad_request)
        |> text("Invalid relay URL")

      {:error, reason} ->
        Logger.error("NIP-11: Failed to fetch data for #{relay_url}: #{inspect(reason)}")

        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch NIP-11 data: #{inspect(reason)}")
    end
  end

  # Fetches and caches NIP-11 data
  defp fetch_and_cache_nip11(relay_url) do
    case fetch_nip11_data(relay_url) do
      {:ok, nip11_data} ->
        Logger.debug("NIP-11: Successfully fetched data for: #{relay_url}")
        # Cache successful results for 24 hours
        Cachex.put(@cache_name, relay_url, nip11_data, ttl: @success_cache_ttl)
        {:ok, nip11_data}

      {:error, :http_error} ->
        Logger.debug("NIP-11: HTTP error for: #{relay_url}")
        # Cache HTTP errors for 30 minutes
        Cachex.put(@cache_name, relay_url, :http_error, ttl: @error_cache_ttl)
        {:error, :http_error}

      {:error, :invalid_json} ->
        Logger.debug("NIP-11: Invalid JSON for: #{relay_url}")
        # Cache JSON errors for 30 minutes
        Cachex.put(@cache_name, relay_url, :invalid_json, ttl: @error_cache_ttl)
        {:error, :invalid_json}

      {:error, :request_failed} ->
        Logger.debug("NIP-11: Request failed for: #{relay_url}")
        # Cache request failures for 30 minutes
        Cachex.put(@cache_name, relay_url, :request_failed, ttl: @error_cache_ttl)
        {:error, :request_failed}

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
          {:error, _decode_error} -> {:error, :invalid_json}
        end

      {:ok, %Req.Response{status: _status}} ->
        {:error, :http_error}

      {:error, _error} ->
        {:error, :request_failed}
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
