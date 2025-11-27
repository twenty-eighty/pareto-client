defmodule NostrBackendWeb.OembedController do
  use NostrBackendWeb, :controller
  require Logger

  alias Req
  alias NostrBackendWeb.SharedHttpClient

  import SweetXml

  @allowed_origins [
    "https://pareto.space",
    "https://test.pareto.space",
    "http://localhost:1234",
    "http://localhost:4000"
  ]

  def fetch_oembed(conn, %{"url" => oembed_url}) do
    conn =
      put_acces_control_headers(conn)

    origin = get_req_header(conn, "origin") |> List.first()

    allowed =
      case origin do
        nil ->
          # Fallback to Referer header if Origin is missing
          conn
          |> get_req_header("referer")
          |> List.first()
          |> extract_origin()

        origin ->
          origin
      end

    if allowed in @allowed_origins do
      handle_oembed_request(conn, oembed_url)
    else
      conn
      |> put_status(:forbidden)
      |> text("Access denied: Origin not allowed")
    end
  end

  defp extract_origin(nil), do: nil

  defp extract_origin(referer) do
    case URI.parse(referer) do
      %URI{scheme: scheme, host: host} when not is_nil(scheme) and not is_nil(host) ->
        "#{scheme}://#{host}"

      _ ->
        nil
    end
  end

  defp handle_oembed_request(conn, oembed_url) do
    case Cachex.get(:oembed_cache, oembed_url) do
      {:ok, nil} ->
        # If not in cache, fetch the oEmbed data
        fetch_and_cache_oembed(conn, oembed_url)

      {:ok, cached_result} when is_map(cached_result) ->
        # Serve cached successful result
        json(conn, cached_result)

      {:ok, :fetch_error} ->
        # Serve cached fetch error
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch oEmbed URL")

      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> text("Cache error: #{inspect(reason)}")
    end
  end

  defp fetch_and_cache_oembed(conn, oembed_url) do
    Logger.debug("OEmbed: Fetching oEmbed URL: #{oembed_url}")

    # Use the shared HTTP client
    case SharedHttpClient.fetch_url_with_cookies(oembed_url) do
      {:ok, %Req.Response{status: 200, body: body, headers: headers}} ->
        content_type = get_content_type(headers)

        case handle_body_by_content_type(content_type, body) do
          {:ok, json_body} ->
            # Cache the result and return it
            Cachex.put(:oembed_cache, oembed_url, json_body)
            json(conn, json_body)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")
            Logger.debug("BODY: #{inspect(body)}")
            # Cache processing errors for 30 minutes
            Cachex.put(:oembed_cache, oembed_url, :fetch_error, ttl: :timer.minutes(30))

            conn
            |> put_status(:unprocessable_entity)
            |> text("Failed to process oEmbed response: #{reason}")
        end

      {:error, reason} ->
        Logger.error("OEmbed: Failed to fetch oEmbed URL: #{inspect(reason)}")
        # Cache fetch errors for 30 minutes
        Cachex.put(:oembed_cache, oembed_url, :fetch_error, ttl: :timer.minutes(30))

        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch oEmbed URL: #{inspect(reason)}")
    end
  end

  defp handle_body_by_content_type("application/json", body) when is_map(body) do
    # JSON is already parsed
    {:ok, body}
  end

  defp handle_body_by_content_type("application/json", body) do
    case Jason.decode(body) do
      {:ok, json_body} -> {:ok, json_body}
      {:error, reason} -> {:error, "Invalid JSON: #{inspect(reason)}"}
    end
  end

  defp handle_body_by_content_type("application/xml", body) do
    parse_xml_to_json(body)
  end

  defp handle_body_by_content_type("text/xml", body) do
    parse_xml_to_json(body)
  end

  defp handle_body_by_content_type(actual_content_type, _body) do
    {:error, "Unsupported Content-Type: #{actual_content_type}"}
  end

  defp parse_xml_to_json(xml_body) do
    try do
      xml_body
      # Parse the raw XML string
      |> SweetXml.parse()
      # Select all child nodes of <oembed>
      |> xpath(~x"//oembed/*"l)
      |> Enum.reduce(%{}, fn element, acc ->
        # Extract tag name
        key = element |> SweetXml.xpath(~x"local-name(.)"s)

        # Extract text content
        value =
          element
          |> SweetXml.xpath(~x"text()"s)
          |> to_string()

        Map.put(acc, key, value)
      end)
      |> then(&{:ok, &1})
    rescue
      e -> {:error, "Failed to parse XML: #{Exception.message(e)}"}
    end
  end

  defp get_content_type(headers) do
    headers
    |> Enum.find_value("", fn {key, value} ->
      if String.downcase(key) == "content-type", do: value
    end)
    |> case do
      value when is_binary(value) -> String.split(value, ";") |> List.first()
      [value] when is_binary(value) -> String.split(value, ";") |> List.first()
      _ -> ""
    end
  end

  defp put_acces_control_headers(conn) do
    conn
    |> put_resp_header("access-control-allow-origin", "*")
    # |> put_resp_header("access-control-allow-origin", "pareto.space")
    |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
  end
end
