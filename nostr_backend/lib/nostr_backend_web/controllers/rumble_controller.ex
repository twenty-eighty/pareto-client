defmodule NostrBackendWeb.RumbleController do
  use NostrBackendWeb, :controller
  require Logger

  alias Req
  alias Floki

  def fetch_embed_url(conn, %{"url" => url}) do
    Logger.debug("URL: #{url}")
    headers = [
      {"User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"},
      {"Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"},
      {"Accept-Language", "en-US,en;q=0.5"},
      {"Accept-Encoding", "gzip, deflate, br"},
      {"DNT", "1"},
      {"Connection", "keep-alive"},
      {"Upgrade-Insecure-Requests", "1"},
      {"Sec-Fetch-Dest", "document"},
      {"Sec-Fetch-Mode", "navigate"},
      {"Sec-Fetch-Site", "none"}
    ]

    # Create a cookie jar and attach the HttpCookie plugin
    empty_jar = HttpCookie.Jar.new()

    req = Req.new(headers: headers, max_redirects: 5)
    |> HttpCookie.ReqPlugin.attach()

    case Req.get(req, url: url, cookie_jar: empty_jar) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        Logger.debug("Received HTML: #{url}")
        process_html_response(body, conn)

      {:ok, %Req.Response{status: status}} ->
        Logger.error("HTTP request failed with status: #{status}")
        conn
        |> put_status(:bad_request)
        |> text("HTTP request failed with status: #{status}")

      {:error, reason} ->
        Logger.error("Failed to fetch URL: #{inspect(reason)}")
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch URL: #{inspect(reason)}")
    end
  end

  defp process_html_response(body, conn) do
    case extract_embed_url(body) do
      {:ok, embed_url} ->
        Logger.debug("Extracted embed URL: #{embed_url}")
        redirect(conn, external: embed_url)

      :error ->
        Logger.error("Failed to extract embed URL from HTML")
        conn
        |> put_status(:not_found)
        |> text("Embed URL not found in the provided page")
    end
  end

  defp extract_embed_url(html) do
    Logger.debug("Extracting embed URL from HTML")
    Logger.debug("HTML length: #{byte_size(html)}, starts with: #{String.slice(html, 0, 100)}")

    parsed_html = Floki.parse_document!(html)

    # Try extraction methods in order of priority, return first success
    [
      &extract_from_oembed_links/1,
      &extract_from_json_ld/1,
      &extract_from_iframe_sources/1,
      &extract_from_meta_tags/1,
      &extract_from_regex_search/1,
      &extract_from_video_ids/1
    ]
    |> Enum.reduce_while(:error, fn extraction_fn, _acc ->
      case extraction_fn.(parsed_html) do
        {:ok, embed_url} -> {:halt, {:ok, embed_url}}
        :error -> {:cont, :error}
      end
    end)
  end

  defp extract_from_oembed_links(parsed_html) do
    oembed_links = parsed_html
    |> Floki.find("link[rel='alternate'][type='application/json+oembed']")
    |> Floki.attribute("href")

    Logger.debug("Found oEmbed links: #{inspect(oembed_links)}")

    case List.first(oembed_links) do
      nil ->
        Logger.debug("No oEmbed links found, trying next method")
        :error
      oembed_url ->
        case extract_url_from_oembed(oembed_url) do
          {:ok, embed_url} ->
            Logger.debug("Extracted embed URL from oEmbed: #{embed_url}")
            {:ok, embed_url}
          :error ->
            Logger.debug("Failed to extract from oEmbed, trying next method")
            :error
        end
    end
  end

  defp extract_from_json_ld(parsed_html) do
    embed_urls_from_json = extract_embed_urls_from_json_ld(parsed_html)
    Logger.debug("Found embed URLs from JSON-LD: #{inspect(embed_urls_from_json)}")

    case List.first(embed_urls_from_json) do
      nil ->
        Logger.debug("No JSON-LD embed URLs found, trying next method")
        :error
      embed_url ->
        Logger.debug("Using embed URL from JSON-LD: #{embed_url}")
        {:ok, embed_url}
    end
  end

  defp extract_from_iframe_sources(parsed_html) do
    iframe_srcs = parsed_html
    |> Floki.find("iframe")
    |> Floki.attribute("src")
    |> Enum.filter(&String.contains?(&1, "rumble.com/embed"))

    Logger.debug("Found iframe embed URLs: #{inspect(iframe_srcs)}")

    case List.first(iframe_srcs) do
      nil ->
        Logger.debug("No iframe embed URLs found, trying next method")
        :error
      embed_url ->
        Logger.debug("Using iframe embed URL: #{embed_url}")
        {:ok, embed_url}
    end
  end

  defp extract_from_meta_tags(parsed_html) do
    video_meta_tags = parsed_html
    |> Floki.find("meta[property='og:video'], meta[property='og:video:url'], meta[name='twitter:player']")
    |> Floki.attribute("content")

    Logger.debug("Found video meta tags: #{inspect(video_meta_tags)}")

    case List.first(video_meta_tags) do
      nil ->
        Logger.debug("No video meta tags found, trying next method")
        :error
      embed_url ->
        Logger.debug("Using video meta tag URL: #{embed_url}")
        {:ok, embed_url}
    end
  end

  defp extract_from_regex_search(parsed_html) do
    all_text = Floki.raw_html(parsed_html)
    embed_urls = Regex.scan(~r/https?:\/\/[^"\s]*\/embed\/[^"\s]*/, all_text)
    |> Enum.map(&List.first/1)
    |> Enum.uniq()

    Logger.debug("Found embed URLs in HTML: #{inspect(embed_urls)}")

    case List.first(embed_urls) do
      nil ->
        Logger.debug("No regex embed URLs found, trying next method")
        :error
      embed_url ->
        Logger.debug("Using regex-found embed URL: #{embed_url}")
        {:ok, embed_url}
    end
  end

  defp extract_from_video_ids(parsed_html) do
    all_text = Floki.raw_html(parsed_html)
    video_ids = Regex.scan(~r/"video[_-]?id":\s*"([^"]+)"/i, all_text)
    |> Enum.map(&Enum.at(&1, 1))

    Logger.debug("Found video IDs: #{inspect(video_ids)}")

    case List.first(video_ids) do
      nil ->
        Logger.debug("No video IDs found, extraction failed")
        :error
      video_id ->
        embed_url = "https://rumble.com/embed/#{video_id}/"
        Logger.debug("Constructed embed URL from video ID: #{embed_url}")
        {:ok, embed_url}
    end
  end

  defp extract_url_from_oembed(oembed_url) do
    case URI.parse(oembed_url) do
      %URI{query: query} when is_binary(query) ->
        query_params = URI.decode_query(query)
        case Map.get(query_params, "url") do
          nil -> :error
          embed_url -> {:ok, embed_url}
        end
      _ -> :error
    end
  end

  defp extract_embed_urls_from_json_ld(parsed_html) do
    json_ld_scripts = [
      # Standard selector
      parsed_html |> Floki.find("script[type='application/ld+json']"),
      # Alternative with quotes
      parsed_html |> Floki.find("script[type=\"application/ld+json\"]"),
      # Case-insensitive approach - find all scripts and filter
      parsed_html
      |> Floki.find("script")
      |> Enum.filter(fn script ->
        type_attr = Floki.attribute(script, "type") |> List.first()
        type_attr && String.downcase(type_attr) == "application/ld+json"
      end)
    ]
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.map(fn script ->
      # Try multiple extraction methods
      text_content = Floki.text(script)
      raw_content = Floki.raw_html(script)

      # Extract content between <script> tags using regex
      regex_content = case Regex.run(~r/<script[^>]*>(.*?)<\/script>/s, raw_content) do
        [_, content] -> content
        _ -> ""
      end

      # Use the first non-empty content
      cond do
        String.trim(text_content) != "" -> text_content
        String.trim(regex_content) != "" -> regex_content
        true -> ""
      end
    end)
    |> Enum.filter(&(String.trim(&1) != ""))

    json_ld_scripts
    |> Enum.with_index()
    |> Enum.flat_map(fn {script_content, index} ->
      Logger.debug("Processing JSON-LD script #{index}: #{String.slice(script_content, 0, 200)}...")

      case Jason.decode(script_content) do
        {:ok, json_data} when is_list(json_data) ->
          Logger.debug("JSON-LD is a list with #{length(json_data)} items")
          video_objects = json_data
          |> Enum.filter(&(Map.get(&1, "@type") == "VideoObject"))

          Logger.debug("Found #{length(video_objects)} VideoObject entries")

          embed_urls = video_objects
          |> Enum.map(fn obj ->
            embed_url = Map.get(obj, "embedUrl")
            Logger.debug("VideoObject embedUrl: #{inspect(embed_url)}")
            embed_url
          end)
          |> Enum.filter(&is_binary/1)

          embed_urls

        {:ok, json_data} when is_map(json_data) ->
          Logger.debug("JSON-LD is a map with @type: #{inspect(Map.get(json_data, "@type"))}")
          case Map.get(json_data, "@type") do
            "VideoObject" ->
              embed_url = Map.get(json_data, "embedUrl")
              Logger.debug("Single VideoObject embedUrl: #{inspect(embed_url)}")
              [embed_url]
            _ -> []
          end
          |> Enum.filter(&is_binary/1)

        {:error, reason} ->
          Logger.debug("Failed to parse JSON-LD: #{inspect(reason)}")
          []
      end
    end)
  end
end
