defmodule NostrBackendWeb.SharedHttpClient do
  @moduledoc """
  Shared HTTP client functionality for making requests to external services
  with proper cookie handling, browser-like headers, and redirect management.
  """

  alias Req
  require Logger

  @doc """
  Fetches a URL with sophisticated request handling including:
  - Browser-like headers to avoid bot detection
  - Cookie jar support for session management using HttpCookie
  - Redirect handling with limits
  - Proper timeouts
  """
  def fetch_url_with_cookies(url) do
    Logger.debug("SharedHttpClient: Fetching URL: #{url}")

    # Browser-like headers to avoid bot detection
    headers = [
      {"User-Agent",
       "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"},
      {"Accept",
       "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"},
      {"Accept-Language", "en-US,en;q=0.9"},
      {"Accept-Encoding", "gzip, deflate, br"},
      {"DNT", "1"},
      {"Connection", "keep-alive"},
      {"Upgrade-Insecure-Requests", "1"},
      {"Sec-Fetch-Dest", "document"},
      {"Sec-Fetch-Mode", "navigate"},
      {"Sec-Fetch-Site", "none"},
      {"Sec-Fetch-User", "?1"},
      {"Cache-Control", "max-age=0"}
    ]

    # Create a cookie jar and attach the HttpCookie plugin
    empty_jar = HttpCookie.Jar.new()

    req =
      Req.new(
        headers: headers,
        max_redirects: 5,
        # 60 seconds connection timeout
        connect_options: [timeout: 60_000],
        # 60 seconds receive timeout
        receive_timeout: 60_000
      )
      |> HttpCookie.ReqPlugin.attach()

    Req.get(req, url: url, cookie_jar: empty_jar)
  end

  @doc """
  Fetches a URL with custom headers (useful for specific services that need different headers)
  """
  def fetch_url_with_cookies(url, custom_headers) do
    Logger.debug("SharedHttpClient: Fetching URL with custom headers: #{url}")

    # Merge default headers with custom headers
    default_headers = [
      {"User-Agent",
       "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"},
      {"Accept",
       "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"},
      {"Accept-Language", "en-US,en;q=0.9"},
      {"Accept-Encoding", "gzip, deflate, br"},
      {"DNT", "1"},
      {"Connection", "keep-alive"},
      {"Upgrade-Insecure-Requests", "1"},
      {"Sec-Fetch-Dest", "document"},
      {"Sec-Fetch-Mode", "navigate"},
      {"Sec-Fetch-Site", "none"},
      {"Sec-Fetch-User", "?1"},
      {"Cache-Control", "max-age=0"}
    ]

    headers = default_headers ++ custom_headers

    # Create a cookie jar and attach the HttpCookie plugin
    empty_jar = HttpCookie.Jar.new()

    req =
      Req.new(
        headers: headers,
        max_redirects: 5,
        # 60 seconds connection timeout
        connect_options: [timeout: 60_000],
        # 60 seconds receive timeout
        receive_timeout: 60_000
      )
      |> HttpCookie.ReqPlugin.attach()

    Req.get(req, url: url, cookie_jar: empty_jar)
  end
end
