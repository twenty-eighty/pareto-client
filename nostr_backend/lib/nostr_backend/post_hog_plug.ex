defmodule NostrBackend.PostHogPlug do
  @client_hint_headers [
    "sec-ch-ua",
    "sec-ch-ua-mobile",
    "sec-ch-ua-platform",
    "sec-ch-ua-platform-version",
    "sec-ch-ua-full-version",
    "sec-ch-ua-arch",
    "sec-ch-ua-bitness",
    "sec-ch-ua-model"
  ]

  def init(options), do: options

  def call(conn, _options) do
    # Capture request information

    user_agent =
      Plug.Conn.get_req_header(conn, "user-agent")
      |> List.first()

    # get client's IP address
    forwarded_header =
      Plug.Conn.get_req_header(conn, "x-forwarded-for")
      |> List.first()

    ip_address =
      if forwarded_header != nil do
        # IP address behind proxy
        [{"x-forwarded-for", forwarded_header}]
        |> RemoteIp.Headers.parse()
        |> List.first()
        |> ip_address_to_string()
      else
        # IP address of the client
        ip_address_to_string(conn.remote_ip)
      end

    client_hints = extract_client_hints(conn)
    ua_result = UAInspector.parse(user_agent, client_hints)

    # don't track (our) site monitors
    is_site_monitor =
      client_is_site_monitor?(ua_result)

    # don't track local (test) requests
    is_local = ip_address == "127.0.0.1"
    # is_local = false

    if !is_local && !is_site_monitor do
      pathname = conn.request_path
      method = conn.method
      accept_language = Plug.Conn.get_req_header(conn, "accept-language")
      referrer = List.first(Plug.Conn.get_req_header(conn, "referer")) || ""
      referring_domain = domain_from_url(referrer)
      useragent_info = ua_info(ua_result)
      browser = browser_from_ua_info(useragent_info)
      browser_version = browser_version_from_ua_info(useragent_info)
      os = os_from_ua_info(useragent_info)

      tracking_data = {
        method,
        %{
          distinct_id: ip_address,
          event: "$pageview",
          "$current_url": pathname,
          properties: %{
            method: method,
            "$ip": ip_address,
            "$lib": "posthog (Elixir)",
            "$lib_version": "0.1",
            "$referrer": referrer,
            "$referring_domain": referring_domain,
            "$host": conn.host,
            "$pathname": pathname,
            browser: useragent_info,
            "$browser": browser,
            "$browser_version": browser_version,
            "$os": os,
            "accept-language": accept_language
          }
        },
        nil
      }

      # Send tracking data to the buffer
      NostrBackend.PostHogBuffer.add_event(tracking_data)
    end

    # Continue with the request
    conn
  end

  defp domain_from_url(url) do
    case URI.parse(url) do
      %URI{host: host} when is_binary(host) -> host
      _ -> ""
    end
  end

  defp ua_info(%UAInspector.Result.Bot{
         category: category,
         name: name,
         url: url,
         user_agent: user_agent
       }) do
    %{category: category, name: name, url: url, user_agent: user_agent}
  end

  defp ua_info(%UAInspector.Result{
         browser_family: :unknown,
         device: :unknown,
         os: :unknown,
         os_family: :unknown
       }) do
    %{
      client: "unknown"
    }
  end

  defp ua_info(%UAInspector.Result{client: :unknown, user_agent: user_agent}) do
    %{
      client: "unknown",
      user_agent: user_agent
    }
  end

  defp ua_info(%UAInspector.Result{
         browser_family: browser_family,
         client: %UAInspector.Result.Client{
           engine: client_engine,
           engine_version: client_engine_version,
           name: client_name,
           type: client_type,
           version: client_version
         },
         device: %UAInspector.Result.Device{
           brand: device_brand,
           model: device_model,
           type: device_type
         },
         os: %UAInspector.Result.OS{
           name: os_name,
           platform: os_platform,
           version: os_version
         },
         os_family: os_family,
         user_agent: user_agent
       }) do
    %{
      browser_family: browser_family,
      client: %{
        engine: client_engine,
        engine_version: client_engine_version,
        name: client_name,
        type: client_type,
        version: client_version
      },
      device: %{
        brand: device_brand,
        model: device_model,
        type: device_type
      },
      os: %{
        name: os_name,
        platform: os_platform,
        version: os_version
      },
      os_family: os_family,
      user_agent: user_agent
    }
  end

  defp extract_client_hints(conn) do
    Enum.reduce(@client_hint_headers, %{}, fn header, acc ->
      case Plug.Conn.get_req_header(conn, header) |> List.first() do
        nil -> acc
        value -> Map.put(acc, header, value)
      end
    end)
  end

  defp browser_from_ua_info(%{browser_family: browser_family}) do
    browser_family
  end

  defp browser_from_ua_info(_), do: nil

  defp browser_version_from_ua_info(%{client: %{engine_version: client_engine_version}}) do
    client_engine_version
  end

  defp browser_version_from_ua_info(_), do: nil

  defp os_from_ua_info(%{os_family: os_family}) do
    os_family
  end

  defp os_from_ua_info(_), do: nil

  defp client_is_site_monitor?(%UAInspector.Result.Bot{category: "Site Monitor"}), do: true
  defp client_is_site_monitor?(_), do: false

  defp ip_address_to_string({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp ip_address_to_string({a, b, c, d, e, f, g, h}) do
    Enum.join([a, b, c, d, e, f, g, h] |> Enum.map(&Integer.to_string(&1, 16)), ":")
  end
end
