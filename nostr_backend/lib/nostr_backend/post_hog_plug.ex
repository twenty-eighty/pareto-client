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
    ip_address = ip_address_to_string(conn.remote_ip)

    user_agent =
      Plug.Conn.get_req_header(conn, "user-agent")
      |> List.first()
      |> IO.inspect(label: "User Agent")

    client_hints = extract_client_hints(conn)
    ua_result = UAInspector.parse(user_agent, client_hints)

    is_site_monitor =
      client_is_site_monitor?(ua_result)

    is_local = ip_address == "127.0.0.1"

    if !is_local && !is_site_monitor do
      url = conn.request_path
      method = conn.method

      tracking_data = {
        method,
        %{
          distinct_id: ip_address,
          event: "#{method} #{url}",
          "$current_url": url,
          "$ip": ip_address,
          "$lib": "posthog",
          properties: %{
            method: method,
            path: url,
            browser: ua_info(ua_result)
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

  defp ua_info(%UAInspector.Result.Bot{
         category: category,
         name: name,
         url: url,
         user_agent: user_agent
       }) do
    %{category: category, name: name, url: url, user_agent: user_agent}
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

  defp client_is_site_monitor?(%UAInspector.Result.Bot{category: "Site Monitor"}), do: true
  defp client_is_site_monitor?(_), do: false

  defp ip_address_to_string({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp ip_address_to_string({a, b, c, d, e, f, g, h}) do
    Enum.join([a, b, c, d, e, f, g, h] |> Enum.map(&Integer.to_string(&1, 16)), ":")
  end
end
