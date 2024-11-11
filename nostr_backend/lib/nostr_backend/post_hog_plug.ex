defmodule NostrBackend.PostHogPlug do
  def init(options), do: options

  def call(conn, _options) do
    # Capture request information
    ip_address = ip_address_to_string(conn.remote_ip)

    if ip_address != "127.0.0.1" do
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
            path: url
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

  defp ip_address_to_string({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp ip_address_to_string({a, b, c, d, e, f, g, h}) do
    Enum.join([a, b, c, d, e, f, g, h] |> Enum.map(&Integer.to_string(&1, 16)), ":")
  end
end
