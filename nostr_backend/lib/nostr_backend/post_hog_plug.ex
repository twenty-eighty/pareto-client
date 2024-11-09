defmodule NostrBackend.PostHogPlug do
  def init(options), do: options

  def call(conn, _options) do
    # Capture request information
    ip_address = ip_address_to_string(conn.remote_ip)

    if ip_address != "127.0.0.1" do
      url = conn.request_path
      method = conn.method

      tracking_data = %{
        distinct_id: ip_address,
        event: "#{method} #{url}",
        "$current_url": url,
        "$ip": ip_address,
        "$lib": "posthog",
        properties: %{
          method: method
        }
      }

      # Send tracking data to the buffer
      NostrBackend.PostHogBuffer.add_event(tracking_data)
    end

    # Continue with the request
    conn
  end

  defp ip_address_to_string(ip_address) do
    ip_address
    |> Tuple.to_list()
    |> Enum.join(".")
  end
end
