defmodule NostrBackendWeb.Plugs.RequestLogger do
  import Plug.Conn
  require Logger

  def init(opts), do: opts

  def call(conn, _opts) do
    # Store start time for response timing
    conn = assign(conn, :request_start_time, System.monotonic_time(:microsecond))

    # Add request information to logger metadata
    Logger.metadata(
      method: conn.method,
      path: conn.request_path,
      query_string: conn.query_string,
      request_id: get_req_header(conn, "x-request-id") |> List.first()
    )

    # Log the enhanced request information
    Logger.info(
      "#{conn.method} #{conn.request_path}#{if conn.query_string != "", do: "?#{conn.query_string}", else: ""}"
    )

    # Register a callback to log the response
    register_before_send(conn, &log_response/1)
  end

  defp log_response(conn) do
    # Calculate response time
    response_time = get_response_time(conn)
    Logger.info("Sent #{conn.status} in #{response_time}")
    conn
  end

  defp get_response_time(conn) do
    case conn.assigns[:request_start_time] do
      nil ->
        "~1ms"

      start_time ->
        end_time = System.monotonic_time(:microsecond)
        duration = end_time - start_time

        if duration < 1000 do
          "#{duration}µs"
        else
          "#{Float.round(duration / 1000, 1)}ms"
        end
    end
  end
end
