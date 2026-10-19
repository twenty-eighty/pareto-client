defmodule NostrBackend.ClientIp do
  @moduledoc """
  The visitor address for this request.

  Behind a proxy, the first address in `x-forwarded-for` is used.
  Otherwise the socket address is used.
  """

  @spec from_conn(Plug.Conn.t()) :: String.t() | nil
  def from_conn(%Plug.Conn{} = conn) do
    case Plug.Conn.get_req_header(conn, "x-forwarded-for") do
      [header | _] ->
        [{"x-forwarded-for", header}]
        |> RemoteIp.Headers.parse()
        |> List.first()
        |> format() || format(conn.remote_ip)

      _ ->
        format(conn.remote_ip)
    end
  end

  defp format({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp format({a, b, c, d, e, f, g, h}) do
    [a, b, c, d, e, f, g, h]
    |> Enum.map(&Integer.to_string(&1, 16))
    |> Enum.join(":")
  end

  defp format(_), do: nil
end
