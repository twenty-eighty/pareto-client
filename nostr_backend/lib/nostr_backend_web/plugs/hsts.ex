defmodule NostrBackendWeb.Plugs.Hsts do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    put_resp_header(
      conn,
      "strict-transport-security",
      "max-age=31536000; includeSubDomains; preload"
    )
  end
end
