defmodule NostrBackendWeb.Plugs.Hsts do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    put_resp_header(
      conn,
      "strict-transport-security",
      "max-age=31536000; includeSubDomains; preload"
    )
    # Disable XSS filtering
    # see https://stackoverflow.com/questions/9090577/what-is-the-http-header-x-xss-protection
    |> put_resp_header("x-xss-protection", "0")
  end
end
