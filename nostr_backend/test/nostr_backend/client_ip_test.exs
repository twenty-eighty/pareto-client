defmodule NostrBackend.ClientIpTest do
  use ExUnit.Case, async: true

  import Plug.Conn

  test "uses the first forwarded address" do
    conn =
      Plug.Test.conn(:get, "/")
      |> put_req_header("x-forwarded-for", "203.0.113.50, 10.0.0.1")

    assert NostrBackend.ClientIp.from_conn(conn) == "203.0.113.50"
  end

  test "uses the socket address when no proxy header is present" do
    conn = %{Plug.Test.conn(:get, "/") | remote_ip: {198, 51, 100, 8}}
    assert NostrBackend.ClientIp.from_conn(conn) == "198.51.100.8"
  end
end
