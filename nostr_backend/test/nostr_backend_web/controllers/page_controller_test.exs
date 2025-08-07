defmodule NostrBackendWeb.PageControllerTest do
  use NostrBackendWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, ~p"/")
    assert redirected_to(conn) == "/en"
  end
end
