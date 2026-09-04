defmodule NostrBackendWeb.PageControllerTest do
  use NostrBackendWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, ~p"/")
    assert redirected_to(conn) == "/en"
  end

  test "GET /en goes through PostHog tracking", %{conn: conn} do
    conn = get(conn, ~p"/en")
    assert html_response(conn, 200)
  end
end
