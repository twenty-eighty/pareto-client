defmodule NostrBackendWeb.PwaControllerTest do
  use NostrBackendWeb.ConnCase, async: false

  test "GET /version.json includes current hashed assets", %{conn: conn} do
    conn = get(conn, "/version.json")
    body = json_response(conn, 200)

    assert is_binary(body["gitVersion"])
    assert String.starts_with?(body["js"], "/assets/")
    assert String.starts_with?(body["css"], "/assets/")
    assert get_resp_header(conn, "cache-control") == ["no-cache, no-store, must-revalidate"]
  end

  test "GET /sw.js is javascript with no-cache when present", %{conn: conn} do
    sw_path = Path.join(:code.priv_dir(:nostr_backend), "static/sw.js")

    if File.exists?(sw_path) do
      conn = get(conn, "/sw.js")
      assert response(conn, 200)
      assert get_resp_header(conn, "cache-control") == ["no-cache, no-store, must-revalidate"]
      assert get_resp_header(conn, "service-worker-allowed") == ["/"]
    else
      conn = get(conn, "/sw.js")
      assert response(conn, 404)
    end
  end
end
