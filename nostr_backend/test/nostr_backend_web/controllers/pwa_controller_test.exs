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

  test "GET /newsletter-sender-sw.js is javascript with no-cache when present", %{conn: conn} do
    script_path = Path.join(:code.priv_dir(:nostr_backend), "static/newsletter-sender-sw.js")
    source = Path.expand("../../../../frontend/static/newsletter-sender-sw.js", __DIR__)
    created = not File.exists?(script_path) and File.exists?(source)

    if created do
      File.mkdir_p!(Path.dirname(script_path))
      File.cp!(source, script_path)
      on_exit(fn -> File.rm(script_path) end)
    end

    if File.exists?(script_path) do
      conn = get(conn, "/newsletter-sender-sw.js")
      assert response(conn, 200) =~ "Newsletter Sender Service Worker"
      assert get_resp_header(conn, "content-type") == ["text/javascript; charset=utf-8"]
      assert get_resp_header(conn, "cache-control") == ["no-cache, no-store, must-revalidate"]
    else
      conn = get(conn, "/newsletter-sender-sw.js")
      assert response(conn, 404)
    end
  end
end
