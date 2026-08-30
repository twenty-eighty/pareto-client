defmodule NostrBackendWeb.WebAuthnControllerTest do
  use NostrBackendWeb.ConnCase, async: false

  setup do
    original = Application.get_env(:nostr_backend, :webauthn_origins)

    on_exit(fn ->
      if original do
        Application.put_env(:nostr_backend, :webauthn_origins, original)
      else
        Application.delete_env(:nostr_backend, :webauthn_origins)
      end
    end)

    :ok
  end

  test "GET /.well-known/webauthn serves default Pareto origins", %{conn: conn} do
    conn = get(conn, "/.well-known/webauthn")

    assert json_response(conn, 200) == %{
             "origins" => ["https://pareto.space", "https://pareto.town"]
           }

    assert get_resp_header(conn, "access-control-allow-origin") == ["*"]
    assert get_resp_header(conn, "content-type") == ["application/json; charset=utf-8"]
  end

  test "GET /.well-known/webauthn uses configured origins", %{conn: conn} do
    Application.put_env(:nostr_backend, :webauthn_origins, [
      "https://pareto.space",
      "https://app.pareto.space"
    ])

    conn = get(conn, "/.well-known/webauthn")

    assert json_response(conn, 200) == %{
             "origins" => ["https://pareto.space", "https://app.pareto.space"]
           }
  end
end
