defmodule NostrBackendWeb.WebAuthnController do
  use NostrBackendWeb, :controller

  @default_origins ["https://pareto.space", "https://pareto.town"]

  def index(conn, _params) do
    origins = Application.get_env(:nostr_backend, :webauthn_origins, @default_origins)

    conn
    |> put_resp_header("access-control-allow-origin", "*")
    |> put_resp_header("access-control-allow-methods", "GET, OPTIONS")
    |> put_resp_header("x-robots-tag", "noindex")
    |> json(%{"origins" => origins})
  end
end
