defmodule NostrBackendWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :nostr_backend

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: []],
    longpoll: [connect_info: []]

  plug NostrBackendWeb.Plugs.Hsts
  plug NostrBackendWeb.Plugs.RequestLogger

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phx.digest
  # when deploying your static files in production.

  plug Plug.Static,
    at: "/",
    from: :nostr_backend,
    gzip: true,
    only: NostrBackendWeb.static_paths()

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug NostrBackendWeb.Router
end
