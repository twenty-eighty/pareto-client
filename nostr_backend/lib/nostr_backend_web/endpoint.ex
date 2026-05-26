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

  # Long-lived static assets
  plug Plug.Static,
    at: "/assets",
    from: {:nostr_backend, "priv/static/assets"},
    brotli: true,
    gzip: true,
    cache_control_for_etags: "public, max-age=31536000, immutable",
    cache_control_for_vsn_requests: "public, max-age=31536000, immutable"

  plug Plug.Static,
    at: "/images",
    from: {:nostr_backend, "priv/static/images"},
    brotli: true,
    gzip: true,
    cache_control_for_etags: "public, max-age=31536000, immutable",
    cache_control_for_vsn_requests: "public, max-age=31536000, immutable"

  plug Plug.Static,
    at: "/fonts",
    from: {:nostr_backend, "priv/static/fonts"},
    brotli: true,
    gzip: true,
    cache_control_for_etags: "public, max-age=31536000, immutable",
    cache_control_for_vsn_requests: "public, max-age=31536000, immutable"

  # Shorter-lived JS (non-hashed helpers under /js)
  plug Plug.Static,
    at: "/js",
    from: {:nostr_backend, "priv/static/js"},
    brotli: true,
    gzip: true,
    cache_control_for_etags: "public, max-age=86400",
    cache_control_for_vsn_requests: "public, max-age=86400"

  plug Plug.Static,
    at: "/",
    from: :nostr_backend,
    brotli: true,
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
