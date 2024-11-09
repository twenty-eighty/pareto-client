defmodule NostrBackendWeb.Router do
  use NostrBackendWeb, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, html: {NostrBackendWeb.Layouts, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  pipeline :posthog do
    plug NostrBackend.PostHogPlug
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/", NostrBackendWeb do
    pipe_through([:browser])

    get("/", PageController, :landing_page)
  end

  scope "/", NostrBackendWeb do
    pipe_through([:browser, :posthog])

    get("/de", PageController, :landing_page_de)
    get("/en", PageController, :landing_page_en)

    # get("/", PageController, :index)
    get("/c", PageController, :communities)
    get("/search", PageController, :search)
    get("/bookmarks", PageController, :bookmarks)
    get("/read", PageController, :read)
    get("/write", PageController, :write)

    get("/a/:article_id", ContentController, :article)
    get("/e/:event_id", ContentController, :event)
    get("/c/:community_id", ContentController, :community)
    get("/p/:profile_id", ContentController, :profile)
    get("/u/:user_nip05/:identifier", ContentController, :nip05_article)
    get("/u/:user_nip05", ContentController, :user_nip05)
  end

  # Other scopes may use custom stacks.
  scope "/.well-known", NostrBackendWeb do
    get("/nostr.json", NostrController, :nip05)
  end

  # Enable LiveDashboard in development
  if Application.compile_env(:nostr_backend, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through(:browser)

      live_dashboard("/dashboard", metrics: NostrBackendWeb.Telemetry)
    end
  end
end
