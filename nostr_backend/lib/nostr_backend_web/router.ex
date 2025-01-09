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
    pipe_through([])

    # Catch typical WordPress paths and respond with 404
    get "/wp-admin/*path", NotFoundController, :index
    get "/wp-content/*path", NotFoundController, :index
    get "/wp-includes/*path", NotFoundController, :index
    get "/xmlrpc.php", NotFoundController, :index
    get "/index.php", NotFoundController, :index
  end

  scope "/", NostrBackendWeb do
    pipe_through([:browser, :posthog])

    get("/de", PageController, :landing_page_de)
    get("/en", PageController, :landing_page_en)

    get("/about", PageController, :about)
    get("/c", PageController, :communities)
    get("/search", PageController, :search)
    get("/bookmarks", PageController, :bookmarks)
    get("/media", PageController, :media)
    get("/messages", PageController, :messages)
    get("/notifications", PageController, :notifications)
    get("/posts", PageController, :posts)
    get("/read", PageController, :read)
    get("/sign-in", PageController, :sign_in)
    get("/write", PageController, :write)

    get("/a/:article_id", ContentController, :article)
    get("/e/:event_id", ContentController, :event)
    get("/c/:community_id", ContentController, :community)
    get("/p/:profile_id", ContentController, :profile)
    get("/t/:hashtag", ContentController, :hashtag)
    get("/u/:user_nip05/:identifier", ContentController, :nip05_article)
    get("/u/:user_nip05", ContentController, :user_nip05)
  end

  # Other scopes may use custom stacks.
  scope "/.well-known", NostrBackendWeb do
    get("/nostr.json", NostrController, :nip05)
    get("/nostr/nip96.json", NostrController, :nip96)
  end

  scope "/api", NostrBackendWeb do
    pipe_through :api

    get "/nip05/validate", NostrController, :validate_nip05_handle
    # get "/opengraph", OpenGraphController, :fetch_metadata
    get "/oembed", OembedController, :fetch_oembed
    get "/opengraph/image", OpenGraphController, :fetch_metadata_image
    get "/rumble/embed", RumbleController, :fetch_embed_url
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
