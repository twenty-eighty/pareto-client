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
    get("/friedenstaube", PageController, :friedenstaube)

    get("/about", PageController, :about)
    get("/authors", PageController, :authors)
    get("/bookmarks", PageController, :bookmarks)
    get("/c", PageController, :communities)
    get("/contact", PageController, :contact)
    get("/imprint", PageController, :imprint)
    get("/internals", PageController, :internals)
    get("/media", PageController, :media)
    get("/messages", PageController, :messages)
    get("/newsletters", PageController, :newsletters)
    get("/notifications", PageController, :notifications)
    get("/pictures", PageController, :pictures)
    get("/posts", PageController, :posts)
    get("/privacy", PageController, :privacy)
    get("/read", PageController, :read)
    get("/search", PageController, :search)
    get("/settings", PageController, :settings)
    get("/sign-in", PageController, :sign_in)
    get("/subscribers", PageController, :subscribers)
    get("/tech-details", PageController, :tech_details)
    get("/uitest", PageController, :uitest)
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
    get "/lnurlp/:username", LightningController, :lnurlp
  end

  # Static atom feeds
  scope "/atom", NostrBackendWeb do
    pipe_through :api

    get "/feed.xml", StaticFileController, :atom_feed
    get "/en_feed.xml", StaticFileController, :en_atom_feed
    get "/de_feed.xml", StaticFileController, :de_atom_feed

    # Only allow feed generation in development and test environments
    if Mix.env() != :prod do
      get "/generate", FeedGenerationController, :generate
    end
  end

  # Static atom feeds
  scope "/rss", NostrBackendWeb do
    pipe_through :api

    get "/feed.xml", StaticFileController, :rss_feed
    get "/en_feed.xml", StaticFileController, :en_rss_feed
    get "/de_feed.xml", StaticFileController, :de_rss_feed
  end

  # Sitemaps
  scope "/sitemap", NostrBackendWeb do
    pipe_through :api

    get "/sitemap.xml.gz", StaticFileController, :sitemap
    get "/sitemap-authors.xml.gz", StaticFileController, :authors_sitemap
    get "/sitemap-landing.xml.gz", StaticFileController, :landing_sitemap
    get "/sitemap-:year", StaticFileController, :year_sitemap
  end

  scope "/api", NostrBackendWeb do
    pipe_through :api

    get "/nip05/validate", NostrController, :validate_nip05_handle
    get "/nip11", Nip11Controller, :fetch_nip11
    # get "/opengraph", OpenGraphController, :fetch_metadata
    get "/oembed", OembedController, :fetch_oembed
    get "/opengraph/image", OpenGraphController, :fetch_metadata_image
    get "/rumble/embed", RumbleController, :fetch_embed_url
  end

  scope "/", NostrBackendWeb do
    pipe_through([:browser])

    # Google site verification
    get "/:filename", StaticFileController, :serve_generic_html
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
