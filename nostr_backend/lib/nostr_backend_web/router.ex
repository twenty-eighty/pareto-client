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

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/", NostrBackendWeb do
    pipe_through(:browser)

    # get "/", PageController, :home
    get("/", PageController, :index)
    get("/search", PageController, :search)
    get("/bookmarks", PageController, :bookmarks)
    get("/write", PageController, :write)

    get("/a/:article_id", ContentController, :article)
    get("/p/:profile_id", ContentController, :profile)
    get("/c/:group_id", ContentController, :group)
  end

  # Other scopes may use custom stacks.
  # scope "/api", NostrBackendWeb do
  #   pipe_through :api
  # end

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
