defmodule NostrBackendWeb.PageController do
  use NostrBackendWeb, :controller

  alias NostrBackendWeb.Endpoint

  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :home, layout: false)
  end

  def index(conn, _params) do
    render(conn, :index,
      page_title: "The Pareto Project",
      page_description: "Welcome to NostrBackend, your gateway to Nostr content.",
      page_image: Endpoint.static_url() <> "/images/home_image.jpg",
      meta_url: "https://pareto.space",
      meta_title: "The Pareto Project",
      meta_description:
        "An open source publishing ecosystem for uncensorable, citizen journalism powered by Nostr, Lightning and eCash.",
      meta_image: Endpoint.url() <> "/images/pareto-banner.png"
    )
  end

  def search(conn, _params) do
    conn
    |> assign(:page_title, "Search - NostrBackend")
    |> assign(:page_description, "Search through Nostr articles and profiles.")
    |> assign(:page_image, Endpoint.static_url() <> "/images/search_image.jpg")
    |> render(:search)
  end

  def bookmarks(conn, _params) do
    conn
    |> assign(:page_title, "Bookmarks - NostrBackend")
    |> assign(:page_description, "Your saved Nostr articles and profiles.")
    |> assign(:page_image, Endpoint.static_url() <> "/images/bookmarks_image.jpg")
    |> render(:bookmarks)
  end

  def write(conn, _params) do
    conn
    |> assign(:page_title, "Write - NostrBackend")
    |> assign(:page_description, "Compose and publish your Nostr articles.")
    |> assign(:page_image, Endpoint.static_url() <> "/images/write_image.jpg")
    |> render(:write)
  end
end
