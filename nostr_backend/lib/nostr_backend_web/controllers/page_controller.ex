defmodule NostrBackendWeb.PageController do
  use NostrBackendWeb, :controller

  @meta_title "The Pareto Project"
  @meta_description "An open source publishing platform for uncensorable, investigative journalism powered by Nostr, Lightning and eCash."
  @meta_url "https://pareto.space"
  @sharing_image "/images/pareto-shared.png"

  def landing_page2(conn, _params) do
    redirect(conn, to: ~p"/lp2/index.html")
  end

  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :home, layout: false)
  end

  def index(conn, _params) do
    conn
    |> add_meta_tags

    render(:index)
  end

  def search(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:search)
  end

  def bookmarks(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:bookmarks)
  end

  def read(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:read)
  end

  def write(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:write)
  end

  defp add_meta_tags(conn) do
    conn
    |> assign(:page_title, @meta_title)
    |> assign(:page_description, @meta_description)
    |> assign(:page_image, @sharing_image)
    |> assign(:meta_title, @meta_title)
    |> assign(:meta_description, @meta_description)
    |> assign(:meta_image, @sharing_image)
    |> assign(:meta_url, @meta_url)
  end
end
