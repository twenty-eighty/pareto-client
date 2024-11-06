defmodule NostrBackendWeb.PageController do
  use NostrBackendWeb, :controller

  @meta_title "The Pareto Project"
  @meta_description "An open source publishing platform for uncensorable, investigative journalism powered by Nostr, Lightning and eCash."
  @meta_url "https://pareto.space"
  @sharing_image "/images/pareto-shared.png"

  def landing_page(conn, _params) do
    # Determine the preferred language from the `Accept-Language` header
    case get_preferred_language(conn) do
      "en" -> redirect(conn, to: ~p"/lp/en/index.html")
      "de" -> redirect(conn, to: ~p"/lp/de/index.html")
      # Default to English
      _ -> redirect(conn, to: ~p"/lp/en/index.html")
    end
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

  defp get_preferred_language(conn) do
    # Parse `Accept-Language` to get the primary language code
    conn
    |> get_req_header("accept-language")
    |> List.first()
    |> parse_language()
  end

  # Default if no language is provided
  defp parse_language(nil), do: "en"

  defp parse_language(lang_header) do
    lang_header
    # Handle multiple languages
    |> String.split(",")
    # Take the first language
    |> List.first()
    # Split language and region (e.g., "en-US")
    |> String.split("-")
    # Use the main language code
    |> List.first()
  end
end
