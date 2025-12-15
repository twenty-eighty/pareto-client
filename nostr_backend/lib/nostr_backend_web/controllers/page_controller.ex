defmodule NostrBackendWeb.PageController do
  use NostrBackendWeb, :controller
  require Logger
  alias NostrBackend.ReadFeed
  alias NostrBackendWeb.{Endpoint, EventPayload}

  @meta_title "The Pareto Project"
  @meta_description "An open source publishing platform for uncensorable, investigative journalism powered by Nostr, Lightning and eCash."
  @sharing_image "/images/pareto-shared.png"

  def landing_page(conn, _params) do
    # Determine the preferred language from the `Accept-Language` header
    case NostrBackend.Locale.preferred_language(conn) do
      "de" ->
        conn
        |> put_resp_header("x-robots-tag", "noindex")
        |> redirect(to: "/de")

      # Default to English
      _ ->
        conn
        |> put_resp_header("x-robots-tag", "noindex")
        |> redirect(to: "/en")
    end
  end

  def landing_page_de(conn, _params) do
    user_agent = get_req_header(conn, "user-agent") |> List.first() || "Unknown"
    is_mobile = Browser.mobile?(user_agent)

    file_path =
      if is_mobile do
        "priv/static/lp/de/index_mobile.html"
      else
        "priv/static/lp/de/index.html"
      end

    conn
    |> put_resp_content_type(MIME.from_path(file_path))
    |> send_file(200, file_path)
  end

  def landing_page_en(conn, _params) do
    user_agent = get_req_header(conn, "user-agent") |> List.first() || "Unknown"
    is_mobile = Browser.mobile?(user_agent)

    file_path =
      if is_mobile do
        "priv/static/lp/en/index_mobile.html"
      else
        "priv/static/lp/en/index.html"
      end

    conn
    |> put_resp_content_type(MIME.from_path(file_path))
    |> send_file(200, file_path)
  end

  def friedenstaube(conn, _params) do
    # redirect(conn, to: "/read?category=friedenstaube")
    redirect(conn, to: "/u/friedenstaube@pareto.space")
  end

  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :home, layout: false)
  end

  def index(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:index)
  end

  def search(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:search)
  end

  def about(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:about)
  end

  def authors(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:authors)
  end

  def bookmarks(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:bookmarks)
  end

  def contact(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:contact)
  end

  def imprint(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:imprint)
  end

  def internals(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:internals)
  end

  def media(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:media)
  end

  def messages(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:messages)
  end

  def newsletters(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:newsletters)
  end

  def notifications(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:notifications)
  end

  def communities(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:communities)
  end

  def pictures(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:pictures)
  end

  def posts(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:posts)
  end

  def privacy(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:privacy)
  end

  def read(conn, _params) do
    conn = add_meta_tags(conn)

    {articles, payload} =
      case ReadFeed.latest(4) do
        {:ok, feed} ->
          extras = %{authors: feed.authors}
          {feed.articles, EventPayload.encode(feed.events, extras)}

        {:error, reason} ->
          Logger.warning("Read feed unavailable: #{inspect(reason)}")
          {[], nil}
      end

    conn
    |> assign(:read_feed_articles, articles)
    |> assign(:nostr_event_json, payload)
    |> render(:read)
  end

  def settings(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:settings)
  end

  def subscribers(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:subscribers)
  end

  def sign_in(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:sign_in)
  end

  def tech_details(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:tech_details)
  end

  def uitest(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:uitest)
  end

  def write(conn, _params) do
    conn
    |> add_meta_tags
    |> render(:write)
  end

  defp add_meta_tags(conn) do
    lang = NostrBackend.Locale.preferred_language(conn)
    current_url = Endpoint.url() <> conn.request_path

    # Prepare schema.org metadata for static pages
    schema_metadata = %{
      "@context" => "https://schema.org",
      "@type" => "WebPage",
      "url" => current_url,
      "name" => @meta_title,
      "description" => @meta_description,
      "publisher" => %{
        "@type" => "Organization",
        "name" => "Pareto Project",
        "url" => Endpoint.url()
      }
    }

    conn
    |> assign(:lang, lang)
    |> assign(:page_title, @meta_title)
    |> assign(:page_description, @meta_description)
    |> assign(:page_image, @sharing_image)
    |> assign(:meta_title, @meta_title)
    |> assign(:meta_description, @meta_description)
    |> assign(:meta_image, @sharing_image)
    |> assign(:meta_url, current_url)
    |> assign(:canonical_url, current_url)
    |> assign(:schema_metadata, Jason.encode!(schema_metadata))
  end
end
