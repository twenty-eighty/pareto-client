defmodule NostrBackendWeb.StaticFileController do
  use NostrBackendWeb, :controller

  @atom_path Path.join(:code.priv_dir(:nostr_backend), "static/atom")
  @rss_path Path.join(:code.priv_dir(:nostr_backend), "static/rss")
  @sitemap_path Path.join(:code.priv_dir(:nostr_backend), "static/sitemap")

  # Atom feeds
  def atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "feed.xml")
    serve_atom_feed(conn, file_path)
  end

  def en_atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "en_feed.xml")
    serve_atom_feed(conn, file_path)
  end

  def de_atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "de_feed.xml")
    serve_atom_feed(conn, file_path)
  end

  # RSS feeds
  def rss_feed(conn, _params) do
    file_path = Path.join(@rss_path, "feed.xml")
    serve_rss_feed(conn, file_path)
  end

  def en_rss_feed(conn, _params) do
    file_path = Path.join(@rss_path, "en_feed.xml")
    serve_rss_feed(conn, file_path)
  end

  def de_rss_feed(conn, _params) do
    file_path = Path.join(@rss_path, "de_feed.xml")
    serve_rss_feed(conn, file_path)
  end

  def sitemap(conn, _params) do
    file_path = Path.join(@sitemap_path, "sitemap.xml.gz")
    serve_gzipped_sitemap(conn, file_path)
  end

  def year_sitemap(conn, %{"year" => year}) do
    if year =~ ~r/^\d+\.xml\.gz$/ do
      file_path = Path.join(@sitemap_path, "sitemap-#{year}")
      serve_gzipped_sitemap(conn, file_path)
    else
      conn
      |> put_status(404)
      |> text("Sitemap not found")
    end
  end

  def authors_sitemap(conn, _params) do
    file_path = Path.join(@sitemap_path, "sitemap-authors.xml.gz")
    serve_gzipped_sitemap(conn, file_path)
  end

  def landing_sitemap(conn, _params) do
    file_path = Path.join(@sitemap_path, "sitemap-landing.xml.gz")
    serve_gzipped_sitemap(conn, file_path)
  end

  defp serve_gzipped_sitemap(conn, file_path) do
    case File.exists?(file_path) do
      true ->
        conn
        |> put_resp_content_type("application/xml")
        |> put_resp_header("content-encoding", "gzip")
        |> send_file(200, file_path)
      false ->
        conn
        |> put_status(404)
        |> text("Sitemap not found")
    end
  end

  defp serve_atom_feed(conn, file_path) do
    serve_file(conn, file_path, "application/atom+xml")
  end

  defp serve_rss_feed(conn, file_path) do
    serve_file(conn, file_path, "application/rss+xml")
  end

  # Generic file serving with MIME type parameter
  defp serve_file(conn, file_path, content_type) do
    case File.exists?(file_path) do
      true ->
        conn
        |> put_resp_content_type(content_type)
        |> send_file(200, file_path)
      false ->
        conn
        |> put_status(404)
        |> text("File not found")
    end
  end

  # Generic static HTML serving (e.g. google site verification)
  def serve_generic_html(conn, %{"filename" => filename}) do
    # Sanitize to avoid directory traversal
    if filename =~ ~r/^[a-zA-Z0-9_\-]+\.(html|xml|asc)$/ do
      file_path = Path.join(:code.priv_dir(:nostr_backend), "static/" <> filename)
      content_type = case Path.extname(filename) do
        ".html" -> "text/html"
        ".xml" -> "application/xml"
        ".asc" -> "application/pgp-keys"
      end
      serve_file(conn, file_path, content_type)
    else
      conn
      |> put_status(404)
      |> text("Not found")
    end
  end
end
