defmodule NostrBackendWeb.StaticFileController do
  use NostrBackendWeb, :controller

  @atom_path Path.join(:code.priv_dir(:nostr_backend), "static/atom")
  @sitemap_path Path.join(:code.priv_dir(:nostr_backend), "static/sitemap")

  def atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "feed.xml")
    serve_feed(conn, file_path)
  end

  def en_atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "en_feed.xml")
    serve_feed(conn, file_path)
  end

  def de_atom_feed(conn, _params) do
    file_path = Path.join(@atom_path, "de_feed.xml")
    serve_feed(conn, file_path)
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

  defp serve_feed(conn, file_path) do
    case File.exists?(file_path) do
      true ->
        conn
        |> put_resp_content_type("application/atom+xml")
        |> send_file(200, file_path)
      false ->
        conn
        |> put_status(404)
        |> text("Feed not found")
    end
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
end
