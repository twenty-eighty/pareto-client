defmodule NostrBackendWeb.PwaController do
  use NostrBackendWeb, :controller

  @sw_path Path.join(:code.priv_dir(:nostr_backend), "static/sw.js")
  @no_cache "no-cache, no-store, must-revalidate"

  def version(conn, _params) do
    conn
    |> put_resp_header("cache-control", @no_cache)
    |> put_resp_header("x-robots-tag", "noindex")
    |> json(NostrBackendWeb.FrontendAssets.version_payload())
  end

  def service_worker(conn, _params) do
    if File.exists?(@sw_path) do
      conn
      |> put_resp_content_type("text/javascript")
      |> put_resp_header("cache-control", @no_cache)
      |> put_resp_header("service-worker-allowed", "/")
      |> send_file(200, @sw_path)
    else
      conn
      |> put_status(:not_found)
      |> text("Service worker not found")
    end
  end
end
