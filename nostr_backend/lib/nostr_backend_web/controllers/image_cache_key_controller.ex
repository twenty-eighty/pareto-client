defmodule NostrBackendWeb.ImageCacheKeyController do
  use NostrBackendWeb, :controller

  def show(conn, _params) do
    conn
    |> put_resp_header("cache-control", "public, max-age=60")
    |> put_resp_header("x-robots-tag", "noindex")
    |> json(NostrBackend.ImageCacheKey.payload())
  end
end
