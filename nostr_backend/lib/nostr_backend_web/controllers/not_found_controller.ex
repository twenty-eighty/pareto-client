defmodule NostrBackendWeb.NotFoundController do
  use NostrBackendWeb, :controller

  def index(conn, _params) do
    send_resp(conn, 404, "Not Found")
  end
end
