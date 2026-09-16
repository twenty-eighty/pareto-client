defmodule NostrBackendWeb.Plugs.NoCacheHtml do
  @moduledoc """
  Prevents browsers from keeping a stale HTML document that points at old
  hashed JS/CSS after a deploy.
  """

  import Plug.Conn

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    put_resp_header(conn, "cache-control", "no-cache")
  end
end
