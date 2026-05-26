defmodule NostrBackendWeb.Plugs.PreloadFrontendAssets do
  @moduledoc """
  Adds `Link` preload hints for the hashed frontend CSS/JS assets.

  Keeps `root.html.heex` focused on markup while allowing the server to
  advertise critical assets early via response headers.
  """

  import Plug.Conn

  alias NostrBackendWeb.FrontendAssets

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    css = FrontendAssets.css_file()
    js = FrontendAssets.js_file()

    conn
    |> append_link(~s(<https://sdk.feedback.one>; rel=preconnect; crossorigin))
    |> append_link(~s(</styles/styles.css>; rel=preload; as=style))
    |> append_link(~s(<#{css}>; rel=preload; as=style))
    |> append_link(~s(<#{js}>; rel=modulepreload; as=script))
  end

  defp append_link(conn, value) do
    case get_resp_header(conn, "link") do
      [] ->
        put_resp_header(conn, "link", value)

      [existing | _] ->
        put_resp_header(conn, "link", existing <> ", " <> value)
    end
  end
end
