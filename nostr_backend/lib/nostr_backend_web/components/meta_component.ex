defmodule NostrBackendWeb.MetaComponent do
  use NostrBackendWeb, :html

  def render_meta(assigns) do
    ~H"""
    <title><%= @page_title || "NostrBackend" %></title>
    <meta name="description" content={@page_description || "Default description."}>

    <!-- Open Graph -->
    <meta property="og:title" content={@page_title || "NostrBackend"}>
    <meta property="og:description" content={@page_description || "Default description."}>
    <meta property="og:image" content={@page_image || NostrBackendWeb.Endpoint.static_url() <> "/images/default_image.jpg"}>
    <meta property="og:url" content={NostrBackendWeb.Endpoint.url() <> @conn.request_path}>
    <meta property="og:type" content="website">

    <!-- Twitter Card -->
    <meta name="twitter:card" content="summary_large_image">
    <meta name="twitter:title" content={@page_title || "NostrBackend"}>
    <meta name="twitter:description" content={@page_description || "Default description."}>
    <meta name="twitter:image" content={@page_image || NostrBackendWeb.Endpoint.static_url() <> "/images/default_image.jpg"}>
    """
  end
end
