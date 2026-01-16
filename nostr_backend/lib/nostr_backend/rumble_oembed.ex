defmodule NostrBackend.RumbleOembed do
  @moduledoc """
  Helpers for Rumble's oEmbed API (`/api/Media/oembed.json`).

  This is used as a more reliable alternative to HTML scraping when Rumble pages
  are protected by bot/CDN checks.
  """

  @oembed_base "https://rumble.com/api/Media/oembed.json"

  @spec oembed_api_url(String.t()) :: String.t()
  def oembed_api_url(video_page_url) when is_binary(video_page_url) do
    @oembed_base <> "?" <> URI.encode_query(%{"url" => video_page_url})
  end

  @spec embed_src(map()) :: {:ok, String.t()} | :error
  def embed_src(%{"html" => html}) when is_binary(html) do
    # The oEmbed response includes an iframe HTML snippet; we extract its `src`.
    with {:ok, parsed} <- Floki.parse_fragment(html),
         [src | _] when is_binary(src) <- Floki.find(parsed, "iframe") |> Floki.attribute("src"),
         true <- String.trim(src) != "" do
      {:ok, src}
    else
      _ -> :error
    end
  end

  def embed_src(_), do: :error

  @spec thumbnail_url(map()) :: {:ok, String.t()} | :error
  def thumbnail_url(%{"thumbnail_url" => url}) when is_binary(url) and url != "", do: {:ok, url}
  def thumbnail_url(_), do: :error
end
