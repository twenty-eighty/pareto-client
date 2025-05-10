defmodule NostrBackend.RSSGenerator do
  @moduledoc """
  Generates an RSS 2.0 feed from Nostr articles and writes it to priv/static/rss/feed.xml.
  """

  require Logger
  alias NostrBackend.NIP19
  alias NostrBackend.ProfileCache

  @rss_path "priv/static/rss"
  # default feed filename; can be overridden per call
  @default_feed_filename "feed.xml"

  @doc """
  Generate the RSS feed for a list of articles.
  You can override the filename, channel title, and channel description.
  """
  def generate_rss(articles,
    feed_filename \\ @default_feed_filename,
    channel_title \\ "Pareto Articles",
    channel_desc \\ "Articles from Pareto authors"
  ) do
    File.mkdir_p!(@rss_path)
    items = prepare_items(articles)
    xml = build_rss(items, channel_title, channel_desc)
    feed_path = Path.join(@rss_path, feed_filename)
    atomic_write(feed_path, xml)
    Logger.info("Generated RSS feed '#{feed_filename}' with #{length(items)} items at #{feed_path}")
  end

  # Prepare the items for the RSS channel
  defp prepare_items(articles) do
    feed_size = Application.get_env(:nostr_backend, :feed_generator)[:feed_size]
    relay_urls = Application.get_env(:nostr_backend, :feed_generator)[:relay_url] |> List.wrap()

    articles
    |> Enum.sort_by(& &1.published_at, {:desc, DateTime})
    |> Enum.take(feed_size)
    |> Enum.map(fn article ->
      # Get author name from ProfileCache
      author_name = case ProfileCache.get_profile(article.author, []) do
        {:ok, prof} -> prof.display_name || prof.name || article.author
        _ -> article.author
      end
      link = encode_link(article, relay_urls)
      %{
        title: article.title || "Untitled",
        link: link,
        guid: link,
        pubDate: Calendar.strftime(article.published_at || DateTime.utc_now(), "%a, %d %b %Y %H:%M:%S GMT"),
        # only summary in description
        description: article.description || "",
        # full HTML content in content:encoded
        content: article.content || article.description || "",
        image_url: article.image_url,
        author: author_name
      }
    end)
  end

  # Build the RSS XML string
  defp build_rss(items, channel_title, channel_desc) do
    channel_link = base_url() <> "/"
    last_build = List.first(items).pubDate || Calendar.strftime(DateTime.utc_now(), "%a, %d %b %Y %H:%M:%S GMT")

    items_xml =
      items
      |> Enum.map(fn item ->
        # include <media:thumbnail> if image_url is present
        thumbnail_xml = if item.image_url do
          ~s(          <media:thumbnail url="#{item.image_url}" />)
        else
          ""
        end
        """
        <item>
          <title>#{escape(item.title)}</title>
          <link>#{item.link}</link>
          <author>#{escape(item.author)}</author>
          <guid>#{item.guid}</guid>
          <pubDate>#{item.pubDate}</pubDate>
          <description><![CDATA[#{item.description}]]></description>
          <content:encoded><![CDATA[#{item.content}]]></content:encoded>
#{thumbnail_xml}
        </item>
        """
      end)
      |> Enum.join("\n")

    """
<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:media="http://search.yahoo.com/mrss/" xmlns:content="http://purl.org/rss/1.0/modules/content/">
  <channel>
    <title>#{channel_title}</title>
    <link>#{channel_link}</link>
    <description>#{channel_desc}</description>
    <lastBuildDate>#{last_build}</lastBuildDate>
#{items_xml}
  </channel>
</rss>
"""
  end

  # Encode a Nostr article link (naddr) for the RSS item link/guid
  defp encode_link(article, relays) do
    case NIP19.encode_naddr(article.kind, article.author, article.identifier, relays) do
      naddr when is_binary(naddr) -> base_url() <> "/a/" <> naddr
      _ -> base_url() <> "/a/" <> "#{article.kind}:#{article.author}:#{article.identifier}"
    end
  rescue
    _ -> base_url() <> "/a/" <> "#{article.kind}:#{article.author}:#{article.identifier}"
  end

  # Derive the base URL for links
  defp base_url do
    "https://" <> Application.get_env(:nostr_backend, NostrBackendWeb.Endpoint)[:url][:host]
  end

  # Basic XML escaping for RSS titles/descriptions
  defp escape(nil), do: ""
  defp escape(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
  end

  # Atomically write to a temp file then rename for atomic updates
  defp atomic_write(path, contents) do
    tmp = path <> ".tmp"
    File.write!(tmp, contents)
    File.rename!(tmp, path)
  end
end
