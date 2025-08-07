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
    feed_size = Application.get_env(:nostr_backend, :feed_generator)[:feed_size]
    relay_urls = Application.get_env(:nostr_backend, :feed_generator)[:relay_url] |> List.wrap()
    items = prepare_items(articles, feed_size, relay_urls)
    xml = build_rss(items, channel_title, channel_desc, feed_filename)
    feed_path = Path.join(@rss_path, feed_filename)
    atomic_write(feed_path, xml)
    Logger.info("Generated RSS feed '#{feed_filename}' with #{length(items)} items at #{feed_path}")
  end

  # Prepare the items for the RSS channel
  defp prepare_items(articles, feed_size, relay_urls) do
    articles
    |> Enum.sort_by(& &1.published_at, {:desc, DateTime})
    |> Enum.take(feed_size)
    |> Enum.map(fn article ->
      # Get author name from ProfileCache with robust fallback
      author_name = case ProfileCache.get_profile(article.author, []) do
        {:ok, prof} when is_map(prof) and map_size(prof) > 1 ->
          # Profile has meaningful data (more than just relays)
          prof.display_name || prof.name || article.author
        {:ok, _prof} ->
          # Profile is incomplete (only has relays or is empty)
          article.author
        _ ->
          # Profile fetch failed
          article.author
      end

      # Link with relay for RSS <link>
      link = encode_link(article, relay_urls)

      # Compute guid: canonical naddr without relay or fallback
      guid = case NIP19.encode_naddr(article.kind, article.author, article.identifier) do
        naddr when is_binary(naddr) -> base_url() <> "/a/" <> naddr
        _ ->
          nip05_id = case ProfileCache.get_profile(article.author, []) do
            {:ok, prof} when is_map(prof) and map_size(prof) > 1 -> prof.nip05
            _ -> nil
          end
          base_url() <> "/u/" <> (nip05_id || article.author) <> "/" <> to_string(article.identifier)
      end

      # Prepare description and content with inline title image
      description = article.description || ""
      raw_content = article.content || description
      content = if article.image_url do
        img_tag = "<p><img src=\"#{article.image_url}\" /></p>"
        img_tag <> raw_content
      else
        raw_content
      end

      %{
        title: article.title || "Untitled",
        link: link,
        guid: guid,
        pubDate: Calendar.strftime(article.published_at || DateTime.utc_now(), "%a, %d %b %Y %H:%M:%S GMT"),
        description: description,
        content: content,
        image_url: article.image_url,
        author: author_name
      }
    end)
  end

  # Build the RSS XML string, including a self <atom:link>, passing the feed filename
  defp build_rss(items, channel_title, channel_desc, feed_filename) do
    channel_link = base_url() <> "/"
    # Self URL for this RSS feed
    self_link = base_url() <> "/rss/" <> feed_filename
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
          <dc:creator>#{escape(item.author)}</dc:creator>
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
<rss version="2.0" xmlns:media="http://search.yahoo.com/mrss/" xmlns:content="http://purl.org/rss/1.0/modules/content/" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:dc="http://purl.org/dc/elements/1.1/">
  <channel>
    <atom:link href="#{self_link}" rel="self" type="application/rss+xml" />
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
