defmodule NostrBackend.FeedGenerator do
  use GenServer
  require Logger

  alias NostrBackend.FollowListCache
  alias NostrBackend.ArticleCache
  alias Sitemapper
  alias NostrBackend.RSSGenerator

  @moduledoc """
  Generates Atom feeds and sitemaps for articles from followed authors.
  """

  # Configuration
  @regeneration_interval :timer.hours(1)

  # File paths
  @base_path "priv/static"
  @atom_path "#{@base_path}/atom"
  @sitemap_path "#{@base_path}/sitemap"

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_) do
    # Create necessary directories
    File.mkdir_p!(@atom_path)
    File.mkdir_p!(@sitemap_path)

    # Schedule initial generation
    Process.send_after(self(), :generate, 0)
    # Schedule periodic regeneration
    schedule_regeneration()

    {:ok, %{}}
  end

  def handle_info(:generate, state) do
    Logger.info("Starting feed and sitemap generation")
    generate_all()
    {:noreply, state}
  end

  def handle_info(:regenerate, state) do
    Logger.info("Regenerating feeds and sitemaps")
    generate_all()
    schedule_regeneration()
    {:noreply, state}
  end

  def handle_info({:event, _subscription_id, _event}, state) do
    # Ignore WebSocket events as they are handled by NostrClient
    {:noreply, state}
  end

  def handle_info({:eose, _subscription_id}, state) do
    # Start feed generation after receiving EOSE
    Logger.info("Received EOSE, starting feed generation")
    generate_all()
    {:noreply, state}
  end

  defp schedule_regeneration do
    Process.send_after(self(), :regenerate, @regeneration_interval)
  end

  defp generate_all do
    source_pubkey = Application.get_env(:nostr_backend, :feed_generator)[:source_pubkey]
    Logger.info("Using source pubkey: #{source_pubkey}")

    with {:ok, follow_list} <- FollowListCache.get_follow_list(source_pubkey) do
      Logger.info("Got follow list with #{length(follow_list)} authors")
      Logger.debug("Follow list: #{inspect(follow_list)}")

      if length(follow_list) > 0 do
        with {:ok, articles} <- fetch_all_articles(follow_list) do
          Logger.info("Fetched #{length(articles)} articles")
          generate_atom_feeds(articles)
          generate_sitemaps(articles)
        else
          {:error, reason} ->
            Logger.error("Failed to generate feeds and sitemaps: #{inspect(reason)}")
        end
      else
        Logger.info("Follow list is empty, skipping feed generation")
      end
    else
      {:error, reason} ->
        Logger.error("Failed to get follow list: #{inspect(reason)}")
    end
  end

  defp get_configured_relay do
    Application.get_env(:nostr_backend, :feed_generator)[:relay_url] || "wss://nostr.pareto.space"
  end

  defp fetch_all_articles(follow_list) do
    relay = get_configured_relay()
    Logger.info("Fetching articles from relay #{relay} for #{length(follow_list)} authors")

    case ArticleCache.get_multiple_authors_articles(follow_list, [relay]) do
      {:ok, articles} ->
        Logger.info("Successfully fetched #{length(articles)} articles")
        if length(articles) > 0 do
          Logger.debug("Sample article: #{inspect(hd(articles))}")
        end
        {:ok, articles}
      {:error, reason} ->
        Logger.error("Failed to fetch articles: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp generate_atom_feeds(articles) do
    # Generate main feed with all articles
    generate_main_feed(articles)

    # Generate language-specific feeds
    generate_language_feed(articles, "en", "English Articles")
    generate_language_feed(articles, "de", "German Articles")

    # Generate RSS feed for all articles
    RSSGenerator.generate_rss(articles, "feed.xml", "Pareto Articles", "Articles from Pareto authors")
  end

  defp generate_language_feed(articles, language_code, feed_title) do
    feed_size = Application.get_env(:nostr_backend, :feed_generator)[:feed_size]
    Logger.debug("Generating #{language_code} language feed")

    # Filter articles by language tag
    language_articles = filter_articles_by_language(articles, language_code)
    Logger.debug("Found #{length(language_articles)} articles with language #{language_code}")

    # Skip if no articles found for this language
    if length(language_articles) > 0 do
      # Sort by published_at
      sorted_articles =
        language_articles
        |> Enum.sort_by(fn a -> a.published_at end, {:desc, DateTime})
        |> Enum.take(feed_size)

      most_recent_date = if length(sorted_articles) > 0 do
        hd(sorted_articles).published_at
      else
        DateTime.utc_now()
      end

      # Get proper language names and titles based on language code
      {title, subtitle} = case language_code do
        "en" ->
          {"Pareto English Articles", "English articles from Pareto authors"}
        "de" ->
          {"Pareto Deutsche Artikel", "Deutsche Artikel von Pareto-Autoren"}
        _ ->
          {"Pareto #{feed_title}", "#{feed_title} from Pareto authors"}
      end

      # Create language specific feed
      feed_id = "#{base_url()}/atom/#{language_code}_feed.xml"

      # Create entries
      entries = Enum.map(sorted_articles, fn article ->
        case article_to_feed_entry(article) do
          nil -> nil
          entry -> entry
        end
      end)
      |> Enum.reject(&is_nil/1)

      # Generate XML and write to file
      xml = Atomex.Feed.new(feed_id, most_recent_date, title)
      |> Atomex.Feed.subtitle(subtitle)
      |> Atomex.Feed.link(feed_id, rel: "self", type: "application/atom+xml")
      |> Atomex.Feed.link("#{base_url()}/", rel: "alternate", type: "text/html")
      |> Atomex.Feed.entries(entries)
      |> Atomex.Feed.build(%{"xmlns:media" => "http://search.yahoo.com/mrss/"})
      |> Atomex.generate_document()
      atomic_write("#{@atom_path}/#{language_code}_feed.xml", xml)
      Logger.info("Generated language feed for #{language_code} with #{length(sorted_articles)} articles")

      # Generate RSS feed for this language
      rss_filename = "#{language_code}_feed.xml"
      RSSGenerator.generate_rss(sorted_articles, rss_filename, title, subtitle)
    else
      Logger.info("No articles found for language #{language_code}, skipping feed generation")
    end
  end

  defp filter_articles_by_language(articles, language_code) do
    Enum.filter(articles, fn article ->
      case get_language_tag(article) do
        {:ok, lang} -> lang == language_code
        _ -> false
      end
    end)
  end

  defp get_language_tag(article) do
    # Get tags from atom key
    tags = article.tags || []

    Logger.debug("Article tags: #{inspect(tags)}")

    # Look for language tag in the format ["l", "en"] or ["l", "de"]
    case Enum.find(tags, fn tag ->
      is_list(tag) && length(tag) >= 2 && List.first(tag) == "l"
    end) do
      ["l", lang | _] ->
        Logger.debug("Found language tag: #{lang}")
        {:ok, lang}
      _ ->
        Logger.debug("No language tag found")
        {:error, :no_language_tag}
    end
  end

  defp generate_main_feed(articles) do
    feed_size = Application.get_env(:nostr_backend, :feed_generator)[:feed_size]
    Logger.debug("Generating main feed with #{length(articles)} articles")

    # Sort by published_at to find most recent article date
    sorted_articles = articles
      |> Enum.sort_by(fn a -> a.published_at end, {:desc, DateTime})
      |> Enum.take(feed_size)

    most_recent_date = if length(sorted_articles) > 0 do
      hd(sorted_articles).published_at
    else
      DateTime.utc_now()
    end

    entries = sorted_articles
      |> Enum.map(&article_to_feed_entry/1)
      |> Enum.reject(&is_nil/1)

    Logger.debug("Generated #{length(entries)} feed entries")

    feed_id = "#{base_url()}/atom/feed.xml"
    xml = Atomex.Feed.new(feed_id, most_recent_date, "Pareto Articles")
    |> Atomex.Feed.subtitle("Articles from Pareto authors")
    |> Atomex.Feed.link(feed_id, rel: "self", type: "application/atom+xml")
    |> Atomex.Feed.link("#{base_url()}/", rel: "alternate", type: "text/html")
    |> Atomex.Feed.entries(entries)
    |> Atomex.Feed.build(%{"xmlns:media" => "http://search.yahoo.com/mrss/"})
    |> Atomex.generate_document()
    atomic_write("#{@atom_path}/feed.xml", xml)
  end

  defp article_to_feed_entry(article) do
    Logger.debug("Processing article: #{inspect(article)}")

    # Get author from atom key
    author = article.author
    Logger.debug("Author from article: #{inspect(author)}")

    # Skip articles with nil author
    if is_nil(author) do
      Logger.debug("Skipping article with nil author")
      nil
    else
      # Get author profile using default relay list
      author_profile = case NostrBackend.ProfileCache.get_profile(author, []) do
        {:ok, profile} ->
          Logger.debug("Got author profile: #{inspect(profile)}")
          profile
        _ ->
          Logger.debug("No profile found for author #{author}")
          %{}
      end

      # Get author name (display_name -> name -> pubkey)
      author_name = cond do
        is_map(author_profile) && Map.get(author_profile, :display_name) ->
          Logger.debug("Using display_name: #{author_profile.display_name}")
          author_profile.display_name
        is_map(author_profile) && Map.get(author_profile, :name) ->
          Logger.debug("Using name: #{author_profile.name}")
          author_profile.name
        true ->
          Logger.debug("Using pubkey as fallback: #{author}")
          author
      end

      # Create NIP-19 identifier
      kind = article.kind
      identifier = article.identifier

      # Get the relay used to fetch articles
      relay = get_configured_relay()
      Logger.debug("Using relay for naddr: #{relay}")

      # Compute canonical entry ID (no-relay) or fallback to NIP-05 route
      entry_id_url = case NostrBackend.NIP19.encode_naddr(kind, author, identifier) do
        naddr when is_binary(naddr) -> "#{base_url()}/a/#{naddr}"
        _ ->
          nip05_id = Map.get(author_profile, :nip05)
          "#{base_url()}/u/#{nip05_id}/#{identifier}"
      end

      # Compute alternate link with relay if possible
      entry_link_url = case NostrBackend.NIP19.encode_naddr(kind, author, identifier, [relay]) do
        naddr when is_binary(naddr) -> "#{base_url()}/a/#{naddr}"
        _ -> entry_id_url
      end

      # Get the timestamp for updated
      updated_at = article.published_at || article.created_at || DateTime.utc_now()
      Logger.debug("Using timestamp: #{inspect(updated_at)}")

      # Get title
      title = article.title || "Untitled"

      # Get content
      content = article.content || ""

      # Get article description/summary
      description = article.description
      Logger.debug("Article description: #{inspect(description)}")

      # Create the feed entry using Atomex.Entry
      entry = Atomex.Entry.new(
        entry_id_url,
        updated_at,
        title
      )
      |> Atomex.Entry.author(author_name)
      |> Atomex.Entry.content(content, type: "html")
      |> Atomex.Entry.link(entry_link_url, rel: "alternate", type: "text/html")

      # Add image as enclosure if present, with MIME type derived from extension
      entry = if article.image_url do
        Atomex.Entry.add_field(entry, :"media:thumbnail", %{url: article.image_url}, nil)
      else
        entry
      end

      # Add summary if present
      entry =
        if description do
          entry |> Atomex.Entry.summary(description)
        else
          entry
        end

      # Build the entry so it's wrapped in <entry> tags
      entry = Atomex.Entry.build(entry)

      Logger.debug("Created feed entry with author: #{author_name}")
      entry
    end
  end

  defp generate_sitemaps(articles) do
    Logger.info("Starting sitemap generation with #{length(articles)} articles")
    source_pubkey = Application.get_env(:nostr_backend, :feed_generator)[:source_pubkey]

    with {:ok, follow_list} <- FollowListCache.get_follow_list(source_pubkey) do
      Logger.info("Got follow list with #{length(follow_list)} authors")

      # Group articles by year
      articles_by_year = group_articles_by_year(articles)

      if map_size(articles_by_year) == 0 do
        Logger.warning("No articles to generate sitemaps for")
        :ok
      else
        # Create empty sitemaps directory
        File.rm_rf!(@sitemap_path)
        File.mkdir_p!(@sitemap_path)
        Logger.info("Created sitemap directory at #{@sitemap_path}")

        # Generate year-based sitemaps
        year_sitemaps = Enum.map(articles_by_year, fn {year, year_articles} ->
          Logger.info("Processing year #{year} with #{length(year_articles)} articles")

          # Sort articles to find the most recent one
          sorted_articles = Enum.sort_by(year_articles, fn article ->
            article.published_at
          end, {:desc, DateTime})

          most_recent_article = List.first(sorted_articles)
          most_recent_date = most_recent_article.published_at
          Logger.info("Most recent article date for year #{year}: #{inspect(most_recent_date)}")

          # Generate sitemap filename with the year
          filename = "sitemap-#{year}"
          sitemap_path = "#{@sitemap_path}/#{filename}.xml"
          Logger.info("Generating sitemap with filename: #{filename}")

          # Get the relay used to fetch articles
          relay = get_configured_relay()
          Logger.info("Using relay for sitemap: #{relay}")

          # Create XML sitemap content
          urls_xml = Enum.map(year_articles, fn article ->
            # Get the necessary fields for the naddr
            kind = article.kind
            author = article.author
            identifier = article.identifier

            Logger.debug("Creating sitemap entry for article: kind=#{kind}, author=#{author}, identifier=#{identifier}")

            # Create naddr with relay information included
            naddr = NostrBackend.NIP19.encode_naddr(kind, author, identifier, [relay])

            # Format the lastmod date in W3C format (YYYY-MM-DD)
            lastmod = Calendar.strftime(article.published_at, "%Y-%m-%d")

            ~s(  <url>
    <loc>#{base_url()}/a/#{naddr}</loc>
    <lastmod>#{lastmod}</lastmod>
    <changefreq>monthly</changefreq>
    <priority>0.8</priority>
  </url>)
          end) |> Enum.join("\n")

          # Create complete sitemap XML
          sitemap_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
#{urls_xml}
</urlset>)

          # Write the XML file
          atomic_write(sitemap_path, sitemap_content)
          Logger.info("Wrote sitemap file: #{sitemap_path}")

          # Compress the XML file
          :os.cmd(~c"gzip -f #{sitemap_path}")
          Logger.info("Compressed sitemap file: #{sitemap_path}.gz")

          # Add to our sitemap index list
          %{
            loc: "#{base_url()}/sitemap/#{filename}.xml.gz",
            lastmod: most_recent_date,
            year: year
          }
        end)

        # Generate the sitemap index file that references all year sitemaps
        generate_sitemap_index(year_sitemaps)

        # Generate author sitemap
        generate_author_sitemap(follow_list)
      end
    else
      {:error, reason} ->
        Logger.error("Failed to get follow list: #{inspect(reason)}")
    end
  end

  defp generate_sitemap_index(year_sitemaps) do
    Logger.info("Generating sitemap index with entries: #{inspect(year_sitemaps)}")

    # Format the lastmod dates in W3C format (YYYY-MM-DD)
    sitemap_entries = Enum.map(year_sitemaps, fn sitemap ->
      lastmod = Calendar.strftime(sitemap.lastmod, "%Y-%m-%d")
      ~s(  <sitemap>
    <loc>#{sitemap.loc}</loc>
    <lastmod>#{lastmod}</lastmod>
  </sitemap>)
    end) |> Enum.join("\n")

    # Create sitemap index XML
    sitemap_index_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<sitemapindex xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/siteindex.xsd" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
#{sitemap_entries}
</sitemapindex>)

    # Write the index file
    sitemap_index_path = "#{@sitemap_path}/sitemap.xml"
    atomic_write(sitemap_index_path, sitemap_index_content)

    # Compress the index file
    :os.cmd(~c"gzip -f #{sitemap_index_path}")

    Logger.info("Sitemap index generated successfully")

    # Update robots.txt to point to the new sitemap index
    generate_robots_txt("#{base_url()}/sitemap/sitemap.xml.gz")
  end

  defp generate_robots_txt(sitemap_url) do
    robots_txt = """
    User-agent: *
    Allow: /
    Disallow: /api/
    Sitemap: #{sitemap_url}
    """

    Logger.info("Generating robots.txt file pointing to #{sitemap_url}")
    atomic_write("#{@base_path}/robots.txt", robots_txt)
  end

  defp generate_author_sitemap(follow_list) do
    Logger.info("Generating author sitemap with #{length(follow_list)} authors")

    # Get the relay used to fetch articles
    relay = get_configured_relay()
    Logger.info("Using relay for author sitemap: #{relay}")

    # Create author sitemap XML content
    urls_xml = Enum.map(follow_list, fn pubkey ->
      nprofile = NostrBackend.NIP19.encode_nprofile(pubkey, [relay])
      Logger.debug("Generated author nprofile: #{nprofile}")

      author_url =
        "/p/#{nprofile}"

      ~s(  <url>
    <loc>#{base_url()}#{author_url}</loc>
    <changefreq>weekly</changefreq>
    <priority>0.7</priority>
  </url>)
    end) |> Enum.join("\n")

    # Create complete sitemap XML
    sitemap_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
#{urls_xml}
</urlset>)

    # Write the XML file
    sitemap_path = "#{@sitemap_path}/sitemap-authors.xml"
    atomic_write(sitemap_path, sitemap_content)

    # Compress the XML file
    :os.cmd(~c"gzip -f #{sitemap_path}")

    # Update the sitemap index to include the authors sitemap
    update_sitemap_index_with_authors()

    Logger.info("Author sitemap generated successfully")
  end

  defp update_sitemap_index_with_authors() do
    sitemap_index_path = "#{@sitemap_path}/sitemap.xml"

    if File.exists?("#{sitemap_index_path}.gz") do
      # Decompress and read the existing sitemap index
      :os.cmd(~c"gunzip -f #{sitemap_index_path}.gz")

      sitemap_index = File.read!(sitemap_index_path)

      # Only add authors sitemap if it's not already included
      unless String.contains?(sitemap_index, "sitemap-authors.xml.gz") do
        # Insert author sitemap entry before the closing sitemapindex tag
        today = Calendar.strftime(DateTime.utc_now(), "%Y-%m-%d")
        author_entry = ~s(  <sitemap>
    <loc>#{base_url()}/sitemap/sitemap-authors.xml.gz</loc>
    <lastmod>#{today}</lastmod>
  </sitemap>
)
        new_sitemap_index = String.replace(sitemap_index, "</sitemapindex>", "#{author_entry}</sitemapindex>")

        # Write the updated sitemap index
        atomic_write(sitemap_index_path, new_sitemap_index)

        # Compress the updated index
        :os.cmd(~c"gzip -f #{sitemap_index_path}")

        Logger.info("Added author sitemap to sitemap index")
      else
        # Recompress the unchanged index
        :os.cmd(~c"gzip -f #{sitemap_index_path}")
      end
    else
      Logger.warning("Sitemap index not found, skipping author sitemap addition to index", [])
    end
  end

  defp group_articles_by_year(articles) do
    Logger.info("Grouping #{length(articles)} articles by year")

    articles
    |> Enum.filter(fn article ->
      case article.published_at do
        nil ->
          Logger.warning("Article has nil published_at: #{inspect(article)}")
          false
        published_at when is_struct(published_at, DateTime) ->
          true
        _ ->
          Logger.warning("Article has invalid published_at format: #{inspect(article.published_at)}")
          false
      end
    end)
    |> Enum.group_by(fn article ->
      article.published_at.year
    end)
    |> tap(fn grouped ->
      Logger.info("Grouped articles by year: #{inspect(Map.keys(grouped))}")
      Enum.each(grouped, fn {year, articles} ->
        Logger.info("Year #{year} has #{length(articles)} articles")
      end)
    end)
  end

  defp base_url do
    "https://#{Application.get_env(:nostr_backend, NostrBackendWeb.Endpoint)[:url][:host]}"
  end

  # Atomically write to a temp file then rename to ensure atomic filesystem updates
  defp atomic_write(path, contents) do
    tmp = path <> ".tmp"
    File.write!(tmp, contents)
    File.rename!(tmp, path)
  end
end
