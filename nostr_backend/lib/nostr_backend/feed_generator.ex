defmodule NostrBackend.FeedGenerator do
  use GenServer
  require Logger

  alias NostrBackend.FollowListCache
  alias NostrBackend.ArticleCache
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
    Logger.warn("FeedGenerator GenServer starting (this should only happen once unless there is a crash or code reload)")
    Process.flag(:trap_exit, true)
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
    Logger.info("Starting feed and sitemap generation (manual or scheduled)")
    generate_all()
    {:noreply, state}
  end

  def handle_info(:regenerate, state) do
    Logger.info("Regenerating feeds and sitemaps (scheduled)")
    generate_all()
    schedule_regeneration()
    {:noreply, state}
  end

  def handle_info({:event, _subscription_id, _event}, state) do
    # Ignore WebSocket events as they are handled by NostrClient
    {:noreply, state}
  end

  def handle_info({:eose, _subscription_id}, state) do
    # Ignore EOSE (end of stream) events; do not trigger feed generation
    Logger.info("Received EOSE (ignored)")
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.error("Linked process #{inspect(pid)} exited: #{inspect(reason)} (ignored, FeedGenerator will not crash)")
    {:noreply, state}
  end

  defp schedule_regeneration do
    Process.send_after(self(), :regenerate, @regeneration_interval)
  end

  defp generate_all do
    try do
      source_pubkey = Application.get_env(:nostr_backend, :feed_generator)[:source_pubkey]
      Logger.info("Using source pubkey: #{source_pubkey}")

      with {:ok, follow_list} <- safe_follow_list(source_pubkey) do
        Logger.info("Got follow list with #{length(follow_list)} authors")
        Logger.debug("Follow list: #{inspect(follow_list)}")

        if length(follow_list) > 0 do
          case safe_fetch_all_articles(follow_list) do
            {:ok, articles} ->
              Logger.info("Fetched #{length(articles)} articles")
              generate_atom_feeds(articles)
              generate_sitemaps(articles)
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
    rescue
      e ->
        Logger.error("Exception in generate_all: #{inspect(e)}\n" <> Exception.format(:error, e, __STACKTRACE__))
    end
  end

  defp safe_follow_list(source_pubkey) do
    try do
      FollowListCache.get_follow_list(source_pubkey)
    rescue
      e ->
        Logger.error("Exception in FollowListCache.get_follow_list: #{inspect(e)}\n" <> Exception.format(:error, e, __STACKTRACE__))
        {:error, e}
    end
  end

  defp safe_fetch_all_articles(follow_list) do
    try do
      fetch_all_articles(follow_list)
    rescue
      e ->
        Logger.error("Exception in fetch_all_articles: #{inspect(e)}\n" <> Exception.format(:error, e, __STACKTRACE__))
        {:error, e}
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
      # Prepend title image to content if present
      content = if article.image_url do
        img_tag = "<p><img src=\"#{article.image_url}\" alt=\"#{title}\" /></p>"
        img_tag <> content
      else
        content
      end

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

          # Create XML sitemap content (sorted by published_at descending) with language alternates
          urls_xml = Enum.map(sorted_articles, fn article ->
            kind = article.kind
            author = article.author
            identifier = article.identifier
            Logger.debug("Creating sitemap entry for article: kind=#{kind}, author=#{author}, identifier=#{identifier}")
            naddr = NostrBackend.NIP19.encode_naddr(kind, author, identifier, [relay])
            lastmod = Calendar.strftime(article.published_at, "%Y-%m-%d")
            path = "/a/#{naddr}"
            alternates_html = alternate_links(path)
            ~s(  <url>
    <loc>#{base_url()}#{path}</loc>
#{alternates_html}
    <lastmod>#{lastmod}</lastmod>
    <changefreq>monthly</changefreq>
    <priority>0.8</priority>
  </url>)
          end) |> Enum.join("\n")

          # Create complete sitemap XML with xhtml namespace
          sitemap_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xhtml="http://www.w3.org/1999/xhtml">
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

        # Generate author and landing sitemap entries
        author_entry  = generate_author_sitemap(follow_list)
        landing_entry = generate_landing_sitemap()
        # Build and write combined sitemap index
        all_sitemaps = year_sitemaps ++ [author_entry, landing_entry]
        generate_sitemap_index(all_sitemaps)
      end
    else
      {:error, reason} ->
        Logger.error("Failed to get follow list: #{inspect(reason)}")
    end
  end

  defp generate_sitemap_index(sitemaps) do
    Logger.info("Generating sitemap index with entries: #{inspect(sitemaps)}")

    # Format the lastmod dates in W3C format (YYYY-MM-DD)
    sitemap_entries = Enum.map(sitemaps, fn sitemap ->
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
    Disallow: /friedenstaube/
    Disallow: /t/
    Disallow: /.well-known/
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
      author_url = "/p/#{nprofile}"
      alternates_html = alternate_links(author_url)
      ~s(  <url>
    <loc>#{base_url()}#{author_url}</loc>
#{alternates_html}
    <changefreq>weekly</changefreq>
    <priority>0.7</priority>
  </url>)
    end) |> Enum.join("\n")

    # Create complete sitemap XML with xhtml namespace
    sitemap_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xhtml="http://www.w3.org/1999/xhtml">
#{urls_xml}
</urlset>)

    # Write the XML file
    sitemap_path = "#{@sitemap_path}/sitemap-authors.xml"
    atomic_write(sitemap_path, sitemap_content)

    # Compress the XML file
    :os.cmd(~c"gzip -f #{sitemap_path}")

    Logger.info("Author sitemap generated successfully")
    # Return entry for inclusion in index
    %{loc: "#{base_url()}/sitemap/sitemap-authors.xml.gz", lastmod: DateTime.utc_now()}
  end

  defp generate_landing_sitemap do
    Logger.info("Generating landing pages sitemap (/en & /de)")

    # Determine last modified time among landing page HTML files
    landing_dir = Path.join(@base_path, "lp")
    html_files = Path.wildcard(Path.join(landing_dir, "**/*.html"))
    lastmod_dt =
      case html_files do
        [] -> DateTime.utc_now()
        files ->
          files
          |> Enum.map(fn file -> File.stat!(file).mtime end)
          |> Enum.max()
          |> NaiveDateTime.from_erl!()
          |> DateTime.from_naive!("Etc/UTC")
      end
    urls = ["#{base_url()}/en", "#{base_url()}/de"]

    # Build each <url> entry with alternate language link tags
    urls_xml = Enum.map(urls, fn loc ->
      """
  <url>
    <loc>#{loc}</loc>
    <xhtml:link rel="alternate" hreflang="x-default" href="#{base_url()}/en" />
    <xhtml:link rel="alternate" hreflang="en" href="#{base_url()}/en" />
    <xhtml:link rel="alternate" hreflang="de" href="#{base_url()}/de" />
    <lastmod>#{Calendar.strftime(lastmod_dt, "%Y-%m-%d")}</lastmod>
    <changefreq>weekly</changefreq>
    <priority>1.0</priority>
  </url>
  """
    end)
    |> Enum.join("\n")

    sitemap_content = ~s(<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xhtml="http://www.w3.org/1999/xhtml">
#{urls_xml}
</urlset>)

    sitemap_path = "#{@sitemap_path}/sitemap-landing.xml"
    atomic_write(sitemap_path, sitemap_content)
    :os.cmd(~c"gzip -f #{sitemap_path}")
    Logger.info("Landing pages sitemap generated successfully")
    # Return entry for inclusion in index with actual lastmod
    %{loc: "#{base_url()}/sitemap/sitemap-landing.xml.gz", lastmod: lastmod_dt}
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

  defp alternate_links(path) do
    default = NostrBackend.Locale.default_language()
    base = base_url() <> path
    # x-default entry
    xdefault = ~s(    <xhtml:link rel="alternate" hreflang="x-default" href="#{base}" />)
    # per-language entries using configured query param
    param_key = NostrBackend.Locale.locale_param_key()
    codes = NostrBackend.Locale.available_languages()
    |> Enum.map(fn code ->
      href = if code == default, do: base, else: "#{base}?#{param_key}=#{code}"
      ~s(    <xhtml:link rel="alternate" hreflang="#{code}" href="#{href}" />)
    end)
    Enum.join([xdefault | codes], "\n")
  end
end
