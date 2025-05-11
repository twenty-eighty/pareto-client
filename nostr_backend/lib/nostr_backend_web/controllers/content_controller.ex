defmodule NostrBackendWeb.ContentController do
  use NostrBackendWeb, :controller
  require Logger

  alias NostrBackendWeb.Endpoint

  alias NostrBackend.Nip05
  alias NostrBackend.NostrId
  alias NostrBackend.ArticleCache
  alias NostrBackend.CommunityCache
  alias NostrBackend.Nip05Cache
  alias NostrBackend.NoteCache
  alias NostrBackend.ProfileCache

  @meta_title "The Pareto Project"
  @meta_description "An open source publishing platform for uncensorable, investigative journalism powered by Nostr, Lightning and eCash."
  @sharing_image "https://pareto.space/images/pareto-shared.png"

  def article(conn, %{"article_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      {:ok, {:author_article, query_data}} ->
        case ArticleCache.get_article(query_data) do
          {:ok, article} ->

            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:ok, {:article, article_hex_id}} ->
        case ArticleCache.get_article(article_hex_id) do
          {:ok, article} ->
            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:ok, {:address, address_info}} ->
        case ArticleCache.get_article(address_info) do
          {:ok, article} ->
            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, _reason} ->
            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:ok, {:event, event_info}} ->
        case ArticleCache.get_article(event_info) do
          {:ok, article} ->
            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, _reason} ->
            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:error, reason} ->
        conn
        |> conn_with_default_meta()
        |> text("Invalid Nostr ID: #{reason}")
        |> render(:not_found, layout: false)
    end
  end

  def community(conn, %{"community_id" => community_id}) do
    case NostrId.parse(community_id) do
      {:ok, {:community, community_data}} ->
        get_and_render_community(conn, community_data)

      {:error, _reason} ->
        conn
        |> conn_with_default_meta()
        |> render(:not_found, layout: false)
    end
  end

  # TODO: check if this branch works
  def event(conn, %{"event_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      {:ok, {:note, note_id_hex}} ->
        case NoteCache.get_note(note_id_hex) do
          {:ok, note} ->
            conn
            |> conn_with_default_meta()
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:note, note: note)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:ok, {:author_event, query_data}} ->
        case ArticleCache.get_article(query_data) do
          {:ok, article} ->
            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            #            |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:ok, {:author_article, query_data}} ->
        case ArticleCache.get_article(query_data) do
          {:ok, article} ->
            conn
            |> conn_with_article_meta(article)
            |> put_view(NostrBackendWeb.ContentHTML)
            |> render(:article, article: article)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)
        end

      {:error, reason} ->
        conn
        |> conn_with_default_meta()
        |> text("Invalid Nostr ID: #{reason}")
        |> render(:not_found, layout: false)
    end
  end

  def profile(conn, %{"profile_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      # Handle nprofile identifiers (with optional relays)
      {:ok, {:profile, profile_hex_id, relays}} ->
        get_and_render_profile(conn, profile_hex_id, relays)
      # Handle npub identifiers (pubkey only)
      {:ok, {:pubkey, pubkey_hex}} ->
        get_and_render_profile(conn, pubkey_hex, [])
      {:error, _reason} ->
        conn
        |> conn_with_default_meta()
        |> render(:not_found, layout: false)
    end
  end

  def hashtag(conn, %{"hashtag" => hashtag}) do
    conn
    |> conn_with_default_meta()
    |> render("hashtag.html", hashtag: hashtag)
  end

  def user_nip05(conn, %{"user_nip05" => user_nip05}) do
    case Nip05.parse_identifier(user_nip05) do
      {:ok, _name, _domain} ->
        case Nip05Cache.get_pubkey_and_relays(user_nip05) do
          {:ok, pubkey, relays} ->
            # TODO: use relays for profile lookup
            get_and_render_profile(conn, pubkey, relays)

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            # |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:error, reason} ->
        conn
        |> conn_with_default_meta()
        |> text("Invalid NIP-05 identifier: #{reason}")
        |> render(:not_found, layout: false)
    end
  end

  def nip05_article(conn, %{"user_nip05" => user_nip05, "identifier" => article_id}) do
    case Nip05.parse_identifier(user_nip05) do
      {:ok, _name, _domain} ->
        case Nip05Cache.get_pubkey_and_relays(user_nip05) do
          {:ok, pubkey, relays} ->
            case ArticleCache.get_article(%{
                   kind: 30023,
                   identifier: article_id,
                   author: pubkey,
                   relays: relays
                 }) do
              {:ok, article} ->
                conn
                |> conn_with_article_meta(article)
                |> put_view(NostrBackendWeb.ContentHTML)
                |> render(:article, article: article)

              {:error, _reason} ->
                conn
                |> conn_with_default_meta()
                |> render(:not_found, layout: false)
            end

          {:error, reason} ->
            Logger.debug("ERROR REASON: #{inspect(reason)}")

            conn
            |> conn_with_default_meta()
            |> render(:not_found, layout: false)

            # |> render(NostrBackendWeb.ErrorHTML, :"404")
        end

      {:error, reason} ->
        conn
        |> conn_with_default_meta()
        |> text("Invalid NIP-05 identifier: #{reason}")
        |> render(:not_found, layout: false)
    end
  end

  defp get_and_render_community(conn, community_data) do
    case CommunityCache.get_community(community_data) do
      {:ok, community} ->
        conn
        |> conn_with_community_meta(community)
        |> put_view(NostrBackendWeb.ContentHTML)
        |> render("community.html", community: community)

      {:error, _reason} ->
        conn
        |> conn_with_default_meta()
        |> render(:not_found, layout: false)
    end
  end

  defp get_and_render_profile(conn, profile_hex_id, relays) do
    case ProfileCache.get_profile(profile_hex_id, relays) do
      {:ok, profile} ->
        conn
        |> conn_with_profile_meta(profile)
        |> put_view(NostrBackendWeb.ContentHTML)
        |> render("profile.html", profile: profile)

      {:error, _reason} ->
        conn
        |> conn_with_default_meta()
        |> render(:not_found, layout: false)
    end
  end

  defp conn_with_article_meta(conn, article) do
    relay = Application.get_env(:nostr_backend, :feed_generator)[:relay_url] || "wss://nostr.pareto.space"
    relay_naddr = NostrBackend.NIP19.encode_naddr(article.kind, article.author, article.identifier, [relay])
    plain_naddr = NostrBackend.NIP19.encode_naddr(article.kind, article.author, article.identifier)
    og_url = Endpoint.url() <> "/a/#{relay_naddr}"
    canonical_url = Endpoint.url() <> "/a/#{plain_naddr}"
    conn
    |> assign(:page_title, article.title || @meta_title)
    |> assign(:meta_title, article.title || @meta_title)
    |> assign(:meta_url, og_url)
    |> assign(:canonical_url, canonical_url)
    |> assign(:meta_description, article.description || @meta_description)
    |> assign(:meta_image, article.image_url |> force_https() || @sharing_image)
  end

  defp conn_with_community_meta(conn, community) do
    Logger.debug("Community: #{inspect(community)}")

    conn
    |> assign(:page_title, community.name <> " | Pareto")
    |> assign(:meta_title, community.name <> " | Pareto")
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:meta_description, community.description)
    |> assign(:meta_image, community.image |> force_https() || @sharing_image)
  end

  defp conn_with_profile_meta(conn, profile) do
    Logger.debug("Profile: #{inspect(profile)}")

    relay = Application.get_env(:nostr_backend, :feed_generator)[:relay_url] || "wss://nostr.pareto.space"
    relay_nprofile = NostrBackend.NIP19.encode_nprofile(profile.profile_id, [relay])
    plain_nprofile = NostrBackend.NIP19.encode_nprofile(profile.profile_id)
    og_url = Endpoint.url() <> "/p/#{relay_nprofile}"
    canonical_url = Endpoint.url() <> "/p/#{plain_nprofile}"
    conn
    |> assign(:page_title, profile.name <> " | Pareto")
    |> assign(:meta_title, profile.name <> " | Pareto")
    |> assign(:meta_url, og_url)
    |> assign(:canonical_url, canonical_url)
    |> assign(:meta_description, profile.about)
    |> assign(:meta_image, profile.image |> force_https() || profile.picture |> force_https() ||
      profile.banner |> force_https() || @sharing_image)
  end

  defp conn_with_default_meta(conn) do
    conn
    |> assign(:title, "Pareto Project")
    |> assign(:meta_title, "Pareto Project")
    |> assign(
      :meta_description,
      "An open source publishing ecosystem for uncensorable, citizen journalism powered by Nostr, Lightning and eCash."
    )
    |> assign(:meta_image, @sharing_image)
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:canonical_url, Endpoint.url() <> conn.request_path)
    |> assign(:article, nil)
  end

  def force_https(nil), do: nil

  def force_https(url) do
    uri = URI.parse(url)

    # Update the scheme to "https"
    updated_uri = %URI{uri | scheme: "https"}

    # Convert back to string
    URI.to_string(updated_uri)
  end
end
