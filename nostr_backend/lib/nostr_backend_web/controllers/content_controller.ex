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
            article = apply_substitution_if_bot(conn, article)
            relay = Map.get(query_data, :relay)
            relays_list = Map.get(query_data, :relays, (if relay, do: [relay], else: []))
            conn
            |> conn_with_article_meta(article, relays_list)
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
            article = apply_substitution_if_bot(conn, article)
            conn
            |> conn_with_article_meta(article, [])
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
            article = apply_substitution_if_bot(conn, article)
            conn
            |> conn_with_article_meta(article, [])
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
            article = apply_substitution_if_bot(conn, article)
            conn
            |> conn_with_article_meta(article, event_info.relays || [])
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
            relay = Map.get(query_data, :relay)
            relays_list = Map.get(query_data, :relays, (if relay, do: [relay], else: []))
            conn
            |> conn_with_article_meta(article, relays_list)
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
            relay = Map.get(query_data, :relay)
            relays_list = Map.get(query_data, :relays, (if relay, do: [relay], else: []))
            conn
            |> conn_with_article_meta(article, relays_list)
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
                article = apply_substitution_if_bot(conn, article)
                conn
                |> conn_with_article_meta(article, relays)
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
        |> conn_with_profile_meta(profile, relays)
        |> put_view(NostrBackendWeb.ContentHTML)
        |> render("profile.html", profile: profile)

      {:error, _reason} ->
        # Generate minimal schema for unknown profile
        schema_metadata = %{
          "@context" => "https://schema.org",
          "@type" => "Thing",
          "url" => get_canonical_profile_url(profile_hex_id),
          "identifier" => get_profile_identifier(profile_hex_id)
        }

        conn
        |> assign(:lang, NostrBackend.Locale.preferred_language(conn))
        |> assign(:page_title, "Profile | Pareto")
        |> assign(:meta_title, "Profile | Pareto")
        |> assign(:meta_url, get_profile_url_with_relays(profile_hex_id, relays))
        |> assign(:canonical_url, get_canonical_profile_url(profile_hex_id))
        |> assign(:meta_description, "Nostr Profile")
        |> assign(:meta_image, @sharing_image)
        |> assign(:schema_metadata, Jason.encode!(schema_metadata))
        |> render(:not_found, layout: false)
    end
  end

  defp conn_with_article_meta(conn, article, _relays) do
    relays_list =
      case Map.get(article, :relays) do
        rel when is_list(rel) and rel != [] -> rel
        _ -> [Application.get_env(:nostr_backend, :feed_generator)[:relay_url] || "wss://nostr.pareto.space"]
      end

    relay_naddr = NostrBackend.NIP19.encode_naddr(article.kind, article.author, article.identifier, relays_list)
    plain_naddr = NostrBackend.NIP19.encode_naddr(article.kind, article.author, article.identifier)
    og_url = Endpoint.url() <> "/a/#{relay_naddr}"
    canonical_url = Endpoint.url() <> "/a/#{plain_naddr}"

    # Get author profile for schema.org metadata
    author_profile = case ProfileCache.get_profile(article.author, []) do
      {:ok, prof} -> prof
      _ -> nil
    end

    # Prepare schema.org metadata
    schema_metadata = %{
      "@context" => "https://schema.org",
      "@type" => "Article",
      "headline" => article.title || @meta_title,
      "description" => article.description || @meta_description,
      "image" => article.image_url |> force_https() || @sharing_image,
      "datePublished" => Map.get(article, :published_at, article.created_at),
      "dateModified" => article.created_at,
      "publisher" => %{
        "@type" => "Organization",
        "name" => "Pareto Project",
        "url" => Endpoint.url()
      },
      "mainEntityOfPage" => %{
        "@type" => "WebPage",
        "@id" => canonical_url
      }
    }

    # Add author information only if we have a valid profile
    schema_metadata = if author_profile do
      Map.put(schema_metadata, "author", %{
        "@type" => "Person",
        "name" => Map.get(author_profile, :display_name) || Map.get(author_profile, :name),
        "url" => get_canonical_profile_url(author_profile.profile_id),
        "identifier" => get_profile_identifier(author_profile)
      })
    else
      # If no profile found, add minimal author information
      Map.put(schema_metadata, "author", %{
        "@type" => "Thing",
        "url" => get_canonical_profile_url(article.author),
        "identifier" => get_profile_identifier(article.author)
      })
    end

    conn
    |> assign(:lang, NostrBackend.Locale.preferred_language(conn))
    |> assign(:page_title, article.title || @meta_title)
    |> assign(:meta_title, article.title || @meta_title)
    |> assign(:meta_url, og_url)
    |> assign(:canonical_url, canonical_url)
    |> assign(:meta_description, article.description || @meta_description)
    |> assign(:meta_image, article.image_url |> force_https() || @sharing_image)
    |> assign(:schema_metadata, Jason.encode!(schema_metadata))
  end

  defp conn_with_community_meta(conn, community) do
    Logger.debug("Community: #{inspect(community)}")

    conn
    |> assign(:lang, NostrBackend.Locale.preferred_language(conn))
    |> assign(:page_title, community.name <> " | Pareto")
    |> assign(:meta_title, community.name <> " | Pareto")
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:meta_description, community.description)
    |> assign(:meta_image, community.image |> force_https() || @sharing_image)
  end

  defp conn_with_profile_meta(conn, profile, _relays) do
    Logger.debug("Profile: #{inspect(profile)}")

    relays_list =
      case Map.get(profile, :relays) do
        rel when is_list(rel) and rel != [] -> rel
        _ -> [Application.get_env(:nostr_backend, :feed_generator)[:relay_url] || "wss://nostr.pareto.space"]
      end

    og_url = get_profile_url_with_relays(profile.profile_id, relays_list)
    canonical_url = get_canonical_profile_url(profile.profile_id)
    lang = NostrBackend.Locale.preferred_language(conn)

    # Prepare schema.org metadata
    relay_nprofile = NostrBackend.NIP19.encode_nprofile(profile.profile_id, relays_list)
    same_as = [
      "https://njump.me/#{relay_nprofile}",
      "https://snort.social/#{relay_nprofile}"
    ]

    same_as = if profile.website do
      same_as ++ [profile.website |> force_https()]
    else
      same_as
    end

    # Get the first available image
    image_url = Map.get(profile, :image) |> force_https() ||
                Map.get(profile, :picture) |> force_https() ||
                Map.get(profile, :banner) |> force_https()

    # Get display name or fall back to name
    display_name = Map.get(profile, :display_name) || Map.get(profile, :name)

    # Start with required fields
    schema_metadata = %{
      "@context" => "https://schema.org",
      "@type" => "Person",
      "url" => canonical_url,
      "sameAs" => same_as
    }

    # Add optional fields only if they exist
    schema_metadata = schema_metadata
      |> maybe_add_field("name", display_name)
      |> maybe_add_field("description", Map.get(profile, :about))
      |> maybe_add_field("image", image_url)
      |> maybe_add_field("identifier", get_profile_identifier(profile))

    conn
    |> assign(:lang, lang)
    |> assign(:page_title, (display_name || "Profile") <> " | Pareto")
    |> assign(:meta_title, (display_name || "Profile") <> " | Pareto")
    |> assign(:meta_url, og_url)
    |> assign(:canonical_url, canonical_url)
    |> assign(:meta_description, Map.get(profile, :about))
    |> assign(:meta_image, image_url || @sharing_image)
    |> assign(:schema_metadata, Jason.encode!(schema_metadata))
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
    |> assign(:lang, NostrBackend.Locale.preferred_language(conn))
    |> assign(:schema_metadata, nil)
  end

  def force_https(nil), do: nil

  def force_https(url) do
    uri = URI.parse(url)

    # Update the scheme to "https"
    updated_uri = %URI{uri | scheme: "https"}

    # Convert back to string
    URI.to_string(updated_uri)
  end

  defp apply_substitution_if_bot(conn, article) do
    user_agent = get_req_header(conn, "user-agent") |> List.first()
    if is_sharing_bot?(user_agent) do
      %{article |
        title: NostrBackend.Substitution.replace_randomly(article.title),
        description: NostrBackend.Substitution.replace_randomly(article.description),
        content: ""
      }
    else
      article
    end
  end

  # Detect known social-sharing bots via User-Agent
  defp is_sharing_bot?(user_agent) do
    ua = (user_agent || "") |> String.downcase()
    cond do
      String.starts_with?(ua, "facebook") -> true
      String.starts_with?(ua, "meta-externalagent") -> true
      String.contains?(ua, "google (+https://developers.google.com/+/web/snippet)/") -> true
      String.contains?(ua, "instagram") -> true
      String.starts_with?(ua, "linkedin bot") -> true
      String.starts_with?(ua, "linkedinbot") -> true
      String.starts_with?(ua, "pinterest bot") -> true
      String.starts_with?(ua, "secondlife") -> true
      String.contains?(ua, "skypeuripreview") -> true
      String.starts_with?(ua, "slackbot link checker") -> true
      String.starts_with?(ua, "slackbot-linkexpanding") -> true
      String.starts_with?(ua, "snapchat") -> true
      String.contains?(ua, "telegrambot") -> true
      String.contains?(ua, "twitterbot") -> true
      String.starts_with?(ua, "viber") -> true
      String.starts_with?(ua, "whatsapp") -> true
      String.starts_with?(ua, "wire linkpreview bot") -> true
      String.contains?(ua, "xing bot") -> true
      String.starts_with?(ua, "zoom info bot") -> true
      String.starts_with?(ua, "zoom.mac") -> true
      true -> false
    end
  end

  @doc """
  Generates a canonical profile URL for a given profile ID.
  This URL is stable and doesn't include relay information.
  """
  def get_canonical_profile_url(profile_id) do
    plain_nprofile = NostrBackend.NIP19.encode_nprofile(profile_id)
    Endpoint.url() <> "/p/#{plain_nprofile}"
  end

  @doc """
  Generates a profile URL with relay information.
  This URL includes the relay information for direct access.
  """
  def get_profile_url_with_relays(profile_id, relays) do
    relay_nprofile = NostrBackend.NIP19.encode_nprofile(profile_id, relays)
    Endpoint.url() <> "/p/#{relay_nprofile}"
  end

  # Helper function to generate proper identifier structure
  defp get_profile_identifier(profile_or_pubkey) do
    case profile_or_pubkey do
      %{nip05: nip05} when not is_nil(nip05) ->
        %{
          "@type" => "PropertyValue",
          "propertyID" => "Nostr NIP-05",
          "value" => nip05
        }
      %{profile_id: pubkey} ->
        %{
          "@type" => "PropertyValue",
          "propertyID" => "Nostr npub",
          "value" => NostrBackend.NIP19.encode_npub(pubkey)
        }
      pubkey when is_binary(pubkey) ->
        %{
          "@type" => "PropertyValue",
          "propertyID" => "Nostr npub",
          "value" => NostrBackend.NIP19.encode_npub(pubkey)
        }
    end
  end

  # Helper function to conditionally add fields to the schema metadata
  defp maybe_add_field(schema, _key, nil), do: schema
  defp maybe_add_field(schema, key, value) when value != "", do: Map.put(schema, key, value)
  defp maybe_add_field(schema, _key, _value), do: schema
end
