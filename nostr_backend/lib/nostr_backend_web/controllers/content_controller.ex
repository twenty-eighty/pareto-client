defmodule NostrBackendWeb.ContentController do
  use NostrBackendWeb, :controller

  alias NostrBackendWeb.Endpoint

  alias NostrBackend.Nip05
  alias NostrBackend.NostrId
  alias NostrBackend.ArticleCache
  alias NostrBackend.Nip05Cache
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
            IO.inspect(reason, label: "ERROR REASON")

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
            IO.inspect(reason, label: "ERROR REASON")

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

      {:error, reason} ->
        conn
        |> conn_with_default_meta()
        |> text("Invalid Nostr ID: #{reason}")
        |> render(:not_found, layout: false)
    end
  end

  def profile(conn, %{"profile_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      {:ok, {:profile, profile_hex_id, relays}} ->
        # TODO: use relays for profile lookup
        get_and_render_profile(conn, profile_hex_id, relays)

      {:error, _reason} ->
        conn
        |> conn_with_default_meta()
        |> render(:not_found, layout: false)
    end
  end

  def user_nip05(conn, %{"user_nip05" => user_nip05}) do
    case Nip05.parse_identifier(user_nip05) do
      {:ok, _name, _domain} ->
        case Nip05Cache.get_pubkey_and_relays(user_nip05) do
          {:ok, pubkey, relays} ->
            # TODO: use relays for profile lookup
            get_and_render_profile(conn, pubkey, relays)

          {:error, reason} ->
            IO.inspect(reason, label: "ERROR REASON")

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
            IO.inspect(reason, label: "ERROR REASON")

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
    conn
    |> assign(:page_title, article.title || @meta_title)
    |> assign(:meta_title, article.title || @meta_title)
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:meta_description, article.description || @meta_description)
    |> assign(
      :meta_image,
      article.image_url || @sharing_image
    )
  end

  defp conn_with_profile_meta(conn, profile) do
    IO.inspect(profile, label: "Profile")

    conn
    |> assign(:page_title, profile.name <> " | Pareto")
    |> assign(:meta_title, profile.name <> " | Pareto")
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:meta_description, profile.about)
    |> assign(
      :meta_image,
      profile.image || profile.picture || profile.banner || @sharing_image
    )
  end

  defp conn_with_default_meta(conn) do
    conn
    |> assign(:title, "Pareto Project")
    |> assign(:meta_title, "Pareto Project")
    |> assign(
      :meta_description,
      "An open source publishing ecosystem for uncensorable, citizen journalism powered by Nostr, Lightning and eCash."
    )
    |> assign(:meta_image, "/images/pareto_banner.png")
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:article, nil)
  end
end
