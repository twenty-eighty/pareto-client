defmodule NostrBackendWeb.ContentController do
  use NostrBackendWeb, :controller

  alias NostrBackendWeb.Endpoint

  alias NostrBackend.NostrId
  alias NostrBackend.ArticleCache
  alias NostrBackend.ProfileCache

  def article(conn, %{"article_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      {:ok, {:author_article, query_data}} ->
        case ArticleCache.get_article(query_data) do
          {:ok, article} ->
            conn
            |> conn_with_meta(article)
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
            |> conn_with_meta(article)
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
            |> conn_with_meta(article)
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

  defp conn_with_meta(conn, article) do
    conn
    |> assign(:page_title, article.title)
    |> assign(:meta_title, article.title)
    |> assign(:meta_url, Endpoint.url() <> conn.request_path)
    |> assign(:meta_description, article.description)
    |> assign(:meta_image, article.image_url)
    |> assign(
      :meta_image,
      article.image_url || Endpoint.static_url() <> "/images/pareto_banner.png"
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

  def profile(conn, %{"profile_id" => nostr_id}) do
    case NostrId.parse(nostr_id) do
      {:ok, {:profile, profile_hex_id}} ->
        case ProfileCache.get_profile(profile_hex_id) do
          {:ok, profile} ->
            render(conn, "profile.html", profile: profile)

          {:error, _reason} ->
            render(conn, "profile.html", profile: nil)
        end

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> text("Invalid Nostr ID: #{reason}")
    end
  end
end
