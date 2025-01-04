defmodule NostrBackendWeb.RumbleController do
  use NostrBackendWeb, :controller
  alias Req
  alias Floki

  def fetch_embed_url(conn, %{"url" => url}) do
    headers = [
      {"User-Agent",
       "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"}
    ]

    case Req.get(url, headers: headers) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        case extract_embed_url(body) do
          {:ok, embed_url} ->
            redirect(conn, external: embed_url)

          :error ->
            conn
            |> put_status(:not_found)
            |> text("Embed URL not found in the provided page")
        end

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch URL: #{inspect(reason)}")
    end
  end

  defp extract_embed_url(html) do
    html
    |> Floki.parse_document!()
    |> Floki.find("link[rel='alternate'][type='application/json+oembed']")
    |> Floki.attribute("href")
    |> List.first()
    |> case do
      nil ->
        :error

      oembed_url ->
        case URI.parse(oembed_url) do
          %URI{query: query} ->
            query_params = URI.decode_query(query)

            case Map.get(query_params, "url") do
              nil -> :error
              embed_url -> {:ok, embed_url}
            end

          _ ->
            :error
        end
    end
  end
end
