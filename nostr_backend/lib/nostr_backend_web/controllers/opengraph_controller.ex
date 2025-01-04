defmodule NostrBackendWeb.OpenGraphController do
  use NostrBackendWeb, :controller
  alias Req
  alias Floki

  def fetch_metadata_image(conn, %{"url" => url}) do
    case Req.get(url) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        case extract_image_url(body) do
          {:ok, image_url} ->
            conn
            |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
            |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
            |> redirect(external: image_url)

          :error ->
            conn
            |> put_status(:not_found)
            |> text("OpenGraph image not found")
        end

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> text("Failed to fetch URL: #{inspect(reason)}")
    end
  end

  defp extract_image_url(html) do
    html
    |> Floki.parse_document!()
    |> Floki.find("meta[property='og:image']")
    |> Floki.attribute("content")
    |> List.first()
    |> case do
      nil -> :error
      image_url -> {:ok, image_url}
    end
  end

  def fetch_metadata(conn, %{"url" => url}) do
    case Req.get(url) do
      {:ok, %Req.Response{body: body, status: 200}} ->
        metadata = extract_opengraph_metadata(body)

        conn
        |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
        |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
        |> json(metadata)

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Failed to fetch URL: #{inspect(reason)}"})
    end
  end

  defp extract_opengraph_metadata(html) do
    html
    |> Floki.parse_document!()
    |> Floki.find("meta[property^='og:']")
    |> Enum.reduce(%{}, fn tag, acc ->
      property =
        Floki.attribute(tag, "property") |> List.first() |> String.replace_prefix("og:", "")

      content = Floki.attribute(tag, "content") |> List.first()
      Map.put(acc, property, content)
    end)
  end
end
