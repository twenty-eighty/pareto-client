defmodule NostrBackendWeb.FrontendAssets do
  @html_file_path Path.join(:code.priv_dir(:nostr_backend), "static/index.html")

  # Public functions to get JS and CSS file paths, caching results on first call
  def js_file do
    extract_file_path("script", @html_file_path)
  end

  def css_file do
    extract_file_path("link", @html_file_path)
  end

  # Private function to extract either JS or CSS file path from HTML
  defp extract_file_path(type, html_file_path) do
    html_file_path
    |> File.read()
    |> case do
      {:ok, content} ->
        regex =
          case type do
            "script" -> ~r/src="\/assets\/(index-[\w\d-]+\.js)"/
            "link" -> ~r/href="\/assets\/(index-[\w\d-]+\.css)"/
          end

        find_asset_in_html(content, regex)

      {:error, reason} ->
        raise "Failed to read index.html file: #{inspect(reason)}"
    end
  end

  # Generic helper to find the matching asset in HTML
  defp find_asset_in_html(html_content, regex) do
    case Regex.run(regex, html_content) do
      [_, file_path] -> "/assets/#{file_path}"
      _ -> raise "Asset file not found in index.html"
    end
  end
end
