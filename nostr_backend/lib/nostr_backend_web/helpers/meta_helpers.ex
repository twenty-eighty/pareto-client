defmodule NostrBackendWeb.MetaHelpers do
  @moduledoc false

  # Twitter/X doesn't support webp images in their cards.
  # Convert .webp URLs to .jpeg while preserving query and fragment.
  def twitter_image_url(nil), do: nil

  def twitter_image_url(url) when is_binary(url) do
    uri = URI.parse(url)
    path = uri.path || ""
    ext = path |> Path.extname() |> String.downcase()

    if ext == ".webp" do
      updated_path = Path.rootname(path) <> ".jpeg"
      URI.to_string(%URI{uri | path: updated_path})
    else
      url
    end
  end
end
