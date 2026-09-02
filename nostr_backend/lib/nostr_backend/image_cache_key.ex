defmodule NostrBackend.ImageCacheKey do
  @moduledoc """
  HMAC keys for the image caching server.

  `IMAGE_CACHE_KEY` is the current key (also injected into the Elm client).
  `IMAGE_CACHE_KEY_PREVIOUS` is accepted during rotation. Neither is stored
  in a database; they are environment variables.
  """

  def current, do: env(:image_cache_key)
  def previous, do: env(:image_cache_key_previous)

  def payload do
    %{
      "current" => current(),
      "previous" => previous()
    }
  end

  defp env(key) do
    case Application.get_env(:nostr_backend, key, "") do
      value when is_binary(value) -> value
      _ -> ""
    end
  end
end
