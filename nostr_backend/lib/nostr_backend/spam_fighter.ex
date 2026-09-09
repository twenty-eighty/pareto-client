defmodule NostrBackend.SpamFighter do
  @moduledoc """
  Optional integration with nostr-spam-fighter moderation API.

  When `SPAM_FIGHTER_API_KEY` is set, article naddrs are checked before serving.
  Suppression only happens when the API returns `blacklisted: true`. Any other
  result (clean, pending/unknown, HTTP errors, timeouts) fails open and serves.
  """

  require Logger

  @default_url "https://nostr-spam-fighter.onrender.com"
  @default_timeout_ms 5_000

  @doc """
  Returns `true` only when the article should be suppressed (empty 404).
  """
  @spec suppress_article?(String.t()) :: boolean()
  def suppress_article?(naddr) when is_binary(naddr) do
    case config() do
      :disabled ->
        false

      {:ok, base_url, api_key} ->
        case fetch_moderation(base_url, api_key, naddr) do
          {:ok, true} -> true
          _ -> false
        end
    end
  end

  defp config do
    api_key =
      :nostr_backend
      |> Application.get_env(:spam_fighter_api_key, "")
      |> to_string()
      |> String.trim()

    if api_key == "" do
      :disabled
    else
      base_url =
        :nostr_backend
        |> Application.get_env(:spam_fighter_url, @default_url)
        |> to_string()
        |> String.trim()
        |> String.trim_trailing("/")

      {:ok, base_url, api_key}
    end
  end

  defp fetch_moderation(base_url, api_key, naddr) do
    fetch = Application.get_env(:nostr_backend, :spam_fighter_fetch, &http_fetch/3)
    fetch.(base_url, api_key, naddr)
  end

  defp http_fetch(base_url, api_key, naddr) do
    url = "#{base_url}/api/v1/articles/#{URI.encode(naddr)}/moderation"

    case Req.get(url,
           headers: [{"authorization", "Bearer #{api_key}"}],
           receive_timeout: timeout_ms(),
           connect_options: [timeout: timeout_ms()]
         ) do
      {:ok, %Req.Response{status: 200, body: body}} ->
        {:ok, body["blacklisted"] == true}

      {:ok, %Req.Response{status: status}} ->
        Logger.warning("SpamFighter moderation returned status #{status} for naddr")
        {:error, {:unexpected_status, status}}

      {:error, reason} ->
        Logger.warning("SpamFighter moderation unavailable: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp timeout_ms do
    Application.get_env(:nostr_backend, :spam_fighter_timeout_ms, @default_timeout_ms)
  end
end
