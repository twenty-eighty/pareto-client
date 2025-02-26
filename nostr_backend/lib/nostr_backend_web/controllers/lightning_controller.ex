defmodule NostrBackendWeb.LightningController do
  use NostrBackendWeb, :controller

  alias NostrBackendWeb.NostrController

  # Define the LNURL-P data for usernames
  # Download https://<domain>/.well-known/lnurlp/<username> to get the relevant data
  @lnurlp_data %{
    "roland" => "rohe@getalby.com",
    "rodant" => "vntonior7z@getalby.com",
    "janosch" => "restfulbench94@walletofsatoshi.com",
    "nachteule" => "brokendarkness103@getalby.com",
    "kalle" => "balmyravioli90@walletofsatoshi.com",
    "donjoe" => "compacthook86@walletofsatoshi.com",
    "_" => "donate2pareto@walletofsatoshi.com",
    "milosz" => "donate2pareto@walletofsatoshi.com",
    "ashoka" => "donate2pareto@walletofsatoshi.com",
    "friedenstaube" => "restorationthrumming883287@getalby.com"
  }

  def lnurlp(conn, %{"username" => username}) do
    conn = put_required_headers(conn)

    nostr_data = NostrController.get_nostr_data()
    case {@lnurlp_data[username], Map.get(nostr_data["names"], username)} do
      {nil, _} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      { _lud16, nil} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Nostr pubkey not found for user"})

      { lud16, nostr_pubkey} ->
        lnurlp_url = build_lnurlp_url(lud16)

        case fetch_lnurlp_json(lnurlp_url) do
          {:ok, json_data} ->
            updated_json = Map.put(json_data, "nostrPubkey", nostr_pubkey)

            conn
            |> put_status(:ok)
            |> json(updated_json)

          {:error, reason} ->
            conn
            |> put_status(:bad_gateway)
            |> json(%{error: "Failed to fetch LNURLP data", reason: reason})
        end
    end
  end

  def test_lnurlp_entries do
    @lnurlp_data
    |> Enum.map(fn {username, lud16 = _user_data} ->
      lnurlp_url = build_lnurlp_url(lud16)

      case fetch_lnurlp_json(lnurlp_url) do
        {:ok, _json_data} ->
          {username, lud16, :success}

        {:error, reason} ->
          {username, lud16, {:error, reason}}
      end
    end)
  end

  defp build_lnurlp_url(lud16) do
    [username, domain] = String.split(lud16, "@")
    "https://#{domain}/.well-known/lnurlp/#{username}"
  end

  defp fetch_lnurlp_json(url) do
    case HTTPoison.get(url, [], follow_redirect: true) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, json} -> {:ok, json}
          {:error, _} -> {:error, "Invalid JSON response"}
        end

      {:ok, %HTTPoison.Response{status_code: code}} ->
        {:error, "HTTP error: #{code}"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp put_required_headers(conn) do
    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  end
end
