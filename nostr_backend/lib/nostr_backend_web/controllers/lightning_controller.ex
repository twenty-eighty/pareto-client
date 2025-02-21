defmodule NostrBackendWeb.LightningController do
  use NostrBackendWeb, :controller

  # Define the LNURL-P data for usernames
  # Download https://<domain>/.well-known/lnurlp/<username> to get the relevant data
  @lnurlp_data %{
    "roland" => %{
      "nostrPubkey" => "79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432",
      "lud16" => "rohe@getalby.com"
    },
    "rodant" => %{
      "nostrPubkey" => "71df211931d26ee41121d295bd43cbc7e382505e333b5c13d4016ced9542d9d7",
      "lud16" => "vntonior7z@getalby.com"
    },
    "janosch" => %{
      "nostrPubkey" => "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01",
      "lud16" => "restfulbench94@walletofsatoshi.com"
    },
    "nachteule" => %{
      "nostrPubkey" => "9c8096eb84d574ca29eb0077d615a2b12c0113064faeac9f72e464a066e47555",
      "lud16" => "brokendarkness103@getalby.com"
    },
    "kalle" => %{
      "nostrPubkey" => "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518",
      "lud16" => "balmyravioli90@walletofsatoshi.com"
    },
    "donjoe" => %{
      "nostrPubkey" => "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe",
      "lud16" => "compacthook86@walletofsatoshi.com"
    },
    "_" => %{
      "nostrPubkey" => "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84",
      "lud16" => "donate2pareto@walletofsatoshi.com"
    },
    "milosz" => %{
      "nostrPubkey" => "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638",
      "lud16" => "donate2pareto@walletofsatoshi.com"
    },
    "ashoka" => %{
      "nostrPubkey" => "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5",
      "lud16" => "donate2pareto@walletofsatoshi.com"
    }
  }

  def lnurlp(conn, %{"username" => username}) do
    conn = put_required_headers(conn)

    case Map.get(@lnurlp_data, username) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "User not found"})

      %{"nostrPubkey" => nostr_pubkey, "lud16" => lud16} ->
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
