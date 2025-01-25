defmodule NostrBackendWeb.LightningController do
  use NostrBackendWeb, :controller

  # Define the LNURL-P data for usernames
  @lnurlp_data %{
    "roland" => %{
      "nostrPubkey" => "79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432",
      "callback" => "https://getalby.com/lnurlp/rohe/callback",
      "metadata" => "[[\"text/identifier\",\"rohe@getalby.com\"],[\"text/plain\",\"Sats for Separate money from state!\"]]",
      "minSendable" => 1000,
      "maxSendable" => 10000000000
    }
  }

  def lnurlp(conn, %{"username" => username}) do
    conn = put_required_headers(conn)

    case Map.get(@lnurlp_data, username) do
      nil ->
        conn
        |> put_status(:not_found)
        |> text("LNURL-P data not found for username: #{username}")

      data ->
        json(conn, %{
          "status" => "OK",
          "tag" => "payRequest",
          "commentAllowed" => 255,
          "callback" => data["callback"],
          "metadata" => data["metadata"],
          "minSendable" => data["minSendable"],
          "maxSendable" => data["maxSendable"],
          "payerData" => %{
            "name" => %{"mandatory" => false},
            "email" => %{"mandatory" => false},
            "pubkey" => %{"mandatory" => false}
          },
          "nostrPubkey" => data["nostrPubkey"],
          "allowsNostr" => true
        })
    end
  end

  defp put_required_headers(conn) do
    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  end
end
