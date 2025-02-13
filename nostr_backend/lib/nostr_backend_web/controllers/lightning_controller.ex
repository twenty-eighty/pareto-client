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
    },
    "rodant" => %{
      "nostrPubkey" => "71df211931d26ee41121d295bd43cbc7e382505e333b5c13d4016ced9542d9d7",
      "callback" => "https://getalby.com/lnurlp/vntonior7z/callback",
      "metadata" => "[[\"text/identifier\",\"vntonior7z@getalby.com\"],[\"text/plain\",\"Sats for VntonIO\"]]",
      "minSendable" => 1000,
      "maxSendable" => 10000000000
    },
    "janosch" => %{
      "nostrPubkey" => "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/4845dac6-5b78-439d-9ab1-90e18bacbef6",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: restfulbench94\"],[\"text/identifier\",\"restfulbench94@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 10000000000
    },
    "nachteule" => %{
      "nostrPubkey" => "9c8096eb84d574ca29eb0077d615a2b12c0113064faeac9f72e464a066e47555",
      "callback" => "https://getalby.com/lnurlp/brokendarkness103/callback",
      "metadata" => "[[\"text/identifier\",\"brokendarkness103@getalby.com\"],[\"text/plain\",\"Sats for Nachteule\"]]",
      "minSendable" => 1000,
      "maxSendable" => 10000000000
    },
    "kalle" => %{
      "nostrPubkey" => "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/4ad194e0-2904-43c4-b6ea-295daeb7ca28",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: balmyravioli90\"],[\"text/identifier\",\"balmyravioli90@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 100000000000
    },
    "donjoe" => %{
      "nostrPubkey" => "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/5b03fa9a-7ab1-4bd1-bcfe-b2718049096c",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: compacthook86\"],[\"text/identifier\",\"compacthook86@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 100000000000
    },
    "_" => %{
      "nostrPubkey" => "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/e7681b38-746f-4694-9fae-f972e1345d01",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: donate2pareto\"],[\"text/identifier\",\"donate2pareto@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 100000000000
    },
    "milosz" => %{
      "nostrPubkey" => "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/e7681b38-746f-4694-9fae-f972e1345d01",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: donate2pareto\"],[\"text/identifier\",\"donate2pareto@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 100000000000
    },
    "ashoka" => %{
      "nostrPubkey" => "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5",
      "callback" => "https://livingroomofsatoshi.com/api/v1/lnurl/payreq/e7681b38-746f-4694-9fae-f972e1345d01",
      "metadata" => "[[\"text/plain\",\"Pay to Wallet of Satoshi user: donate2pareto\"],[\"text/identifier\",\"donate2pareto@walletofsatoshi.com\"]]",
      "minSendable" => 1000,
      "maxSendable" => 100000000000
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
