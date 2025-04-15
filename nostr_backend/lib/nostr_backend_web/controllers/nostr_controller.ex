defmodule NostrBackendWeb.NostrController do
  use NostrBackendWeb, :controller

  alias NostrBackend.Nip05

  @default_relays [
        "wss://nostr.pareto.space",
        "wss://nostr.pareto.town",
        "wss://pareto.nostr1.com"
  ]

  # Define the names and relays in a module attribute
  @nostr_data %{
    "names" => %{
      "milosz" => "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638",
      "ashoka" => "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5",
      "roland" => "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2",
      "donjoe" => "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe",
      "kalle" => "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518",
      "rodant" => "71df211931d26ee41121d295bd43cbc7e382505e333b5c13d4016ced9542d9d7",
      "aron" => "9f94e6cc5ce50dcaccfc42b18029aba0ac9215d673197a40172896d3f3472946",
      "matej" => "5aa5e38abbb37f89c863419bd1e4e60aa31d82fa3c39397e386586e3961b8021",
      "psychobabble" => "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f",
      "indikativ" => "1a040599c19734813abcec04d9bda0ff5fc5054fc4d035b79484bf970a05f5c4",
      "walter_siegrist" => "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c",
      "_" => "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84",
      "client" => "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5",
      "christof_weber" => "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066",
      "michael_meyen" => "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e",
      "janosch" => "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01",
      "j1000" => "135f20a6f142a3f8a6c3fde48772bd6cffece0fc3aa31bef6f12c99b3937e969",
      "hartmut" => "92af1031a8dc9fd1c2ef982219ff6cff9944ec62bd45d4c6e4e8d5ffd9939aeb",
      "nachteule" => "9c8096eb84d574ca29eb0077d615a2b12c0113064faeac9f72e464a066e47555",
      "friedenstaube" => "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5",
      "danjel" => "22ea455f4b837d60c2615721ccc2ef54a4d56e5f00993f4cffd2b98c9180f82f",
      "admin" => "0f475886295e78e74af76dfacb3ea537de1938ee79cf92c66d417b5ccd03f43c",
      "kuma" => "0f467b0603cd3229ad436399a2d4144965f16841767160296a0a7048a844c6f4",
      "email-gateway" => "cefbf43addd677426c671d7cd275289be35f7b6b398fced7fae420d060e7a345",
      "authors" => "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef",
      "editors" => "0f479cb726c1578ca765d5ff6a0c58855263977d5d7cf7b4cea23d42d557c611",
      "rss" => "0f4791c38e1236dc55f11acbf37a00da8879906d9374498378db8d6ea7952869",
      "subscription-server" => "f7721f8728935c943710a2f06288cbd56da7ab20b43400a16d26ac58880e0087",
      "beta-testers" => "0f479ef1a8870a917afbae778ea75fedb5db4cb64501e0e64a6d2010a2908e64"
    },
    "relays" => %{
      "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638" => @default_relays,
      "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5" => @default_relays,
      "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2" => @default_relays,
      "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe" => @default_relays,
      "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518" => @default_relays,
      "71df211931d26ee41121d295bd43cbc7e382505e333b5c13d4016ced9542d9d7" => @default_relays,
      "9f94e6cc5ce50dcaccfc42b18029aba0ac9215d673197a40172896d3f3472946" => @default_relays,
      "5aa5e38abbb37f89c863419bd1e4e60aa31d82fa3c39397e386586e3961b8021" => @default_relays,
      "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f" => @default_relays,
      "1a040599c19734813abcec04d9bda0ff5fc5054fc4d035b79484bf970a05f5c4" => @default_relays,
      "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c" => @default_relays,
      "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84" => @default_relays,
      "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5" => @default_relays,
      "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066" => @default_relays,
      "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e" => @default_relays,
      "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01" => @default_relays,
      "135f20a6f142a3f8a6c3fde48772bd6cffece0fc3aa31bef6f12c99b3937e969" => @default_relays,
      "92af1031a8dc9fd1c2ef982219ff6cff9944ec62bd45d4c6e4e8d5ffd9939aeb" => @default_relays,
      "9c8096eb84d574ca29eb0077d615a2b12c0113064faeac9f72e464a066e47555" => @default_relays,
      "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5" => @default_relays,
      "22ea455f4b837d60c2615721ccc2ef54a4d56e5f00993f4cffd2b98c9180f82f" => @default_relays,
      "0f475886295e78e74af76dfacb3ea537de1938ee79cf92c66d417b5ccd03f43c" => @default_relays,
      "0f467b0603cd3229ad436399a2d4144965f16841767160296a0a7048a844c6f4" => @default_relays,
      "cefbf43addd677426c671d7cd275289be35f7b6b398fced7fae420d060e7a345" => @default_relays,
      "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef" => @default_relays,
      "0f479cb726c1578ca765d5ff6a0c58855263977d5d7cf7b4cea23d42d557c611" => @default_relays,
      "0f4791c38e1236dc55f11acbf37a00da8879906d9374498378db8d6ea7952869" => @default_relays,
      "f7721f8728935c943710a2f06288cbd56da7ab20b43400a16d26ac58880e0087" => @default_relays,
      "0f479ef1a8870a917afbae778ea75fedb5db4cb64501e0e64a6d2010a2908e64" => @default_relays
    }
  }

  @nip96_redirect %{
    "api_url" => "",
    "delegated_to_url" => "https://route96.pareto.space"
  }

  @empty_data %{
    "names" => %{},
    "relays" => %{}
  }

  def nip05(conn, %{"name" => name}) do
    conn = put_required_headers(conn)

    case Map.get(@nostr_data["names"], String.downcase(name)) do
      nil ->
        # return empty data if name is not found
        json(conn, @empty_data)

      pubkey ->
        data = %{
          "names" => %{name => pubkey},
          "relays" => %{pubkey => Map.get(@nostr_data["relays"], pubkey)}
        }

        json(conn, data)
    end
  end

  # call without specific name parameter
  def nip05(conn, _params) do
    conn
    |> put_required_headers()
    |> json(@empty_data)
  end

  def validate_nip05_handle(conn, %{"handle" => handle}) do
    conn =
      put_same_domain_headers(conn)

    case Nip05.parse_identifier(handle) do
      {:ok, name, domain} ->
        case Nip05.get_cached_well_known(name, domain) do
          {:ok, response} ->
            conn
            |> json(response)

          {:error, message} ->
            IO.inspect(message, label: "Error")

            conn
            |> put_status(:not_found)
            |> text(message)
        end

      {:error, message} ->
        conn
        |> put_status(:bad_request)
        |> text(message)
    end
  end

  def nip96(conn, _params) do
    conn
    |> put_required_headers()
    |> json(@nip96_redirect)
  end

  def get_nostr_data(), do: @nostr_data

  defp put_required_headers(conn) do
    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  end

  defp put_same_domain_headers(conn) do
    conn
    # change this to pareto.space in case the API endpoint is (mis)used by other Nostr applications
    #   |> put_resp_header("Access-Control-Allow-Origin", "pareto.space")
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  end
end
