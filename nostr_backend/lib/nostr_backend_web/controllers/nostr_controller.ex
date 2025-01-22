defmodule NostrBackendWeb.NostrController do
  use NostrBackendWeb, :controller

  alias NostrBackend.Nip05

  # Define the names and relays in a module attribute
  @nostr_data %{
    "names" => %{
      "milosz" => "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638",
      "ashoka" => "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5",
      "roland" => "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2",
      "donjoe" => "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe",
      "kalle" => "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518",
      "aron" => "9f94e6cc5ce50dcaccfc42b18029aba0ac9215d673197a40172896d3f3472946",
      "psychobabble" => "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f",
      "indikativ" => "1a040599c19734813abcec04d9bda0ff5fc5054fc4d035b79484bf970a05f5c4",
      "walter_siegrist" => "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c",
      "_" => "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84",
      "client" => "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5",
      "christof_weber" => "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066",
      "michael_meyen" => "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e",
      "janosch" => "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01",
      "j1000" => "135f20a6f142a3f8a6c3fde48772bd6cffece0fc3aa31bef6f12c99b3937e969",
      "hartmut" => "92af1031a8dc9fd1c2ef982219ff6cff9944ec62bd45d4c6e4e8d5ffd9939aeb"
    },
    "relays" => %{
      "2c917bfcfe4f3777ccacb4c968d6a3e9266d39a22db65c2cf2ca0c09fddf8638" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "e373ca4101e25a4d4fcb2a53473fa4113b91dba2c2e451d039d8528eb82abcc5" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "08d79c2e514edd4634ea92bbfe1ec089730a049216be9d28c77c4c1c7733f518" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "9f94e6cc5ce50dcaccfc42b18029aba0ac9215d673197a40172896d3f3472946" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "6734e11d8d67d9ca4dedb920f81182ded2bca918e3e0f3528bd5f4c4c7e34e8f" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "1a040599c19734813abcec04d9bda0ff5fc5054fc4d035b79484bf970a05f5c4" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "78c90fc422bdadafb4df0de5d1ed87133265f896949705bebc61aee44bff983c" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "a81a69992a8b7fff092bb39a6a335181c16eb37948f55b90f3c5d09f3c502c84" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com",
        "nos.lol",
        "relay.damus.io"
      ],
      "0f479c7dff7bb53dae53f3bb32ad1109edbb07ba562bdd5168044b3f4364e7b5" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "79271c81ef2fd4994c20d73b0555a3e58d7ee9caaa2328082e3a336de18d9066" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "044da3442a54bd55202b66ca0c4f5fd58cbb158b67f2fb067cc0467c073a8a0e" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "89bae92f9d9b0f6d97a300496cfb0b73c92a74c9675a724c0689975f8074dc01" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "135f20a6f142a3f8a6c3fde48772bd6cffece0fc3aa31bef6f12c99b3937e969" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ],
      "92af1031a8dc9fd1c2ef982219ff6cff9944ec62bd45d4c6e4e8d5ffd9939aeb" => [
        "wss://nostr.pareto.space",
        "wss://pareto.nostr1.com"
      ]
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

    case Map.get(@nostr_data["names"], name) do
      nil ->
        # return full fill if name is not found
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
    |> json(@nostr_data)
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
