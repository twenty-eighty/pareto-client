defmodule NostrBackendWeb.NostrController do
  use NostrBackendWeb, :controller

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
      "indikativ" => "1a040599c19734813abcec04d9bda0ff5fc5054fc4d035b79484bf970a05f5c4"
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
      ]
    }
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

  def nip05(conn, _params) do
    conn
    |> put_required_headers()
    |> json(@nostr_data)
  end

  defp put_required_headers(conn) do
    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> put_resp_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  end
end
