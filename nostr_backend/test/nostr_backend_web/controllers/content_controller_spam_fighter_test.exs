defmodule NostrBackendWeb.ContentControllerSpamFighterTest do
  use NostrBackendWeb.ConnCase, async: false

  alias NostrBackend.NIP19

  setup do
    originals = %{
      api_key: Application.get_env(:nostr_backend, :spam_fighter_api_key),
      fetch: Application.get_env(:nostr_backend, :spam_fighter_fetch)
    }

    on_exit(fn ->
      restore(:spam_fighter_api_key, originals.api_key)
      restore(:spam_fighter_fetch, originals.fetch)
    end)

    naddr =
      NIP19.encode_naddr(
        30023,
        "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5",
        "spam-fighter-test",
        []
      )

    %{naddr: naddr}
  end

  test "GET /a/:naddr returns empty 404 when blacklisted", %{conn: conn, naddr: naddr} do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")
    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, ^naddr -> {:ok, true} end)

    conn = get(conn, "/a/#{naddr}")

    assert conn.status == 404
    assert conn.resp_body == ""
  end

  test "GET /a/:naddr does not suppress when check fails open", %{conn: conn, naddr: naddr} do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")
    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, ^naddr -> {:error, :timeout} end)

    conn = get(conn, "/a/#{naddr}")

    # Article may be missing from relays; either way we must not return empty 404 suppress.
    refute conn.status == 404 and conn.resp_body == ""
  end

  defp restore(key, nil), do: Application.delete_env(:nostr_backend, key)
  defp restore(key, value), do: Application.put_env(:nostr_backend, key, value)
end
