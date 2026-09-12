defmodule NostrBackendWeb.ContentControllerSpamFighterTest do
  use NostrBackendWeb.ConnCase, async: false

  alias NostrBackend.NIP19
  alias NostrBackend.NostrId

  setup do
    originals = %{
      api_key: Application.get_env(:nostr_backend, :spam_fighter_api_key),
      fetch: Application.get_env(:nostr_backend, :spam_fighter_fetch)
    }

    naddr =
      NIP19.encode_naddr(
        30023,
        "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5",
        "spam-fighter-test",
        []
      )

    {:ok, {:author_article, query_data}} = NostrId.parse(naddr)

    on_exit(fn ->
      restore(:spam_fighter_api_key, originals.api_key)
      restore(:spam_fighter_fetch, originals.fetch)
      Cachex.del(:articles_cache, query_data)
    end)

    %{naddr: naddr, query_data: query_data}
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

  test "GET /a/:naddr skips NSF when article is already cached", %{
    conn: conn,
    naddr: naddr,
    query_data: query_data
  } do
    now = DateTime.utc_now()

    article = %{
      article_id: "cached-spam-fighter-test",
      kind: 30023,
      author: query_data.author,
      identifier: query_data.identifier,
      title: "Cached article",
      description: "Already in cache",
      content: "<p>hello</p>",
      image_url: nil,
      published_at: now,
      created_at: now,
      tags: []
    }

    assert {:ok, true} = Cachex.put(:articles_cache, query_data, article)

    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")

    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, _ ->
      flunk("Spam Fighter should not be called for a cached article")
    end)

    conn = get(conn, "/a/#{naddr}")

    assert conn.status == 200
    assert conn.resp_body =~ "Cached article"
  end

  defp restore(key, nil), do: Application.delete_env(:nostr_backend, key)
  defp restore(key, value), do: Application.put_env(:nostr_backend, key, value)
end
