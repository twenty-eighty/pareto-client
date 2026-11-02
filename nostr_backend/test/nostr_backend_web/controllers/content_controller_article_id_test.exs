defmodule NostrBackendWeb.ContentControllerArticleIdTest do
  use NostrBackendWeb.ConnCase, async: false

  alias NostrBackend.{NIP19, NostrId}

  @pubkey "866e013908559f15c5eff9d1295453082f01a1fb5f40a25bcf0776a36a9334e5"

  test "GET /a/:article_id renders a cached naddr article", %{conn: conn} do
    naddr = NIP19.encode_naddr(30023, @pubkey, "article-id-test")
    {:ok, {:author_article, query_data}} = NostrId.parse(naddr)
    now = DateTime.utc_now()

    article = %{
      article_id: "article-id-test",
      kind: 30023,
      author: @pubkey,
      identifier: "article-id-test",
      title: "Article ID regression test",
      description: "Cached article",
      content: "<p>hello</p>",
      image_url: nil,
      published_at: now,
      created_at: now,
      tags: []
    }

    on_exit(fn -> Cachex.del(:articles_cache, query_data) end)
    assert {:ok, true} = Cachex.put(:articles_cache, query_data, article)

    conn = get(conn, "/a/#{naddr}")

    assert conn.status == 200
    assert conn.resp_body =~ "Article ID regression test"
  end

  test "GET /a/:article_id returns 404 for a valid npub", %{conn: conn} do
    conn = get(conn, "/a/#{NIP19.encode_npub(@pubkey)}")

    assert conn.status == 404
    assert conn.resp_body =~ "Article/Profile Not Found"
  end

  test "GET /a/:article_id returns 404 for a valid nprofile", %{conn: conn} do
    conn = get(conn, "/a/#{NIP19.encode_nprofile(@pubkey, [])}")

    assert conn.status == 404
    assert conn.resp_body =~ "Article/Profile Not Found"
  end

  test "GET /a/:article_id returns 404 for a checksum-valid nprofile without a pubkey", %{
    conn: conn
  } do
    tlv = <<1, 7, "wss://x">>
    nprofile = Bech32.encode("nprofile", tlv)
    assert {:ok, "nprofile", ^tlv} = Bech32.decode(nprofile)

    conn = get(conn, "/a/#{nprofile}")

    assert conn.status == 404
    assert conn.resp_body =~ "Article/Profile Not Found"
  end

  test "GET /a/:article_id returns 404 for a malformed ID", %{conn: conn} do
    conn = get(conn, "/a/not-a-nostr-id")

    assert conn.status == 404
    assert conn.resp_body =~ "Article/Profile Not Found"
  end
end
