defmodule NostrBackendWeb.ImageCacheKeyControllerTest do
  use NostrBackendWeb.ConnCase, async: false

  setup do
    original_current = Application.get_env(:nostr_backend, :image_cache_key)
    original_previous = Application.get_env(:nostr_backend, :image_cache_key_previous)

    on_exit(fn ->
      restore(:image_cache_key, original_current)
      restore(:image_cache_key_previous, original_previous)
    end)

    :ok
  end

  test "GET /image-cache-key.json serves current and previous keys", %{conn: conn} do
    Application.put_env(:nostr_backend, :image_cache_key, "current-secret")
    Application.put_env(:nostr_backend, :image_cache_key_previous, "previous-secret")

    conn = get(conn, "/image-cache-key.json")

    assert json_response(conn, 200) == %{
             "current" => "current-secret",
             "previous" => "previous-secret"
           }

    assert get_resp_header(conn, "cache-control") == ["public, max-age=60"]
  end

  test "GET /image-cache-key.json allows empty keys", %{conn: conn} do
    Application.put_env(:nostr_backend, :image_cache_key, "")
    Application.put_env(:nostr_backend, :image_cache_key_previous, "")

    conn = get(conn, "/image-cache-key.json")

    assert json_response(conn, 200) == %{
             "current" => "",
             "previous" => ""
           }
  end

  defp restore(key, nil), do: Application.delete_env(:nostr_backend, key)
  defp restore(key, value), do: Application.put_env(:nostr_backend, key, value)
end
