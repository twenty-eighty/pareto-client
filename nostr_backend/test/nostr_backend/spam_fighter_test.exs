defmodule NostrBackend.SpamFighterTest do
  use ExUnit.Case, async: false

  alias NostrBackend.SpamFighter

  @naddr "naddr1qqrxywpn8y6rwv3sxymkxwfpefex2uedxq3k2mrfv45x2ttzv9ekzmt9v9ekzmr9v9ekzmr9v9ekzmrpv9ekzmrpv9ekzmr9v9ekzmrpv9ekzmr9v9ekzmrpv"

  setup do
    originals = %{
      api_key: Application.get_env(:nostr_backend, :spam_fighter_api_key),
      url: Application.get_env(:nostr_backend, :spam_fighter_url),
      fetch: Application.get_env(:nostr_backend, :spam_fighter_fetch)
    }

    on_exit(fn ->
      restore(:spam_fighter_api_key, originals.api_key)
      restore(:spam_fighter_url, originals.url)
      restore(:spam_fighter_fetch, originals.fetch)
    end)

    :ok
  end

  test "skips check when API key is absent" do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "")
    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, _ -> flunk("should not fetch") end)

    refute SpamFighter.suppress_article?(@naddr)
  end

  test "suppresses when blacklisted is true" do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")

    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn base_url, api_key, naddr ->
      assert base_url =~ "nostr-spam-fighter"
      assert api_key == "nsf_test_key"
      assert naddr == @naddr
      {:ok, true}
    end)

    assert SpamFighter.suppress_article?(@naddr)
  end

  test "fails open when blacklisted is false" do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")
    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, _ -> {:ok, false} end)

    refute SpamFighter.suppress_article?(@naddr)
  end

  test "fails open when fetch errors" do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")
    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn _, _, _ -> {:error, :timeout} end)

    refute SpamFighter.suppress_article?(@naddr)
  end

  test "uses configured base URL" do
    Application.put_env(:nostr_backend, :spam_fighter_api_key, "nsf_test_key")
    Application.put_env(:nostr_backend, :spam_fighter_url, "https://example.test/")

    Application.put_env(:nostr_backend, :spam_fighter_fetch, fn base_url, _, _ ->
      assert base_url == "https://example.test"
      {:ok, false}
    end)

    refute SpamFighter.suppress_article?(@naddr)
  end

  defp restore(key, nil), do: Application.delete_env(:nostr_backend, key)
  defp restore(key, value), do: Application.put_env(:nostr_backend, key, value)
end
