defmodule NostrBackend.Nip11ControllerTest do
  use NostrBackendWeb.ConnCase, async: false
  alias NostrBackendWeb.Nip11Controller

  setup do
    # Clear NIP-11 cache before each test
    Cachex.clear(:nip11_cache)
    :ok
  end

  describe "NIP-11 Controller" do
    test "caches invalid URL errors for 1 hour", %{conn: conn} do
      # Test with invalid URL
      result = Nip11Controller.fetch_nip11(conn, %{"url" => "invalid-url"})
      assert result.status == 400

      # Second call should serve error from cache
      cached_result = Nip11Controller.fetch_nip11(conn, %{"url" => "invalid-url"})
      assert cached_result.status == 400

      # Verify cache entry exists
      assert {:ok, :invalid_url} = Cachex.get(:nip11_cache, "invalid-url")
    end

    test "handles cache errors gracefully", %{conn: conn} do
      # Test with a valid URL format but invalid domain
      result = Nip11Controller.fetch_nip11(conn, %{"url" => "https://nonexistent-domain-12345.com"})

      # Should return an error (either HTTP error or timeout)
      assert result.status == 400

      # Verify cache entry exists (should cache the failure)
      cache_result = Cachex.get(:nip11_cache, "https://nonexistent-domain-12345.com")
      assert cache_result != {:ok, nil}
    end

    test "verifies cache consistency across multiple calls", %{conn: conn} do
      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = Nip11Controller.fetch_nip11(conn, %{"url" => "invalid-url"})

      # Second call
      result2 = Nip11Controller.fetch_nip11(conn, %{"url" => "invalid-url"})

      # Third call
      result3 = Nip11Controller.fetch_nip11(conn, %{"url" => "invalid-url"})

      # All results should have the same status
      assert result1.status == result2.status
      assert result2.status == result3.status
      assert result1.status == 400
    end
  end
end
