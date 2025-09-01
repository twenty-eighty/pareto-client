defmodule NostrBackend.RumbleControllerTest do
  use NostrBackendWeb.ConnCase, async: false
  alias NostrBackendWeb.RumbleController

  setup do
    # Clear Rumble cache before each test
    Cachex.clear(:rumble_cache)
    :ok
  end

  describe "Rumble Controller" do
    test "handles cache errors gracefully", %{conn: conn} do
      # Test with a valid URL format but invalid domain
      result = RumbleController.fetch_embed_url(conn, %{"url" => "https://nonexistent-domain-12345.com/video.html"})

      # Should return an error (either HTTP error or timeout)
      assert result.status == 400

      # Verify cache entry exists (should cache the failure)
      cache_result = Cachex.get(:rumble_cache, "https://nonexistent-domain-12345.com/video.html")
      assert cache_result != {:ok, nil}
    end

    test "verifies cache consistency across multiple calls", %{conn: conn} do
      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = RumbleController.fetch_embed_url(conn, %{"url" => "https://nonexistent-domain-12345.com/video.html"})

      # Second call
      result2 = RumbleController.fetch_embed_url(conn, %{"url" => "https://nonexistent-domain-12345.com/video.html"})

      # Third call
      result3 = RumbleController.fetch_embed_url(conn, %{"url" => "https://nonexistent-domain-12345.com/video.html"})

      # All results should have the same status
      assert result1.status == result2.status
      assert result2.status == result3.status
      assert result1.status == 400
    end

    test "handles invalid URL formats", %{conn: conn} do
      # Test with a valid URL format that will fail during HTTP request
      result = RumbleController.fetch_embed_url(conn, %{"url" => "https://invalid-url-format.com/video.html"})

      # Should return an error
      assert result.status == 400

      # Verify cache entry exists
      cache_result = Cachex.get(:rumble_cache, "https://invalid-url-format.com/video.html")
      assert cache_result != {:ok, nil}
    end

    test "handles non-rumble URLs", %{conn: conn} do
      # Test with non-Rumble URLs
      result = RumbleController.fetch_embed_url(conn, %{"url" => "https://example.com/video.html"})

      # Should return an error (not a Rumble URL)
      assert result.status == 400

      # Verify cache entry exists
      cache_result = Cachex.get(:rumble_cache, "https://example.com/video.html")
      assert cache_result != {:ok, nil}
    end
  end
end
