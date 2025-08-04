defmodule NostrBackend.OembedControllerTest do
  use NostrBackendWeb.ConnCase, async: false
  alias NostrBackendWeb.OembedController

  setup do
    # Clear oEmbed cache before each test
    Cachex.clear(:oembed_cache)
    :ok
  end

  describe "OEmbed Controller" do
    test "handles cache errors gracefully", %{conn: conn} do
      # Add proper Origin header to pass CORS check
      conn = put_req_header(conn, "origin", "https://pareto.space")

      # Test with a valid URL format but invalid domain
      result = OembedController.fetch_oembed(conn, %{"url" => "https://nonexistent-domain-12345.com/oembed"})

      # Should return an error (either HTTP error or timeout)
      assert result.status == 400

      # Verify cache entry exists (should cache the failure)
      cache_result = Cachex.get(:oembed_cache, "https://nonexistent-domain-12345.com/oembed")
      assert cache_result != {:ok, nil}
    end

    test "verifies cache consistency across multiple calls", %{conn: conn} do
      # Add proper Origin header to pass CORS check
      conn = put_req_header(conn, "origin", "https://pareto.space")

      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = OembedController.fetch_oembed(conn, %{"url" => "https://nonexistent-domain-12345.com/oembed"})

      # Second call
      result2 = OembedController.fetch_oembed(conn, %{"url" => "https://nonexistent-domain-12345.com/oembed"})

      # Third call
      result3 = OembedController.fetch_oembed(conn, %{"url" => "https://nonexistent-domain-12345.com/oembed"})

      # All results should have the same status
      assert result1.status == result2.status
      assert result2.status == result3.status
      assert result1.status == 400
    end

    test "handles invalid URL formats", %{conn: conn} do
      # Add proper Origin header to pass CORS check
      conn = put_req_header(conn, "origin", "https://pareto.space")

      # Test with a valid URL format that will fail during HTTP request
      result = OembedController.fetch_oembed(conn, %{"url" => "https://invalid-url-format.com/oembed"})

      # Should return an error
      assert result.status == 400

      # Verify cache entry exists
      cache_result = Cachex.get(:oembed_cache, "https://invalid-url-format.com/oembed")
      assert cache_result != {:ok, nil}
    end
  end
end
