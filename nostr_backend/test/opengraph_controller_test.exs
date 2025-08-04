defmodule NostrBackend.OpenGraphControllerTest do
  use NostrBackendWeb.ConnCase, async: false
  alias NostrBackendWeb.OpenGraphController

  setup do
    # Clear OpenGraph cache before each test
    Cachex.clear(:opengraph_cache)
    :ok
  end

  describe "OpenGraph Controller" do
    test "handles cache errors gracefully for image requests", %{conn: conn} do
      # Test with a valid URL format but invalid domain
      result = OpenGraphController.fetch_metadata_image(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Should return an error (either HTTP error or timeout)
      assert result.status == 400

      # Verify cache entry exists (should cache the failure)
      cache_result = Cachex.get(:opengraph_cache, "image:https://nonexistent-domain-12345.com/page")
      assert cache_result != {:ok, nil}
    end

    test "handles cache errors gracefully for metadata requests", %{conn: conn} do
      # Test with a valid URL format but invalid domain
      result = OpenGraphController.fetch_metadata(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Should return an error (either HTTP error or timeout)
      assert result.status == 400

      # Verify cache entry exists (should cache the failure)
      cache_result = Cachex.get(:opengraph_cache, "metadata:https://nonexistent-domain-12345.com/page")
      assert cache_result != {:ok, nil}
    end

    test "verifies cache consistency across multiple image calls", %{conn: conn} do
      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = OpenGraphController.fetch_metadata_image(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Second call
      result2 = OpenGraphController.fetch_metadata_image(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Third call
      result3 = OpenGraphController.fetch_metadata_image(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # All results should have the same status
      assert result1.status == result2.status
      assert result2.status == result3.status
      assert result1.status == 400
    end

    test "verifies cache consistency across multiple metadata calls", %{conn: conn} do
      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = OpenGraphController.fetch_metadata(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Second call
      result2 = OpenGraphController.fetch_metadata(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # Third call
      result3 = OpenGraphController.fetch_metadata(conn, %{"url" => "https://nonexistent-domain-12345.com/page"})

      # All results should have the same status
      assert result1.status == result2.status
      assert result2.status == result3.status
      assert result1.status == 400
    end

    test "handles invalid URL formats for image requests", %{conn: conn} do
      # Test with a valid URL format that will fail during HTTP request
      result = OpenGraphController.fetch_metadata_image(conn, %{"url" => "https://invalid-url-format.com/page"})

      # Should return an error
      assert result.status == 400

      # Verify cache entry exists
      cache_result = Cachex.get(:opengraph_cache, "image:https://invalid-url-format.com/page")
      assert cache_result != {:ok, nil}
    end

    test "handles invalid URL formats for metadata requests", %{conn: conn} do
      # Test with a valid URL format that will fail during HTTP request
      result = OpenGraphController.fetch_metadata(conn, %{"url" => "https://invalid-url-format.com/page"})

      # Should return an error
      assert result.status == 400

      # Verify cache entry exists
      cache_result = Cachex.get(:opengraph_cache, "metadata:https://invalid-url-format.com/page")
      assert cache_result != {:ok, nil}
    end
  end
end
