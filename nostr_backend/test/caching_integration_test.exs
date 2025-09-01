defmodule NostrBackend.CachingIntegrationTest do
  use ExUnit.Case, async: false

  # This module serves as an overview and summary of all caching tests.
  # Individual test modules are:
  # - NostrBackend.Nip05CacheTest
  # - NostrBackend.Nip11ControllerTest
  # - NostrBackend.OembedControllerTest
  # - NostrBackend.OpenGraphControllerTest
  # - NostrBackend.RumbleControllerTest
  # - NostrBackend.CacheCommonTest

  describe "Caching System Overview" do
    test "verifies all cache modules are loaded" do
      # This test ensures all cache-related modules are available
      assert Code.ensure_loaded(NostrBackend.Nip05Cache)
      assert Code.ensure_loaded(NostrBackendWeb.Nip11Controller)
      assert Code.ensure_loaded(NostrBackendWeb.OembedController)
      assert Code.ensure_loaded(NostrBackendWeb.OpenGraphController)
      assert Code.ensure_loaded(NostrBackendWeb.RumbleController)
    end

    test "verifies cache configuration is correct" do
      # Test that all required caches are configured in the application
      # This is a basic smoke test to ensure the caching system is properly set up

      # Check that we can access all cache tables
      assert {:ok, _} = Cachex.get(:nip05_cache, "test")
      assert {:ok, _} = Cachex.get(:nip11_cache, "test")
      assert {:ok, _} = Cachex.get(:oembed_cache, "test")
      assert {:ok, _} = Cachex.get(:opengraph_cache, "test")
      assert {:ok, _} = Cachex.get(:rumble_cache, "test")
    end
  end
end
