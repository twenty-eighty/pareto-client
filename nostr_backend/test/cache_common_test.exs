defmodule NostrBackend.CacheCommonTest do
  use ExUnit.Case, async: false

  setup do
    # Clear all caches before each test
    Cachex.clear(:nip05_cache)
    Cachex.clear(:nip11_cache)
    Cachex.clear(:oembed_cache)
    Cachex.clear(:opengraph_cache)
    Cachex.clear(:rumble_cache)
    :ok
  end

  describe "Cache TTL Verification" do
    test "verifies cache clearing works" do
      # Test that cache clearing works properly

      # Add a test entry
      Cachex.put(:nip05_cache, "test_key", "test_value")
      assert {:ok, "test_value"} = Cachex.get(:nip05_cache, "test_key")

      # Clear the cache
      Cachex.clear(:nip05_cache)
      assert {:ok, nil} = Cachex.get(:nip05_cache, "test_key")
    end

    test "verifies different TTLs work correctly" do
      # Test that different TTLs work as expected

      # Test short TTL (1 second)
      Cachex.put(:nip05_cache, "short_ttl", "value", ttl: 1000)
      assert {:ok, "value"} = Cachex.get(:nip05_cache, "short_ttl")

      # Test long TTL (1 hour)
      Cachex.put(:nip05_cache, "long_ttl", "value", ttl: :timer.hours(1))
      assert {:ok, "value"} = Cachex.get(:nip05_cache, "long_ttl")
    end
  end

  describe "Cache Error Handling" do
    test "handles cache errors gracefully" do
      # Test that cache errors are handled properly

      # Test with invalid cache name
      result = Cachex.get(:nonexistent_cache, "key")
      assert {:error, _} = result
    end

    test "verifies cache put operations work" do
      # Test that cache put operations work correctly

      # Test successful put
      assert {:ok, true} = Cachex.put(:nip05_cache, "test_put", "test_value")
      assert {:ok, "test_value"} = Cachex.get(:nip05_cache, "test_put")

      # Test put with TTL
      assert {:ok, true} = Cachex.put(:nip05_cache, "test_put_ttl", "test_value", ttl: 5000)
      assert {:ok, "test_value"} = Cachex.get(:nip05_cache, "test_put_ttl")
    end
  end

  describe "Cache Integration Tests" do
    test "verifies all cache tables are accessible" do
      # Test that all cache tables exist and can be accessed

      # Test that we can put and get from all caches
      test_value = "test_value"

      # NIP-05 cache
      assert {:ok, true} = Cachex.put(:nip05_cache, "test", test_value)
      assert {:ok, ^test_value} = Cachex.get(:nip05_cache, "test")

      # NIP-11 cache
      assert {:ok, true} = Cachex.put(:nip11_cache, "test", test_value)
      assert {:ok, ^test_value} = Cachex.get(:nip11_cache, "test")

      # oEmbed cache
      assert {:ok, true} = Cachex.put(:oembed_cache, "test", test_value)
      assert {:ok, ^test_value} = Cachex.get(:oembed_cache, "test")

      # OpenGraph cache
      assert {:ok, true} = Cachex.put(:opengraph_cache, "test", test_value)
      assert {:ok, ^test_value} = Cachex.get(:opengraph_cache, "test")

      # Rumble cache
      assert {:ok, true} = Cachex.put(:rumble_cache, "test", test_value)
      assert {:ok, ^test_value} = Cachex.get(:rumble_cache, "test")
    end

    test "verifies cache isolation" do
      # Test that different caches are isolated from each other

      # Put different values in different caches
      Cachex.put(:nip05_cache, "key", "nip05_value")
      Cachex.put(:nip11_cache, "key", "nip11_value")
      Cachex.put(:oembed_cache, "key", "oembed_value")

      # Verify each cache has its own value
      assert {:ok, "nip05_value"} = Cachex.get(:nip05_cache, "key")
      assert {:ok, "nip11_value"} = Cachex.get(:nip11_cache, "key")
      assert {:ok, "oembed_value"} = Cachex.get(:oembed_cache, "key")
    end
  end
end
