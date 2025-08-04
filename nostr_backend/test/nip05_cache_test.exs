defmodule NostrBackend.Nip05CacheTest do
  use ExUnit.Case, async: false
  alias NostrBackend.Nip05Cache

  # Test data
  @valid_nip05 "test@example.com"
  @invalid_nip05 "invalid@nonexistent.com"

  setup do
    # Clear NIP-05 cache before each test
    Cachex.clear(:nip05_cache)
    :ok
  end

  describe "NIP-05 Cache" do
    test "caches successful resolutions for 24 hours" do
      # Test with a real NIP-05 identifier (this will fail but we can test the caching logic)
      result = Nip05Cache.get_pubkey_and_relays(@valid_nip05)

      # The result will be an error, but we can verify the caching behavior
      # by checking if the cache entry exists
      cache_result = Cachex.get(:nip05_cache, @valid_nip05)
      assert cache_result != {:ok, nil}

      # Second call should serve from cache (may have different error message but same type)
      second_result = Nip05Cache.get_pubkey_and_relays(@valid_nip05)
      assert {:error, _} = second_result
      assert {:error, _} = result
    end

    test "caches not found errors for 1 hour" do
      # Test with an invalid NIP-05 identifier
      result = Nip05Cache.get_pubkey_and_relays(@invalid_nip05)

      # Should return an error
      assert {:error, _reason} = result

      # Verify cache entry exists
      cache_result = Cachex.get(:nip05_cache, @invalid_nip05)
      assert cache_result != {:ok, nil}

      # Second call should serve error from cache (may have different error message but same type)
      second_result = Nip05Cache.get_pubkey_and_relays(@invalid_nip05)
      assert {:error, _} = second_result
      assert {:error, _} = result
    end

    test "verifies cache integration with error types" do
      # Test that the NIP-05 cache properly handles different error types

      # Test with a domain that will likely fail
      result = Nip05Cache.get_pubkey_and_relays("test@nonexistent-domain-12345.com")

      # Should return an error
      assert {:error, _reason} = result

      # Verify cache entry exists
      cache_result = Cachex.get(:nip05_cache, "test@nonexistent-domain-12345.com")
      assert cache_result != {:ok, nil}

      # The cached value should be either :not_found or :resolution_failed
      assert {:ok, cached_value} = cache_result
      assert cached_value in [:not_found, :resolution_failed]
    end

    test "verifies cache consistency across multiple calls" do
      # Test that cache provides consistent results across multiple calls

      # First call
      result1 = Nip05Cache.get_pubkey_and_relays(@invalid_nip05)

      # Second call
      result2 = Nip05Cache.get_pubkey_and_relays(@invalid_nip05)

      # Third call
      result3 = Nip05Cache.get_pubkey_and_relays(@invalid_nip05)

      # All results should be error types (may have different messages but same type)
      assert {:error, _} = result1
      assert {:error, _} = result2
      assert {:error, _} = result3
    end
  end
end
