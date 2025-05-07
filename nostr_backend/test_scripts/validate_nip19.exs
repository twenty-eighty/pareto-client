# Run with: mix run test_scripts/validate_nip19.exs

defmodule NIP19Test do
  alias NostrBackend.NIP19
  alias NostrBackend.NostrId
  require Logger

  def run do
    IO.puts("\n==== Testing NIP19 Encoding/Decoding ====\n")

    # Test pubkey (from original test case)
    pubkey_hex = "99e7936f129df8c6c826df78a3be975f166081902bde9046848cbf1ed5d2197c"
    relay = "wss://nostr.pareto.space"

    # Test npub encoding and decoding
    test_npub(pubkey_hex)

    # Test nprofile encoding and decoding (with and without relay)
    test_nprofile(pubkey_hex, [])
    test_nprofile(pubkey_hex, [relay])

    # Test the improved fallback case
    test_fallback_nprofile()
  end

  defp test_npub(pubkey_hex) do
    IO.puts("=== Testing npub encoding/decoding ===")

    # Encode pubkey as npub
    npub = NIP19.encode_npub(pubkey_hex)
    IO.puts("Encoded npub: #{npub}")

    # Try to decode the npub using NostrId
    case NostrId.validate_nip19(npub) do
      {:ok, {:pubkey, decoded_pubkey}} ->
        IO.puts("Decoded pubkey: #{decoded_pubkey}")
        if decoded_pubkey == pubkey_hex do
          IO.puts("✅ SUCCESS: Encoding/decoding roundtrip works!")
        else
          IO.puts("❌ FAILURE: Decoded pubkey doesn't match original")
          IO.puts("  Expected: #{pubkey_hex}")
          IO.puts("  Got: #{decoded_pubkey}")
        end

      {:error, reason} ->
        IO.puts("❌ FAILURE: Failed to decode npub: #{reason}")
    end
    IO.puts("")
  end

  defp test_nprofile(pubkey_hex, relays) do
    relay_info = if Enum.empty?(relays), do: "without relay", else: "with relay"
    IO.puts("=== Testing nprofile encoding/decoding (#{relay_info}) ===")

    # Encode pubkey as nprofile
    nprofile = NIP19.encode_nprofile(pubkey_hex, relays)
    IO.puts("Encoded nprofile: #{nprofile}")

    # Try to decode the nprofile using NostrId
    case NostrId.validate_nip19(nprofile) do
      {:ok, {:profile, decoded_pubkey, decoded_relays}} ->
        IO.puts("Decoded pubkey: #{decoded_pubkey}")
        IO.puts("Decoded relays: #{inspect(decoded_relays)}")

        if decoded_pubkey == pubkey_hex do
          IO.puts("✅ SUCCESS: Pubkey decoded correctly!")
        else
          IO.puts("❌ FAILURE: Decoded pubkey doesn't match original")
          IO.puts("  Expected: #{pubkey_hex}")
          IO.puts("  Got: #{decoded_pubkey}")
        end

        if relays == decoded_relays do
          IO.puts("✅ SUCCESS: Relays decoded correctly!")
        else
          IO.puts("❌ FAILURE: Decoded relays don't match original")
          IO.puts("  Expected: #{inspect(relays)}")
          IO.puts("  Got: #{inspect(decoded_relays)}")
        end

      {:error, reason} ->
        IO.puts("❌ FAILURE: Failed to decode nprofile: #{reason}")
    end
    IO.puts("")
  end

  defp test_fallback_nprofile() do
    IO.puts("=== Testing fallback nprofile encoding ===")

    # Create a random pubkey that won't match the test cases in the implementation
    random_pubkey_hex = :crypto.strong_rand_bytes(32) |> Base.encode16(case: :lower)
    relay = "wss://random.relay.com"

    # Encode with a random relay to ensure we hit the fallback case
    nprofile = NIP19.encode_nprofile(random_pubkey_hex, [relay])
    IO.puts("Encoded nprofile (should use fallback): #{nprofile}")

    # Try to decode the nprofile
    case NostrId.validate_nip19(nprofile) do
      {:ok, {:profile, decoded_pubkey, decoded_relays}} ->
        IO.puts("Decoded pubkey: #{decoded_pubkey}")
        IO.puts("Decoded relays: #{inspect(decoded_relays)}")

        if decoded_pubkey == random_pubkey_hex do
          IO.puts("✅ SUCCESS: Pubkey decoded correctly from fallback implementation!")
        else
          IO.puts("❌ FAILURE: Decoded pubkey doesn't match original")
          IO.puts("  Expected: #{random_pubkey_hex}")
          IO.puts("  Got: #{decoded_pubkey}")
        end

        if Enum.at(decoded_relays, 0) == relay do
          IO.puts("✅ SUCCESS: Relay decoded correctly from fallback implementation!")
        else
          IO.puts("❌ FAILURE: Decoded relay doesn't match original")
          IO.puts("  Expected: #{relay}")
          IO.puts("  Got: #{Enum.at(decoded_relays, 0)}")
        end

      {:error, reason} ->
        IO.puts("❌ FAILURE: Failed to decode fallback nprofile: #{reason}")
    end
  end
end

# Run the tests
NIP19Test.run()
