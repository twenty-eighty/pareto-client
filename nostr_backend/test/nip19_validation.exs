#!/usr/bin/env elixir

# This script validates our NIP19 implementation against NAK's reference implementation
# It will help identify any discrepancies that need to be fixed

# First let's set up the environment so we can access our modules
Code.prepend_path("_build/dev/lib/nostr_backend/ebin")
Code.prepend_path("_build/dev/lib/bech32/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

# Import our NIP19 module
alias NostrBackend.NIP19

IO.puts("Environment set up, NIP19 module loaded")

# Sample data for validation
test_data = [
  # Test nprofile encoding
  %{
    type: :nprofile,
    pubkey: "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d",
    relays: ["wss://relay.damus.io", "wss://nos.lol"]
  },
  %{
    type: :nprofile,
    pubkey: "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245",
    relays: ["wss://relay.damus.io"]
  },
  %{
    type: :nprofile,
    pubkey: "9ec7a778167afb1d30c4833de9322da0c08ba4c57e27baa9e833c0210f5e3a81",
    relays: ["wss://nostr.wine", "wss://nos.lol", "wss://relay.damus.io"]
  },
  # Test naddr encoding
  %{
    type: :naddr,
    kind: 30023,
    pubkey: "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245",
    identifier: "1234567890",
    relays: ["wss://relay.damus.io"]
  },
  %{
    type: :naddr,
    kind: 30023,
    pubkey: "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d",
    identifier: "test_post",
    relays: ["wss://relay.damus.io", "wss://nos.lol"]
  }
]

# Function to run NAK encode command for nprofile
nak_encode_nprofile = fn pubkey, relays ->
  # Build NAK command for nprofile with correct format
  relay_args = Enum.flat_map(relays, fn relay -> ["--relay", relay] end)
  args = ["encode", "nprofile"] ++ relay_args ++ [pubkey]

  # Run NAK command
  case System.cmd("nak", args, stderr_to_stdout: true) do
    {output, 0} ->
      String.trim(output)

    {error, _} ->
      IO.puts("NAK ERROR: #{error}")
      nil
  end
end

# Function to run NAK encode command for naddr
nak_encode_naddr = fn kind, pubkey, identifier, relays ->
  # Build NAK command for naddr with correct format
  relay_args = Enum.flat_map(relays, fn relay -> ["--relay", relay] end)

  args =
    ["encode", "naddr", "--kind", to_string(kind), "--pubkey", pubkey, "--identifier", identifier] ++
      relay_args

  # Run NAK command
  case System.cmd("nak", args, stderr_to_stdout: true) do
    {output, 0} ->
      String.trim(output)

    {error, _} ->
      IO.puts("NAK ERROR: #{error}")
      nil
  end
end

# Function to validate our encoding against NAK
validate_encoding = fn test_item ->
  case test_item.type do
    :nprofile ->
      reference = nak_encode_nprofile.(test_item.pubkey, test_item.relays)
      # Call our NIP19 module implementation
      our_result = NIP19.encode_nprofile(test_item.pubkey, test_item.relays)

      IO.puts("\n== NPROFILE VALIDATION ==")
      IO.puts("Input: pubkey=#{test_item.pubkey}, relays=#{inspect(test_item.relays)}")
      IO.puts("NAK reference output: #{reference}")
      IO.puts("Our implementation: #{our_result}")

      # Check if our implementation matches NAK's reference
      is_valid = our_result == reference
      IO.puts("MATCH: #{is_valid}")

      if reference do
        # Decode the reference to inspect
        reference_decode_cmd = "nak decode #{reference}"
        {decode_output, _} = System.cmd("sh", ["-c", reference_decode_cmd])
        IO.puts("NAK decode output: #{decode_output}")
      end

    :naddr ->
      reference =
        nak_encode_naddr.(
          test_item.kind,
          test_item.pubkey,
          test_item.identifier,
          test_item.relays
        )

      # Call our NIP19 module implementation
      our_result =
        NIP19.encode_naddr(
          test_item.kind,
          test_item.pubkey,
          test_item.identifier,
          test_item.relays
        )

      IO.puts("\n== NADDR VALIDATION ==")

      IO.puts(
        "Input: kind=#{test_item.kind}, pubkey=#{test_item.pubkey}, identifier=#{test_item.identifier}, relays=#{inspect(test_item.relays)}"
      )

      IO.puts("NAK reference output: #{reference}")
      IO.puts("Our implementation: #{our_result}")

      # Check if our implementation matches NAK's reference
      is_valid = our_result == reference
      IO.puts("MATCH: #{is_valid}")

      if reference do
        # Decode the reference to inspect
        reference_decode_cmd = "nak decode #{reference}"
        {decode_output, _} = System.cmd("sh", ["-c", reference_decode_cmd])
        IO.puts("NAK decode output: #{decode_output}")
      end
  end
end

# Run validation for each test item
IO.puts("=== Starting NIP19 Validation ===")
Enum.each(test_data, validate_encoding)
IO.puts("=== Validation Complete ===")
