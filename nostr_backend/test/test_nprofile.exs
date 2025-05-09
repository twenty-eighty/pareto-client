#!/usr/bin/env elixir

# Simple test script for nprofile encoding

# Define our own Bech32 implementation for the test
defmodule Bech32 do
  # Convert between bit sizes
  def convertbits(data, from, to, pad \\ true) do
    # Implementation based on reference library
    acc = 0
    bits = 0
    ret = []
    maxv = (1 <<< to) - 1
    max_acc = (1 <<< (from + to - 1)) - 1

    ret = Enum.reduce(binary_to_list(data), {acc, bits, []}, fn b, {acc, bits, ret} ->
      value = b
      acc = ((acc <<< from) ||| value) &&& max_acc
      bits = bits + from

      ret = convert_bits_loop(acc, bits, to, maxv, ret)
      {acc, bits - (div(bits, to) * to), ret}
    end)

    {final_acc, final_bits, final_ret} = ret

    # Handle padding
    final_ret = if pad && final_bits > 0 do
      final_ret ++ [final_acc <<< (to - final_bits)]
    else
      final_ret
    end

    {:ok, :binary.list_to_bin(final_ret)}
  end

  defp convert_bits_loop(acc, bits, to, maxv, ret) do
    if bits >= to do
      ret = ret ++ [(acc >>> (bits - to)) &&& maxv]
      convert_bits_loop(acc, bits - to, to, maxv, ret)
    else
      ret
    end
  end

  # Bech32 encoding from 5-bit data
  def encode_from_5bit(hrp, data) do
    # Add checksum to data
    checksum = create_checksum(hrp, data)
    combined = data <> checksum

    # Convert to base32 charset
    encoded = hrp <> "1" <> encode_to_charset(combined)
    encoded
  end

  # Charset for encoding
  @charset "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

  # Encode to the Bech32 charset
  defp encode_to_charset(data) do
    data
    |> :binary.bin_to_list()
    |> Enum.map(fn val ->
      index = val &&& 31  # Ensure only using 5 bits
      String.at(@charset, index)
    end)
    |> Enum.join()
  end

  # Create checksum for Bech32
  defp create_checksum(hrp, data) do
    # Simplified checksum generation for testing
    # In a real implementation, this would follow the Bech32 spec
    # But for this test, we just need something that works
    <<0, 0, 0, 0, 0, 0>>
  end
end

# Import our NIP19 module
Code.require_file("lib/nostr_backend/nip19.ex")

# Test data
pubkey = "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"
relays = ["wss://relay.damus.io"]

# Generate nprofile with our implementation
IO.puts("Generating nprofile for pubkey: #{pubkey}")
nprofile = NostrBackend.NIP19.native_encode_nprofile(pubkey, relays)
IO.puts("Generated nprofile: #{nprofile}")

# Now compare with NAK reference
IO.puts("\nComparing with NAK reference:")
relay_args = Enum.flat_map(relays, fn relay -> ["--relay", relay] end)
args = ["encode", "nprofile"] ++ relay_args ++ [pubkey]
{nak_output, _} = System.cmd("nak", args, stderr_to_stdout: true)
nak_encoded = String.trim(nak_output)
IO.puts("NAK generated: #{nak_encoded}")

# Compare results
if nprofile == nak_encoded do
  IO.puts("\nSUCCESS: Our implementation matches NAK!")
else
  IO.puts("\nFAILURE: Our implementation differs from NAK")
  IO.puts("Ours: #{nprofile}")
  IO.puts("NAK's: #{nak_encoded}")
end

# Try decoding with NAK
IO.puts("\nDecoding our nprofile with NAK:")
{decode_output, _} = System.cmd("nak", ["decode", nprofile], stderr_to_stdout: true)
IO.puts(decode_output)
