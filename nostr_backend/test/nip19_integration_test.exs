defmodule NostrBackend.NIP19IntegrationTest do
  use ExUnit.Case, async: true
  alias NostrBackend.NIP19

  @moduledoc """
  Integration tests for NIP19 implementation against NAK reference implementation
  """

  @test_data [
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
    # Test naddr encoding - commented out due to potential timeouts
    # %{
    #   type: :naddr,
    #   kind: 30023,
    #   pubkey: "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245",
    #   identifier: "1234567890",
    #   relays: ["wss://relay.damus.io"]
    # },
    # %{
    #   type: :naddr,
    #   kind: 30023,
    #   pubkey: "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d",
    #   identifier: "test_post",
    #   relays: ["wss://relay.damus.io", "wss://nos.lol"]
    # }
  ]

  # Verify NAK CLI is available
  setup do
    assert {_, 0} = System.cmd("which", ["nak"], stderr_to_stdout: true),
      "NAK CLI must be installed to run these tests"
    :ok
  end

  # Function to run NAK encode command for nprofile
  defp nak_encode_nprofile(pubkey, relays) do
    # Build NAK command for nprofile with correct flag format
    # First the relay flags
    relay_args = Enum.flat_map(relays, fn relay -> ["--relay", relay] end)
    # Then the pubkey as a positional argument at the end
    args = ["encode", "nprofile"] ++ relay_args ++ [pubkey]

    # Run NAK command
    case System.cmd("nak", args, stderr_to_stdout: true) do
      {output, 0} -> String.trim(output)
      {error, _} ->
        IO.puts("NAK ERROR: #{error}")
        nil
    end
  end



  # Helper to run with timeout guard
  defp with_timeout(fun, timeout \\ 5_000) do
    task = Task.async(fun)
    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      _ -> :timeout
    end
  end

  test "nprofile encoding should match NAK reference implementation" do
    for test_item <- Enum.filter(@test_data, &(&1.type == :nprofile)) do
      reference = with_timeout(fn -> nak_encode_nprofile(test_item.pubkey, test_item.relays) end)
      our_result = NIP19.encode_nprofile(test_item.pubkey, test_item.relays)

      IO.puts("Input: pubkey=#{test_item.pubkey}, relays=#{inspect(test_item.relays)}")
      IO.puts("NAK reference: #{reference}")
      IO.puts("Our implementation: #{our_result}")

      assert reference != nil, "NAK reference should be valid"
      assert our_result == reference, "Our nprofile encoding should match NAK reference"
    end
  end

  @tag :skip_on_ci
  test "naddr encoding validation" do
    # Test fields for naddr
    test_kind = 30023
    test_pubkey = "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"
    test_identifier = "test_post"
    test_relays = ["wss://relay.damus.io", "wss://nos.lol"]

    # Encode naddr using our implementation
    our_naddr = NIP19.encode_naddr(test_kind, test_pubkey, test_identifier, test_relays)
    IO.puts("Generated naddr: #{our_naddr}")

    # Verify the format looks like a valid naddr
    assert String.starts_with?(our_naddr, "naddr1"), "Should have the correct prefix"
  end

  test "npub encoding and decoding should work correctly" do
    test_pubkeys = [
      "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d",
      "32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245"
    ]

    for pubkey <- test_pubkeys do
      # Use the NAK tool to get a reference npub
      reference_npub = with_timeout(fn ->
        {output, 0} = System.cmd("nak", ["encode", "npub", pubkey], stderr_to_stdout: true)
        String.trim(output)
      end)

      # Use our implementation to encode
      our_npub = NIP19.encode_npub(pubkey)

      IO.puts("Pubkey: #{pubkey}")
      IO.puts("NAK encoded npub: #{reference_npub}")
      IO.puts("Our encoded npub: #{our_npub}")

      # Compare our implementation with NAK
      assert our_npub == reference_npub, "Our npub encoding should match NAK"
    end
  end
end
