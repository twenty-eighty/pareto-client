defmodule NostrBackend.NIP19_TLV do
  @moduledoc """
  Type-Length-Value (TLV) encoding for NIP-19 format.
  Used for constructing TLV data for nprofile, naddr, nevent identifiers.

  This ensures proper encoding of binary data according to the NIP-19 specification.
  """

  require Logger

  @doc """
  Create a TLV encoding from the given list of {type, value} tuples.

  ## Examples

      iex> make_tlv([{0, "pubkey_bin"}, {1, "relay_url"}])
      <<0, 9, "pubkey_bin", 1, 9, "relay_url">>
  """
  def make_tlv(entries) do
    Enum.reduce(entries, <<>>, fn {type, value}, acc ->
      add_to_tlv(acc, type, value)
    end)
  end

  @doc """
  Add a new TLV entry to an existing TLV binary.

  ## Examples

      iex> add_to_tlv(<<0, 3, "foo">>, 1, "bar")
      <<0, 3, "foo", 1, 3, "bar">>
  """
  def add_to_tlv(tlv_data, type, value) when is_integer(type) do
    value_bin = ensure_binary(value)
    tlv_data <> <<type::8, byte_size(value_bin)::8, value_bin::binary>>
  end

  # Ensure the value is a binary, with better handling for edge cases
  defp ensure_binary(value) when is_binary(value), do: value
  defp ensure_binary(value) when is_nil(value), do: ""
  defp ensure_binary(value) when is_integer(value), do: Integer.to_string(value)
  defp ensure_binary(value) when is_float(value), do: Float.to_string(value)
  defp ensure_binary(value) when is_atom(value), do: Atom.to_string(value)

  defp ensure_binary(value) when is_list(value) do
    case Enum.all?(value, fn c -> is_integer(c) and c >= 0 and c <= 0x10FFFF end) do
      # It's a char list (code points)
      true ->
        List.to_string(value)

      false ->
        Logger.warning(
          "Received a list that is not a charlist for TLV encoding: #{inspect(value)}",
          []
        )

        # Fallback to string representation
        inspect(value)
    end
  end

  defp ensure_binary(value) do
    Logger.warning(
      "Converting non-binary value to string for TLV encoding: #{inspect(value)}",
      []
    )

    to_string(value)
  rescue
    e ->
      Logger.error("Failed to convert value to binary: #{inspect(e)}, value: #{inspect(value)}")
      # Use inspect as a last resort
      inspect(value)
  end
end
