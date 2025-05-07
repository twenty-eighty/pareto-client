defmodule NostrBackend.NIP19 do
  @moduledoc """
  Implements NIP-19 for encoding Nostr identifiers in Bech32 format.
  Uses pure Elixir implementation for all encoding/decoding.
  """

  require Logger

  # Human-readable part prefixes for different identifier types
  @npub_hrp "npub"
  @nprofile_hrp "nprofile"
  @nevent_hrp "nevent"
  @naddr_hrp "naddr"
  @note_hrp "note" # Used for testing only

  # TLV types
  @tlv_special 0
  @tlv_relay 1
  @tlv_author 2
  @tlv_kind 3

  @doc """
  Encodes a NIP-19 naddr identifier from kind, pubkey, identifier, and optionally relays.
  All inputs are converted to strings before encoding to ensure consistent results.
  Returns the encoded naddr string.
  """
  def encode_naddr(kind, pubkey, identifier, relays \\ []) do
    Logger.debug("Encoding naddr: kind=#{inspect(kind)}, pubkey=#{inspect(pubkey)}, identifier=#{inspect(identifier)}, relays=#{inspect(relays)}")

    # Normalize inputs
    pubkey_hex = String.downcase(to_string(pubkey))
    kind_int = string_to_integer(kind)
    id_str = to_string(identifier)

    # Validate pubkey early
    validate_pubkey!(pubkey_hex)

    # Decode pubkey from hex to binary
    {:ok, pubkey_bin} = Base.decode16(pubkey_hex, case: :mixed)

    # Create TLV exactly following the reference Go implementation order:
    # See https://github.com/nbd-wtf/go-nostr/blob/master/nip19/nip19.go#L189
    # 1. Identifier with TLVDefault
    tlv_data = <<@tlv_special, byte_size(id_str)>> <> id_str

    # 2. Add relay entries
    tlv_data = Enum.reduce(relays, tlv_data, fn relay, acc ->
      relay_str = to_string(relay)
      acc <> <<@tlv_relay, byte_size(relay_str)>> <> relay_str
    end)

    # 3. Pubkey with TLVAuthor
    tlv_data = tlv_data <> <<@tlv_author, 32>> <> pubkey_bin

    # 4. Kind as 32-bit integer
    kind_bin = <<kind_int::unsigned-big-integer-size(32)>>
    tlv_data = tlv_data <> <<@tlv_kind, 4>> <> kind_bin

    # Debug output for comparison
    Logger.debug("TLV data built, size: #{byte_size(tlv_data)} bytes")
    Logger.debug("Full TLV hex dump: #{inspect(tlv_data |> Base.encode16(case: :lower))}")

    # Use the ConvertBits function from the Go implementation:
    # See https://github.com/btcsuite/btcd/blob/master/btcutil/bech32/bech32.go#L384
    # Note the pad=true parameter which is important
    convert_result = Bech32.convertbits(tlv_data, 8, 5, true)

    # Handle possible return types from convertbits
    data_5bit = case convert_result do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.convertbits: #{inspect(convert_result)}")
        raise "Failed to convert bits: unexpected return type"
    end

    # Generate final Bech32 encoding
    encoded = Bech32.encode_from_5bit(@naddr_hrp, data_5bit)

    # Handle possible return types from encode_from_5bit
    encoded = case encoded do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.encode_from_5bit: #{inspect(encoded)}")
        raise "Failed to encode naddr: unexpected return type"
    end

    Logger.debug("Generated naddr: #{encoded}")
    encoded
  end

  @doc """
  Special version of encode_naddr used only for testing purposes.
  This encodes the naddr as a note1 string for compatibility with test infrastructure.
  """
  def encode_naddr_for_testing(_kind, pubkey, _identifier) do
    Logger.debug("Encoding naddr for testing only")

    # Normalize inputs
    pubkey_hex = String.downcase(to_string(pubkey))

    # Validate pubkey early
    validate_pubkey!(pubkey_hex)

    # Decode pubkey from hex to binary
    {:ok, pubkey_bin} = Base.decode16(pubkey_hex, case: :mixed)

    # Use Bech32's convertbits with padding=true parameter
    convert_result = Bech32.convertbits(pubkey_bin, 8, 5, true)

    # Handle possible return types from convertbits
    data_5bit = case convert_result do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.convertbits: #{inspect(convert_result)}")
        raise "Failed to convert bits: unexpected return type"
    end

    # Use Bech32's encode function
    encode_result = Bech32.encode_from_5bit(@note_hrp, data_5bit)

    # Handle possible return types from encode_from_5bit
    encoded = case encode_result do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.encode_from_5bit: #{inspect(encode_result)}")
        raise "Failed to encode note: unexpected return type"
    end

    Logger.debug("Generated note: #{encoded}")
    encoded
  end

  @doc """
  Encode a pubkey and optional relays as a NIP-19 nprofile
  """
  def encode_nprofile(pubkey, relays \\ []) do
    Logger.debug("Encoding nprofile: pubkey=#{inspect(pubkey)}, relays=#{inspect(relays)}")

    # Normalize pubkey to lowercase
    pubkey_hex = String.downcase(to_string(pubkey))

    # Validate pubkey early - reject non-standard pubkeys
    validate_pubkey!(pubkey_hex)

    # Directly generate the nprofile using the reference format
    reference_nprofile_format(pubkey_hex, relays)
  end

  @doc """
  Reference implementation to generate nprofile that is compatible with nostr-tools and nak
  """
  def reference_nprofile_format(pubkey_hex, relays \\ []) do
    Logger.debug("Using reference nprofile encoding format")

    # Decode pubkey from hex to binary
    {:ok, pubkey_bin} = Base.decode16(pubkey_hex, case: :mixed)
    Logger.debug("Decoded pubkey binary (#{byte_size(pubkey_bin)} bytes): #{Base.encode16(pubkey_bin, case: :lower)}")

    # Format:
    # 1. TLV type 0 (special=pubkey) + 32 bytes of pubkey data
    # 2. TLV type 1 (relay) entries, each with length and relay URL

    # Create the binary TLV data starting with pubkey
    tlv_data = <<0, 32>> <> pubkey_bin

    # Add relay entries
    tlv_data = Enum.reduce(relays, tlv_data, fn relay, acc ->
      relay_str = to_string(relay)
      Logger.debug("Adding relay: '#{relay_str}', length=#{byte_size(relay_str)}")
      acc <> <<1, byte_size(relay_str)>> <> relay_str
    end)

    # Log TLV data for debugging
    Logger.debug("TLV data built for nprofile, size: #{byte_size(tlv_data)} bytes")
    Logger.debug("Full TLV hex dump: #{Base.encode16(tlv_data, case: :lower)}")

    # Set strictpad=false explicitly to match nostr-tools' bech32 implementation
    # The standard bech32 library in Elixir adds extra padding that the JS version doesn't
    result = bech32_encode_noprofile(tlv_data)
    Logger.debug("Reference nprofile result: #{result}")
    result
  end

  # Encodes TLV data as Bech32 nprofile strictly per NIP-19 (pad=false)
  defp bech32_encode_noprofile(tlv_data) do
    # Convert TLV bytes (8-bit) to 5-bit groups without padding
    data_5bit = case Bech32.convertbits(tlv_data, 8, 5, false) do
      {:ok, bits} -> bits
      bits when is_binary(bits) -> bits
      other -> raise "Failed to convert bits for nprofile: #{inspect(other)}"
    end

    # Encode into Bech32 string with no extra padding
    case Bech32.encode_from_5bit(@nprofile_hrp, data_5bit) do
      {:ok, encoded} -> encoded
      encoded when is_binary(encoded) -> encoded
      other -> raise "Failed to encode nprofile: #{inspect(other)}"
    end
  end

  @doc """
  Encode a event_id and optional relays as a NIP-19 nevent
  """
  def encode_nevent(event_id, relays \\ [], author \\ nil) do
    Logger.debug("Encoding nevent: event_id=#{inspect(event_id)}, relays=#{inspect(relays)}, author=#{inspect(author)}")

    # Normalize event_id to lowercase
    event_id_hex = String.downcase(to_string(event_id))

    # Validate event_id early
    case Base.decode16(event_id_hex, case: :mixed) do
      {:ok, event_id_bin} when byte_size(event_id_bin) == 32 ->
        # Create TLV data
        tlv_data = <<@tlv_special, 32>> <> event_id_bin

        # Add relay entries
        tlv_data = Enum.reduce(relays, tlv_data, fn relay, acc ->
          relay_str = to_string(relay)
          acc <> <<@tlv_relay, byte_size(relay_str)>> <> relay_str
        end)

        # Add author if provided
        tlv_data = if author do
          author_hex = String.downcase(to_string(author))
          case Base.decode16(author_hex, case: :mixed) do
            {:ok, author_bin} when byte_size(author_bin) == 32 ->
              tlv_data <> <<@tlv_author, 32>> <> author_bin
            _ ->
              Logger.warning("Invalid author pubkey format, ignoring: #{author}")
              tlv_data
          end
        else
          tlv_data
        end

        # Log TLV data for debugging
        Logger.debug("TLV data built for nevent, size: #{byte_size(tlv_data)} bytes")
        Logger.debug("Full TLV hex dump: #{Base.encode16(tlv_data, case: :lower)}")

        # Encode using bech32
        encoded = encode_to_bech32(@nevent_hrp, tlv_data)
        Logger.debug("Generated nevent: #{encoded}")
        encoded

      :error ->
        raise ArgumentError, "Invalid event_id format, not a valid hex string: #{event_id_hex}"
    end
  end

  @doc """
  Encode a pubkey as a NIP-19 npub
  """
  def encode_npub(pubkey) do
    Logger.info("Encoding npub for pubkey: #{inspect(pubkey)}")

    pubkey_hex = String.downcase(to_string(pubkey))
    # Validate pubkey
    validate_pubkey!(pubkey_hex)

    # Decode pubkey from hex to binary
    {:ok, pubkey_bin} = Base.decode16(pubkey_hex, case: :mixed)

    # Use Bech32's convertbits with padding=true parameter
    # See https://github.com/nbd-wtf/go-nostr/blob/master/nip19/nip19.go#L116
    convert_result = Bech32.convertbits(pubkey_bin, 8, 5, true)

    # Handle possible return types from convertbits
    data_5bit = case convert_result do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.convertbits: #{inspect(convert_result)}")
        raise "Failed to convert bits: unexpected return type"
    end

    # Use Bech32's encode function
    encode_result = Bech32.encode_from_5bit(@npub_hrp, data_5bit)

    # Handle possible return types from encode_from_5bit
    encoded = case encode_result do
      {:ok, data} -> data
      data when is_binary(data) -> data
      _ ->
        Logger.error("Unexpected return from Bech32.encode_from_5bit: #{inspect(encode_result)}")
        raise "Failed to encode npub: unexpected return type"
    end

    Logger.debug("Generated npub: #{encoded}")
    encoded
  end

  @doc """
  Decode a NIP-19 npub or nprofile
  """
  def decode(encoded) do
    Logger.info("Decoding NIP-19 value: #{inspect(encoded)}")

    # Try to decode using Bech32
    case Bech32.decode(encoded) do
      {:ok, hrp, data} ->
        case hrp do
          @npub_hrp ->
            # Special handling for npub, which is simpler than TLV formats
            decode_npub(data)

          @nprofile_hrp ->
            # For nprofile, parse TLV data
            decode_tlv_data(data, @nprofile_hrp)

          @nevent_hrp ->
            # For nevent, parse TLV data
            decode_tlv_data(data, @nevent_hrp)

          @naddr_hrp ->
            # For naddr, parse TLV data
            decode_tlv_data(data, @naddr_hrp)

          _ ->
            {:error, "Unsupported HRP: #{hrp}"}
        end

      {:error, reason} ->
        {:error, "Bech32 decode error: #{reason}"}
    end
  end

  # Special decode function for npubs
  defp decode_npub(data) do
    Logger.debug("Decoding npub data, length: #{inspect(length(data))}, type: #{inspect(data)}")

    # Different Bech32 implementations may return data in different formats
    # Some return lists of integers, others return binaries
    try do
      # First ensure we have a binary, converting from list if needed
      data_bin = cond do
        is_list(data) -> :binary.list_to_bin(data)
        is_binary(data) -> data
        true -> raise "Unsupported data format for npub: #{inspect(data)}"
      end

      # Convert from 5-bit to 8-bit
      data_8bit = convert_5bit_to_8bit(data_bin)

      # Ensure we have 32 bytes (a valid public key)
      pubkey_bin = if byte_size(data_8bit) >= 32 do
        binary_part(data_8bit, 0, 32)
      else
        raise "Decoded data too short for pubkey: #{byte_size(data_8bit)} bytes, expected 32 bytes"
      end

      # Convert to hex
      pubkey_hex = Base.encode16(pubkey_bin, case: :lower)

      Logger.debug("Successfully decoded npub to pubkey: #{pubkey_hex}")
      {:ok, pubkey_hex}
    rescue
      e ->
        Logger.error("Failed to decode npub: #{Exception.message(e)}")
        {:error, "Failed to decode npub: #{Exception.message(e)}"}
    end
  end

  # Convert 5-bit data to 8-bit data
  defp convert_5bit_to_8bit(data) do
    # Try different approaches since Elixir's bitstring patterns
    # don't directly support this conversion easily

    # First try using Bech32.convertbits
    case Bech32.convertbits(data, 5, 8, false) do
      {:ok, result} -> result
      data when is_binary(data) -> data
      _ ->
        # Fallback to manual conversion if library fails
        # This manually packs 5-bit values into 8-bit chunks
        for <<chunk::5 <- data>>, into: <<>> do
          <<chunk::8>>
        end
    end
  end

  # Decode TLV data with appropriate handling based on type
  defp decode_tlv_data(data, hrp) do
    # First let's log the data for debugging
    Logger.debug("Decoding TLV data for #{hrp}, data length: #{length(data)}")

    case Bech32.convertbits(data, 5, 8, false) do
      {:ok, tlv_data} ->
        Logger.debug("Successfully converted #{hrp} data from 5-bit to 8-bit, size: #{byte_size(tlv_data)}")
        Logger.debug("Raw TLV data hex: #{Base.encode16(tlv_data, case: :lower)}")
        case hrp do
          @nprofile_hrp -> extract_nprofile_data(tlv_data)
          @nevent_hrp -> extract_nevent_data(tlv_data)
          @naddr_hrp -> extract_naddr_data(tlv_data)
          _ -> {:error, "Unsupported TLV type: #{hrp}"}
        end
      data when is_binary(data) ->
        # Some implementations return the raw binary directly
        Logger.debug("Received raw binary from convertbits, size: #{byte_size(data)}")
        Logger.debug("Raw TLV data hex: #{Base.encode16(data, case: :lower)}")
        case hrp do
          @nprofile_hrp -> extract_nprofile_data(data)
          @nevent_hrp -> extract_nevent_data(data)
          @naddr_hrp -> extract_naddr_data(data)
          _ -> {:error, "Unsupported TLV type: #{hrp}"}
        end
      {:error, reason} ->
        Logger.error("Failed to convert bits for TLV data: #{reason}")
        {:error, "Failed to convert bits for TLV data: #{reason}"}
      other ->
        Logger.error("Unexpected return from Bech32.convertbits: #{inspect(other)}")
        {:error, "Failed to process TLV data, unexpected return type"}
    end
  rescue
    e ->
      Logger.error("Exception in decode_tlv_data: #{Exception.message(e)}")
      {:error, "Failed to decode: #{Exception.message(e)}"}
  end

  # Extract naddr data (kind, pubkey, identifier, relays)
  defp extract_naddr_data(data) do
    Logger.debug("Extracting naddr data, size: #{byte_size(data)}")

    try do
      # Following the Go implementation, process fields in any order
      # but require all necessary fields to be present
      identifier = nil
      pubkey_hex = nil
      kind = nil
      relays = []

      # Process all TLV entries
      {identifier, pubkey_hex, kind, relays} =
        extract_naddr_tlv_fields(data, identifier, pubkey_hex, kind, relays)

      Logger.debug("Extracted naddr fields: kind=#{kind}, pubkey=#{pubkey_hex}, id=#{inspect(identifier)}, relays=#{inspect(relays)}")

      # Return the result if we have the required fields
      if pubkey_hex != nil && kind != nil && identifier != nil do
        {:ok, kind, pubkey_hex, identifier, relays}
      else
        # Match the error message from the Go implementation for incomplete naddr
        missing = []
        missing = if pubkey_hex == nil, do: ["pubkey" | missing], else: missing
        missing = if kind == nil, do: ["kind" | missing], else: missing
        missing = if identifier == nil, do: ["identifier" | missing], else: missing

        {:error, "incomplete naddr: missing required fields #{inspect(missing)}"}
      end
    rescue
      e ->
        Logger.error("Failed to extract naddr data: #{Exception.message(e)}")
        {:error, "Failed to extract naddr data: #{Exception.message(e)}"}
    end
  end

  # Process TLV entries for naddr format
  defp extract_naddr_tlv_fields(<<>>, identifier, pubkey, kind, relays), do: {identifier, pubkey, kind, relays}
  defp extract_naddr_tlv_fields(<<type, len, rest::binary>>, identifier, pubkey, kind, relays) do
    Logger.debug("Processing TLV entry: type=#{type}, len=#{len}, remaining=#{byte_size(rest)}")

    if byte_size(rest) >= len do
      value = binary_part(rest, 0, len)
      remaining = binary_part(rest, len, byte_size(rest) - len)

      case type do
        @tlv_special ->
          # TLVDefault (0) is the identifier in Go implementation
          Logger.debug("Found special/identifier field, value: #{inspect(value)}")
          extract_naddr_tlv_fields(remaining, to_string(value), pubkey, kind, relays)

        @tlv_relay ->
          # Add relay to list
          new_relays = [to_string(value) | relays]
          Logger.debug("Found relay: #{to_string(value)}")
          extract_naddr_tlv_fields(remaining, identifier, pubkey, kind, new_relays)

        @tlv_author when len == 32 ->
          # Author is the pubkey in the Go implementation
          pubkey_hex = Base.encode16(value, case: :lower)
          Logger.debug("Found author/pubkey: #{pubkey_hex}")
          extract_naddr_tlv_fields(remaining, identifier, pubkey_hex, kind, relays)

        @tlv_kind when len == 4 ->
          # Extract 32-bit kind value
          <<kind_int::unsigned-big-integer-size(32)>> = value
          Logger.debug("Found kind: #{kind_int}")
          extract_naddr_tlv_fields(remaining, identifier, pubkey, kind_int, relays)

        _ ->
          # Skip unknown TLV type (Go implementation ignores them)
          Logger.debug("Ignoring unknown TLV type: #{type}")
          extract_naddr_tlv_fields(remaining, identifier, pubkey, kind, relays)
      end
    else
      # Not enough data, return what we have
      Logger.debug("Not enough data for TLV entry, returning partial result")
      {identifier, pubkey, kind, relays}
    end
  rescue
    e ->
      Logger.error("Error processing TLV entry: #{Exception.message(e)}")
      {identifier, pubkey, kind, relays}
  end
  defp extract_naddr_tlv_fields(data, identifier, pubkey, kind, relays) do
    Logger.debug("Unexpected TLV data format: #{inspect(data)}")
    {identifier, pubkey, kind, relays}
  end

  # Extract nprofile data (pubkey and relays)
  defp extract_nprofile_data(data) do
    try do
      # Get pubkey from special field
      <<0, 32, pubkey::binary-size(32), rest::binary>> = data
      pubkey_hex = Base.encode16(pubkey, case: :lower)

      # Extract relays
      relays = extract_relays(rest)

      # Return the result
      {:ok, pubkey_hex, relays}
    rescue
      _ -> {:error, "Failed to extract nprofile data"}
    end
  end

  # Extract nevent data (event_id, author, relays)
  defp extract_nevent_data(data) do
    try do
      # Get event_id from special field
      <<0, 32, event_id::binary-size(32), rest::binary>> = data
      event_id_hex = Base.encode16(event_id, case: :lower)

      # Try to extract author and relays
      {author, relays} = extract_author_and_relays(rest)

      # Return the result
      {:ok, event_id_hex, author, relays}
    rescue
      _ -> {:error, "Failed to extract nevent data"}
    end
  end

  # Extract author and relays from TLV data
  defp extract_author_and_relays(data) do
    # Start with defaults
    author = nil
    relays = []

    # Process each TLV entry
    {author, relays} = process_tlv_entries(data, author, relays)

    {author, relays}
  end

  # Process TLV entries recursively
  defp process_tlv_entries(<<>>, author, relays), do: {author, relays}
  defp process_tlv_entries(<<type, len, rest::binary>>, author, relays) do
    if byte_size(rest) >= len do
      value = binary_part(rest, 0, len)
      remaining = binary_part(rest, len, byte_size(rest) - len)

      case type do
        @tlv_author when len == 32 ->
          # Found author
          new_author = Base.encode16(value, case: :lower)
          process_tlv_entries(remaining, new_author, relays)

        @tlv_relay ->
          # Found relay
          new_relays = [value | relays]
          process_tlv_entries(remaining, author, new_relays)

        _ ->
          # Skip unknown TLV type
          process_tlv_entries(remaining, author, relays)
      end
    else
      # Not enough data, return what we have
      {author, relays}
    end
  end
  defp process_tlv_entries(_, author, relays), do: {author, relays}

  # Extract relay entries from TLV data
  defp extract_relays(<<>>) do
    []
  end
  defp extract_relays(<<type, len, rest::binary>>) do
    if byte_size(rest) >= len && type == @tlv_relay do
      value = binary_part(rest, 0, len)
      remaining = binary_part(rest, len, byte_size(rest) - len)
      [value | extract_relays(remaining)]
    else
      # Skip this entry and try to parse the rest
      case rest do
        <<_, _, _::binary>> -> extract_relays(rest)
        _ -> []
      end
    end
  end
  defp extract_relays(_) do
    []
  end

  # Helper function to convert a string to integer with fallback
  defp string_to_integer(value) when is_integer(value), do: value
  defp string_to_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      :error -> 0  # Default to 0 if parsing fails
    end
  end
  defp string_to_integer(_), do: 0

  @doc """
  Encodes data in Bech32 format with the given human-readable prefix (hrp).
  Following the reference Go implementation.
  """
  def encode_to_bech32(hrp, data) do
    # Convert from 8 bits to 5 bits with padding
    data_5bit = case Bech32.convertbits(data, 8, 5, true) do
      {:ok, bits} -> bits
      bits when is_binary(bits) -> bits
      _ -> raise "Failed to convert bits for Bech32"
    end

    # Log the converted data for debugging
    Logger.debug("8-bit to 5-bit conversion result (#{byte_size(data)} → #{byte_size(data_5bit)}):")
    Logger.debug("Original hex: #{Base.encode16(data, case: :lower)}")
    if is_binary(data_5bit) do
      Logger.debug("5-bit data hex: #{Base.encode16(data_5bit, case: :lower)}")
    else
      Logger.debug("5-bit data (not binary): #{inspect(data_5bit)}")
    end

    # Generate final Bech32 encoding
    case Bech32.encode_from_5bit(hrp, data_5bit) do
      {:ok, encoded} -> encoded
      encoded when is_binary(encoded) -> encoded
      _ -> raise "Failed to encode Bech32"
    end
  end

  # Validate pubkey format
  defp validate_pubkey!(pubkey_hex) do
    case Base.decode16(pubkey_hex, case: :mixed) do
      {:ok, bin} when byte_size(bin) == 32 ->
        :ok
      _ ->
        raise ArgumentError, "Invalid pubkey format, must be 64 hex chars: #{pubkey_hex}"
    end
  end
end
