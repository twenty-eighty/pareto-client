defmodule NostrBackend.NostrId do
  @moduledoc """
  Module for parsing Nostr IDs like naddr1, nprofile1, etc.
  """

  alias NostrBackend.TLVDecoder
  require Logger

  @type nostr_id ::
          {:article, String.t()}
          | {:profile, String.t()}
          | {:community, String.t()}
          | {:address, %{kind: integer(), pubkey: String.t(), identifier: String.t()}}

  @doc """
  Validates and decodes a NIP-19 identifier (npub, nprofile, nevent, naddr, etc.)
  Returns {:ok, decoded_data} or {:error, reason}
  """
  @spec validate_nip19(String.t()) :: {:ok, any()} | {:error, String.t()}
  def validate_nip19(nip19_id) when is_binary(nip19_id) do
    Logger.info("Validating NIP-19 ID: #{nip19_id}")

    case parse(nip19_id) do
      {:ok, decoded} ->
        Logger.info("Successfully decoded NIP-19 ID: #{inspect(decoded)}")
        {:ok, decoded}

      {:error, reason} ->
        Logger.error("Failed to validate NIP-19 ID: #{reason}")
        {:error, reason}
    end
  end

  @spec parse(String.t()) :: {:ok, nostr_id} | {:error, String.t()}
  def parse(nostr_id) when is_binary(nostr_id) do
    case Bech32.decode(nostr_id) do
      {:ok, hrp, data} ->
        parse_data(hrp, data)

      {:error, reason} ->
        {:error, "Invalid Nostr ID: #{inspect(reason)}"}
    end
  end

  # Grouped parse_data/2 functions
  defp parse_data("naddr", data) do
    case TLVDecoder.decode_tlv_stream(data) do
      {:ok, tlv_list} ->
        parsed_data = extract_tlv_data(:naddr, tlv_list)

        case parsed_data do
          %{kind: 34550} ->
            {:ok, {:community, parsed_data}}

          _ ->
            {:ok, {:author_article, parsed_data}}
        end

      {:error, reason} ->
        {:error, "Invalid TLV data in naddr: #{inspect(reason)}"}
    end
  end

  # Implementation for decoding npub Bech32 data to a pubkey hex string
  defp parse_data("npub", data) do
    try do
      pubkey_hex = Base.encode16(data, case: :lower)
      Logger.debug("Extracted pubkey hex: #{pubkey_hex}")

      {:ok, {:pubkey, pubkey_hex}}
    rescue
      e ->
        Logger.error("Failed to decode npub: #{Exception.message(e)}")
        {:error, "Failed to decode npub: #{Exception.message(e)}"}
    end
  end

  # TODO: check how ncomm's are built
  defp parse_data("ncomm", data) do
    community_id = Base.encode16(data, case: :lower)
    {:ok, {:community, community_id}}
  end

  defp parse_data("nprofile", data) do
    case TLVDecoder.decode_tlv_stream(data) do
      {:ok, tlv_list} ->
        {:ok, pubkey, relays} =
          extract_tlv_data(:nprofile, tlv_list)
          |> IO.inspect()

        {:ok, {:profile, pubkey, relays}}

      {:error, reason} ->
        {:error, "Invalid TLV data in nprofile: #{inspect(reason)}"}
    end
  end

  defp parse_data("nevent", data) do
    case TLVDecoder.decode_tlv_stream(data) do
      {:ok, tlv_list} ->
        parsed_data = extract_tlv_data(:nevent, tlv_list)
        {:ok, {:event, parsed_data}}

      {:error, reason} ->
        {:error, "Invalid TLV data in nevent: #{inspect(reason)}"}
    end
  end

  defp parse_data("nattr", data) do
    article_id = Base.encode16(data, case: :lower)
    {:ok, {:article, article_id}}
  end

  defp parse_data("note", data) do
    note_id = Base.encode16(data, case: :lower)
    {:ok, {:note, note_id}}
  end

  defp parse_data(prefix, _data) do
    {:error, "Unknown prefix: #{prefix}"}
  end

  defp extract_tlv_data(:naddr, tlv_list) do
    Enum.reduce(tlv_list, %{}, fn tlv, acc ->
      case tlv.tag do
        # Identifier
        0x00 ->
          identifier = tlv.value
          Map.put(acc, :identifier, identifier)

        # Pubkey
        0x01 ->
          pubkey = Base.encode16(tlv.value, case: :lower)
          Map.put(acc, :pubkey, pubkey)

        # Author
        0x02 ->
          author = Base.encode16(tlv.value, case: :lower)
          Map.put(acc, :author, author)

        # Kind
        0x03 ->
          kind = :binary.decode_unsigned(tlv.value, :big)
          Map.put(acc, :kind, kind)

        _ ->
          # Ignore unknown tags
          acc
      end
    end)
  end

  defp extract_tlv_data(:nevent, tlv_list) do
    Enum.reduce(tlv_list, %{relays: []}, fn tlv, acc ->
      case tlv.tag do
        # Event ID
        0x00 ->
          id = Base.encode16(tlv.value, case: :lower)
          Map.put(acc, :id, id)

        # Relay
        0x01 ->
          relays = acc.relays ++ [tlv.value]
          Map.put(acc, :relays, relays)

        # Author
        0x02 ->
          author = Base.encode16(tlv.value, case: :lower)
          Map.put(acc, :author, author)

        # Kind
        0x03 ->
          kind = :binary.decode_unsigned(tlv.value, :big)
          Map.put(acc, :kind, kind)

        _ ->
          # Ignore unknown tags
          acc
      end
    end)
  end

  defp extract_tlv_data(:nprofile, tlv_list) do
    {pubkey, relays} =
      Enum.reduce(tlv_list, {nil, []}, fn tlv, {pubkey, relays} ->
        case tlv.tag do
          # Pubkey
          0x00 ->
            {Base.encode16(tlv.value, case: :lower), relays}

          # Relay
          0x01 ->
            {pubkey, relays ++ [tlv.value]}

          _ ->
            # Ignore unknown tags
            {pubkey, relays}
        end
      end)

    if pubkey == nil do
      {:error, "No pubkey found in nprofile TLV"}
    else
      {:ok, pubkey, relays}
    end
  end
end
