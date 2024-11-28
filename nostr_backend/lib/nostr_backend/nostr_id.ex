defmodule NostrBackend.NostrId do
  @moduledoc """
  Module for parsing Nostr IDs like naddr1, nprofile1, etc.
  """

  alias NostrBackend.TLVDecoder

  @type nostr_id ::
          {:article, String.t()}
          | {:profile, String.t()}
          | {:community, String.t()}
          | {:address, %{kind: integer(), pubkey: String.t(), identifier: String.t()}}

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

  # defp parse_naddr_tlv(data) do
  #   IO.inspect(data, label: "TLV Data")
  #   parse_tlv(data, %{})
  # end

  defp parse_tlv(<<>>, acc) do
    IO.inspect(acc, label: "Final Accumulator")
    {:ok, acc}
  end

  defp parse_tlv(<<t::8, l::8, rest::binary>>, acc) do
    IO.inspect({t, l}, label: "Type and Length")
    IO.inspect(rest, label: "Rest before extracting value")

    if byte_size(rest) >= l do
      <<v::binary-size(l), rest::binary>> = rest
      IO.inspect(v, label: "Value")

      updated_acc =
        case t do
          0x00 ->
            kind = :binary.decode_unsigned(v, :big)
            Map.put(acc, :kind, kind)

          0x01 ->
            pubkey = Base.encode16(v, case: :lower)
            Map.put(acc, :pubkey, pubkey)

          0x02 ->
            identifier = v
            Map.put(acc, :identifier, identifier)

          _ ->
            # Ignore unknown types
            acc
        end

      parse_tlv(rest, updated_acc)
    else
      {:error, "Invalid TLV format"}
    end
  end

  defp parse_tlv(_, _acc), do: {:error, "Invalid TLV format"}
end
