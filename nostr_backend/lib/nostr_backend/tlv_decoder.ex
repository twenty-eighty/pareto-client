defmodule NostrBackend.TLVDecoder do
  @moduledoc false

  @compile {:inline, accumulate_tag: 2, constructed_tag?: 1}

  alias NostrBackend.TLV

  import Bitwise

  def decode(tlv) do
    with {tag, constructed, lv} <- decode_tag(tlv),
         {len, v} <- decode_length(constructed, lv),
         {value, rest} <- decode_value(constructed, len, v) do
      {%TLV{tag: tag, value: value, indefinite_length: len == :indefinite}, rest}
    else
      _ -> :no_tlv
    end
  end

  def decode_tlv_stream(binary) do
    decode_tlv_stream([], binary)
  end

  defp decode_tlv_stream(acc, <<>>) do
    {:ok, Enum.reverse(acc)}
  end

  defp decode_tlv_stream(acc, binary) do
    case decode(binary) do
      {tlv, rest} ->
        decode_tlv_stream([tlv | acc], rest)

      :no_tlv ->
        {:error, :invalid_tlv_format}
    end
  end

  defp decode_tag(<<tag::unsigned-big-integer-size(8), rest::binary>>)
       when (tag &&& 0x1F) != 0x1F do
    {tag, constructed_tag?(tag), rest}
  end

  defp decode_tag(<<tag::unsigned-big-integer-size(8), rest::binary>>) do
    decode_tag_number(tag, constructed_tag?(tag), rest)
  end

  defp decode_tag(_binary), do: :no_tlv

  defp decode_tag_number(acc, constructed, <<tag::unsigned-big-integer-size(8), rest::binary>>)
       when (tag &&& 0x80) == 0x80 do
    acc |> accumulate_tag(tag) |> decode_tag_number(constructed, rest)
  end

  defp decode_tag_number(acc, constructed, <<tag::unsigned-big-integer-size(8), rest::binary>>) do
    {accumulate_tag(acc, tag), constructed, rest}
  end

  defp decode_tag_number(_acc, _constructed, _binary), do: :no_tlv

  defp accumulate_tag(acc, tag), do: acc <<< 8 ||| tag
  defp constructed_tag?(tag), do: (tag &&& 0x20) == 0x20

  defp decode_length(true, <<len::unsigned-big-integer-size(8), rest::binary>>)
       when len == 0x80 do
    {:indefinite, rest}
  end

  defp decode_length(false, <<len::unsigned-big-integer-size(8), _rest::binary>>)
       when len == 0x80 do
    :no_tlv
  end

  defp decode_length(_constructed, <<len::unsigned-big-integer-size(8), rest::binary>>)
       when len <= 0x7F do
    {len, rest}
  end

  defp decode_length(_constructed, <<len_of_len::unsigned-big-integer-size(8), lv::binary>>) do
    len_of_len = len_of_len &&& 0x7F

    if byte_size(lv) >= len_of_len do
      <<len::unsigned-big-integer-size(len_of_len)-unit(8), rest::binary>> = lv
      {len, rest}
    else
      :no_tlv
    end
  end

  defp decode_length(_constructed, _binary), do: :no_tlv

  defp decode_value(false, len, v) do
    if byte_size(v) >= len do
      <<value::binary-size(len), rest::binary>> = v
      {value, rest}
    else
      :no_tlv
    end
  end

  defp decode_value(true, :indefinite, v) do
    decode_inner_tlvs([], true, v)
  end

  defp decode_value(true, len, v) do
    {inner_tlvs, rest} = decode_value(false, len, v)
    {decode_inner_tlvs([], false, inner_tlvs), rest}
  end

  defp decode_inner_tlvs(acc, true, <<0x00, 0x00, rest::binary>>), do: {Enum.reverse(acc), rest}
  defp decode_inner_tlvs(acc, false, ""), do: Enum.reverse(acc)

  defp decode_inner_tlvs(acc, indefinite, data) do
    case decode(data) do
      :no_tlv -> :no_tlv
      {tlv, rest} -> decode_inner_tlvs([tlv | acc], indefinite, rest)
    end
  end
end
