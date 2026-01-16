defmodule NostrBackendWeb.EventPayload do
  @moduledoc """
  Utilities for building the JSON payload consumed by the Elm frontend.
  """

  @doc """
  Builds the JSON string for the `<script id="nostr-event-data">` element.
  Returns `nil` if no events are present after sanitization.
  """
  @spec encode(list() | map() | nil, map()) :: String.t() | nil
  def encode(events, extras \\ %{})
  def encode(nil, _extras), do: nil
  def encode([], _extras), do: nil

  def encode(events, extras) when is_list(events) do
    sanitized_events =
      events
      |> Enum.reject(&is_nil/1)
      |> Enum.map(&sanitize/1)
      |> Enum.reject(&is_nil/1)

    if sanitized_events == [] do
      nil
    else
      %{"events" => sanitized_events}
      |> Map.merge(build_extras(extras))
      |> Jason.encode!()
    end
  end

  def encode(event, extras) when is_map(event), do: encode([event], extras)

  @doc """
  Merges `base_events` into an already-encoded payload JSON.

  - If `payload_json` is `nil`, returns `encode(base_events)`.
  - If `base_events` is empty, returns `payload_json`.
  - Otherwise, decodes the payload, prepends missing events (by `id`) and re-encodes.
  """
  @spec merge_into_json(String.t() | nil, list()) :: String.t() | nil
  def merge_into_json(payload_json, base_events)
  def merge_into_json(payload_json, base_events) when base_events in [nil, []], do: payload_json
  def merge_into_json(nil, base_events), do: encode(base_events)

  def merge_into_json(payload_json, base_events)
      when is_binary(payload_json) and is_list(base_events) do
    with {:ok, %{} = payload} <- Jason.decode(payload_json),
         events when is_list(events) <- Map.get(payload, "events", []) do
      merged = merge_unique_events(base_events, events)
      Jason.encode!(Map.put(payload, "events", merged))
    else
      _ -> payload_json
    end
  end

  @doc """
  Appends an event to the accumulator list if the event is present.
  """
  @spec add_event(list(), map() | nil) :: list()
  def add_event(events, nil), do: events
  def add_event(events, event), do: events ++ [event]

  @doc """
  Extracts the raw Nostr event from an entity if available.
  """
  @spec raw_event(map() | nil) :: map() | nil
  def raw_event(nil), do: nil

  def raw_event(data) when is_map(data) do
    Map.get(data, :raw_event) || Map.get(data, "raw_event")
  end

  defp build_extras(nil), do: %{}

  defp build_extras(extras) when is_map(extras) do
    extras
    |> Enum.reduce(%{}, fn {key, value}, acc ->
      case sanitize(value) do
        nil -> acc
        sanitized -> Map.put(acc, key, sanitized)
      end
    end)
  end

  defp sanitize(%DateTime{} = datetime), do: DateTime.to_iso8601(datetime)
  defp sanitize(%NaiveDateTime{} = datetime), do: NaiveDateTime.to_iso8601(datetime)
  defp sanitize(%Time{} = time), do: Time.to_iso8601(time)

  defp sanitize(map) when is_map(map) do
    map
    |> Enum.reduce(%{}, fn {key, value}, acc ->
      Map.put(acc, key, sanitize(value))
    end)
  end

  defp sanitize(list) when is_list(list), do: Enum.map(list, &sanitize/1)
  defp sanitize(other), do: other

  defp merge_unique_events(base_events, events) do
    seen =
      base_events
      |> Enum.map(&event_id/1)
      |> Enum.reject(&is_nil/1)
      |> MapSet.new()

    {merged, _seen} =
      Enum.reduce(events, {base_events, seen}, fn event, {acc, seen_ids} ->
        id = event_id(event)

        cond do
          is_nil(id) ->
            {acc ++ [event], seen_ids}

          MapSet.member?(seen_ids, id) ->
            {acc, seen_ids}

          true ->
            {acc ++ [event], MapSet.put(seen_ids, id)}
        end
      end)

    merged
  end

  defp event_id(%{} = event) do
    Map.get(event, "id") || Map.get(event, :id)
  end
end
