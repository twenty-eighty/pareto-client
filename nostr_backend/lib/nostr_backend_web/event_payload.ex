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
end


