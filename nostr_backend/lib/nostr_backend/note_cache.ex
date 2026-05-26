defmodule NostrBackend.NoteCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  @type note_id :: binary() | %{id: binary(), kind: integer(), relays: [binary()]}
  @type note :: map()
  @type picture_post :: Content.picture_post()
  @type cache_result :: {:ok, note() | picture_post()} | {:error, binary()}

  @cache_name :note_cache
  # 24 hours
  @ttl_in_seconds 86_400

  @spec get_note(note_id()) :: cache_result()
  def get_note(note_id) do
    case Cachex.get(@cache_name, note_id) do
      {:ok, nil} ->
        # Profile not found in cache, load it
        with {:ok, note} <- load_note(note_id) do
          # Store the profile in the cache with a TTL
          Cachex.put(@cache_name, note_id, note, ttl: @ttl_in_seconds)
          {:ok, note}
        else
          error -> error
        end

      {:ok, note} ->
        {:ok, note}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec load_note(%{id: binary(), kind: 20, relays: [binary()]}) ::
          {:ok, picture_post()} | {:error, binary()}
  defp load_note(%{id: id, kind: 20, relays: relays}) do
    case NostrClient.fetch_picture_post(id, relays) do
      {:ok, [event | _]} -> {:ok, Content.parse_picture_post(event)}
      {:ok, []} -> {:error, "No picture post events found"}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec load_note(binary()) :: cache_result()
  defp load_note(note_id) do
    # Implement the logic to load the profile from the Nostr network
    # For example:
    case NostrClient.fetch_note(note_id) do
      {:ok, [event | _]} -> {:ok, Content.parse_note_event(event)}
      {:ok, []} -> {:error, "No note events found"}
      {:error, reason} -> {:error, reason}
    end
  end
end
