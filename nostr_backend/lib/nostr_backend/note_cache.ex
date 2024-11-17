defmodule NostrBackend.NoteCache do
  alias NostrBackend.Content
  alias NostrBackend.NostrClient

  @cache_name :note_cache
  # 24 hours
  @ttl_in_seconds 86_400

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

  defp load_note(note_id) do
    # Implement the logic to load the profile from the Nostr network
    # For example:
    case NostrClient.fetch_note(note_id) do
      {:ok, event} -> {:ok, Content.parse_note_event(event)}
      {:error, reason} -> {:error, reason}
    end
  end
end
