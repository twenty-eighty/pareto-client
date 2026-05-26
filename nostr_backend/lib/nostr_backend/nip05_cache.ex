defmodule NostrBackend.Nip05Cache do
  require Logger
  alias NostrBackend.Nip05

  @cache_name :nip05_cache
  # 24 hours for successful resolutions
  @success_ttl_in_seconds 86_400
  # 1 hour for resolution failures
  @error_ttl_in_seconds 3600

  def get_pubkey_and_relays(nip05_identifier) do
    case Cachex.get(@cache_name, nip05_identifier) do
      {:ok, nil} ->
        # NIP-05 not found in cache, load it
        load_and_cache_nip05(nip05_identifier)

      {:ok, {pubkey, relays}} when is_binary(pubkey) ->
        Logger.debug("NIP-05: Found in cache: #{nip05_identifier}")
        {:ok, pubkey, relays}

      {:ok, :not_found} ->
        Logger.debug("NIP-05: Serving cached not found for: #{nip05_identifier}")
        {:error, "NIP-05 identifier not found"}

      {:ok, :resolution_failed} ->
        Logger.debug("NIP-05: Serving cached resolution failure for: #{nip05_identifier}")
        {:error, "Failed to resolve NIP-05 identifier"}

      {:error, reason} ->
        Logger.error("NIP-05: Cache error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp load_and_cache_nip05(nip05_identifier) do
    case Nip05.resolve_identifier(nip05_identifier) do
      {:ok, pubkey, relays} ->
        Logger.debug("NIP-05: Successfully resolved: #{nip05_identifier}")
        # Cache successful resolutions for 24 hours
        Cachex.put(@cache_name, nip05_identifier, {pubkey, relays}, ttl: @success_ttl_in_seconds)
        {:ok, pubkey, relays}

      {:error, reason} ->
        Logger.debug("NIP-05: Resolution failed for #{nip05_identifier}: #{inspect(reason)}")
        # Cache resolution failures for 1 hour
        Cachex.put(@cache_name, nip05_identifier, :resolution_failed, ttl: @error_ttl_in_seconds)
        {:error, reason}
    end
  end
end
