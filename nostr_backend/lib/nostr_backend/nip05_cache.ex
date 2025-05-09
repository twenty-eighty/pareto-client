defmodule NostrBackend.Nip05Cache do
  require Logger
  alias NostrBackend.Nip05

  @cache_name :nip05_cache
  # 24 hours
  @ttl_in_seconds 86_400

  def get_pubkey_and_relays(nip05_identifier) do
    case Cachex.get(@cache_name, nip05_identifier) do
      {:ok, nil} ->
        # NIP-05 not found in cache, load it
        with {:ok, pubkey, relays} <- Nip05.resolve_identifier(nip05_identifier) do
          # Store the article in the cache with a TTL
          Cachex.put(@cache_name, nip05_identifier, {pubkey, relays}, ttl: @ttl_in_seconds)
          {:ok, pubkey, relays}
        else
          error -> error
        end

      {:ok, {pubkey, relays}} ->
        Logger.debug("nip05 found in cache")
        {:ok, pubkey, relays}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
