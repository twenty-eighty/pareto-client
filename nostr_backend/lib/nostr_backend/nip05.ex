defmodule NostrBackend.Nip05 do
  # Removing HTTPoison.Base, no longer needed

  @cache_name :nip05_cache
  # 24 hours in seconds
  @cache_ttl 86_400

  @moduledoc """
  Provides functions for handling NIP-05 identifiers, parsing them,
  and retrieving associated public keys and relays.
  """

  @type nip05_identifier :: String.t()
  @type name :: String.t()
  @type domain :: String.t()
  @type public_key :: String.t()
  @type relay :: String.t()
  @type result :: {:ok, public_key, [relay]} | {:error, String.t()}

  @doc """
  Parses a NIP-05 identifier into a `name` and `domain`.

  ## Examples

      iex> NostrBackend.Nip05.parse_identifier("alice@example.com")
      {:ok, "alice", "example.com"}

      iex> NostrBackend.Nip05.parse_identifier("invalid_identifier")
      {:error, "Invalid NIP-05 identifier"}
  """
  def parse_identifier(identifier) when is_binary(identifier) do
    case String.split(identifier, "@") do
      [name, domain] -> {:ok, name, domain}
      _ -> {:error, "Invalid NIP-05 identifier"}
    end
  end

  @doc """
  Retrieves the raw `.well-known/nostr.json` response for a given `name` and `domain`,
  caching the result to avoid repeated network requests.

  ## Examples

      iex> NostrBackend.Nip05.get_cached_well_known("alice", "example.com")
      {:ok, %{"names" => %{"alice" => "npub123..."}}}

      iex> NostrBackend.Nip05.get_cached_well_known("unknown", "example.com")
      {:error, "NIP-05 data not found on domain"}
  """
  def get_cached_well_known(name, domain) do
    cache_key = "#{domain}/#{name}"

    case Cachex.get(@cache_name, cache_key) do
      {:ok, nil} ->
        # If not in cache, fetch and cache the response
        case fetch_well_known(name, domain) do
          {:ok, response} ->
            Cachex.put(@cache_name, cache_key, response, ttl: @cache_ttl)
            {:ok, response}

          error ->
            error
        end

      {:ok, response} ->
        {:ok, response}

      {:error, reason} ->
        {:error, "Cache error: #{inspect(reason)}"}
    end
  end

  defp fetch_well_known(name, domain) do
    url = "https://#{domain}/.well-known/nostr.json?name=#{name}"

    case Req.get(url, redirect: true) do
      {:ok, %Req.Response{status: 200, body: body}} when is_map(body) ->
        # Req already decoded the JSON for us
        {:ok, body}

      {:ok, %Req.Response{status: 200, body: body}} ->
        # Handle case where Req didn't decode the JSON automatically
        case Jason.decode(body) do
          {:ok, data} -> {:ok, data}
          {:error, reason} -> {:error, "Invalid JSON: #{inspect(reason)}"}
        end

      {:ok, %Req.Response{status: status}} when status in [404, 400] ->
        {:error, "NIP-05 data not found on domain"}

      {:ok, %Req.Response{status: status}} ->
        {:error, "NIP-05 data not available: HTTP status #{status}"}

      {:error, exception} ->
        {:error, "HTTP request failed: #{inspect(exception)}"}
    end
  end

  @doc """
  Retrieves the public key and relay URLs for a given `name` and `domain`
  by performing an HTTP request to the domain as per the NIP-05 specification.

  ## Examples

      iex> NostrBackend.Nip05.get_pubkey_and_relays("alice", "example.com")
      {:ok, "pub_key_abc123", ["wss://relay1.example.com", "wss://relay2.example.com"]}

      iex> NostrBackend.Nip05.get_pubkey_and_relays("unknown", "example.com")
      {:error, "Public key and relays not found for name and domain"}
  """
  def get_pubkey_and_relays(name, domain) when is_binary(name) and is_binary(domain) do
    case fetch_well_known(name, domain) do
      {:ok, %{"names" => names} = data} ->
        case Map.get(names, name) do
          nil ->
            {:error, "Public key and relays not found for name and domain"}

          public_key when is_binary(public_key) ->
            relays =
              case Map.get(data, "relays") do
                nil -> []
                relay_map -> Map.get(relay_map, public_key, [])
              end

            {:ok, public_key, relays}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Parses a NIP-05 identifier and returns the public key and relays if available.

  ## Examples

      iex> NostrBackend.Nip05.resolve_identifier("alice@example.com")
      {:ok, "pub_key_abc123", ["wss://relay1.example.com", "wss://relay2.example.com"]}

      iex> NostrBackend.Nip05.resolve_identifier("unknown@example.com")
      {:error, "Public key and relays not found for identifier"}
  """
  def resolve_identifier(identifier) when is_binary(identifier) do
    with {:ok, name, domain} <- parse_identifier(identifier),
         {:ok, pubkey, relays} <- get_pubkey_and_relays(name, domain) do
      {:ok, pubkey, relays}
    else
      {:error, _} -> {:error, "Public key and relays not found for identifier"}
    end
  end
end
