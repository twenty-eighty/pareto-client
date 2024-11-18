defmodule NostrBackend.Nip05 do
  use HTTPoison.Base

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
  Retrieves the public key and relay URLs for a given `name` and `domain`
  by performing an HTTP request to the domain as per the NIP-05 specification.

  ## Examples

      iex> NostrBackend.Nip05.get_pubkey_and_relays("alice", "example.com")
      {:ok, "pub_key_abc123", ["wss://relay1.example.com", "wss://relay2.example.com"]}

      iex> NostrBackend.Nip05.get_pubkey_and_relays("unknown", "example.com")
      {:error, "Public key and relays not found for name and domain"}
  """
  def get_pubkey_and_relays(name, domain) when is_binary(name) and is_binary(domain) do
    url = "https://#{domain}/.well-known/nostr.json?name=#{name}"

    case HTTPoison.get(url, [], follow_redirect: true) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        with {:ok, data} <- Jason.decode(body),
             %{"names" => names} <- data,
             public_key when is_binary(public_key) <- Map.get(names, name) do
          relays = data["relays"]

          relay_urls =
            if relays != nil do
              Map.get(relays, public_key, [])
            else
              []
            end

          {:ok, public_key, relay_urls}
        else
          _ -> {:error, "Public key and relays not found for name and domain"}
        end

      {:ok, %HTTPoison.Response{status_code: status}} when status in [404, 400] ->
        {:error, "NIP-05 data not found on domain"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "HTTP request failed: #{inspect(reason)}"}
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
