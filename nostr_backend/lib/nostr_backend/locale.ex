defmodule NostrBackend.Locale do
  @moduledoc """
  Provides functions to fetch the available and default languages defined in
  priv/static/translations/languages.json. The JSON is loaded at compile time.
  """

  # Path to the translations file in priv
  @languages_file Path.join(:code.priv_dir(:nostr_backend), "static/translations/languages.json")
  @external_resource @languages_file

  # Load and decode JSON at compile time
  @translations @languages_file
                |> File.read!()
                |> Jason.decode!()

  # Extract available and default values
  @available_languages Map.get(@translations, "available", [])
  @default_language Map.get(@translations, "default")
  @query_param_key Map.get(@translations, "queryParam", "locale")

  @doc "Returns the list of available language codes."
  @spec available_languages() :: [String.t()]
  def available_languages(), do: @available_languages

  @doc "Returns the default language code."
  @spec default_language() :: String.t()
  def default_language(), do: @default_language

  @doc false
  @spec locale_param_key() :: String.t()
  def locale_param_key(), do: @query_param_key

  @doc "Returns the preferred language code, checking the 'locale' query param first and falling back to the Accept-Language header."
  @spec preferred_language(Plug.Conn.t()) :: String.t()
  def preferred_language(conn) do
    key = locale_param_key()
    locale_param = conn.params[key]

    if locale_param in @available_languages do
      locale_param
    else
      conn
      |> Plug.Conn.get_req_header("accept-language")
      |> List.first()
      |> parse_language()
    end
  end

  @doc false
  @spec parse_language(String.t() | nil) :: String.t()
  defp parse_language(nil), do: default_language()

  defp parse_language(lang_header) do
    lang_header
    |> String.split(",", trim: true)
    |> List.first()
    |> String.split("-", trim: true)
    |> List.first()
  end
end
