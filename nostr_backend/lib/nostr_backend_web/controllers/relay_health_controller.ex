defmodule NostrBackendWeb.RelayHealthController do
  use NostrBackendWeb, :controller

  alias NostrBackendWeb.Endpoint
  alias NostrAccess.RelayHealth

  def index(conn, _params) do
    relays = Application.get_env(:nostr_backend, :relay_urls, [])

    json(conn, %{
      relays: RelayHealth.all_states(relays)
    })
  end

  def page(conn, _params) do
    relays = Application.get_env(:nostr_backend, :relay_urls, [])
    states = RelayHealth.all_states(relays)

    conn
    |> add_meta_tags()
    |> assign(:skip_app_script, true)
    |> render(:index, relays: states, layout: false)
  end

  def show(conn, %{"url" => relay_url}) do
    states = RelayHealth.all_states([relay_url])
    state = Enum.find(states, &(&1.relay_url == relay_url))
    window_minutes = relay_fail_window_minutes()

    conn
    |> add_meta_tags()
    |> assign(:skip_app_script, true)
    |> render(:show,
      relay_url: relay_url,
      state: state,
      window_minutes: window_minutes,
      layout: false
    )
  end

  defp add_meta_tags(conn) do
    lang = NostrBackend.Locale.preferred_language(conn)
    current_url = Endpoint.url() <> conn.request_path

    schema_metadata = %{
      "@context" => "https://schema.org",
      "@type" => "WebPage",
      "url" => current_url,
      "name" => "Relay Health",
      "description" => "Relay health status for Nostr relays",
      "publisher" => %{
        "@type" => "Organization",
        "name" => "Pareto Project",
        "url" => Endpoint.url()
      }
    }

    conn
    |> assign(:lang, lang)
    |> assign(:meta_title, "Relay Health")
    |> assign(:meta_description, "Relay health status for Nostr relays")
    |> assign(:meta_image, "/images/pareto-shared.png")
    |> assign(:meta_url, current_url)
    |> assign(:canonical_url, current_url)
    |> assign(:schema_metadata, Jason.encode!(schema_metadata))
  end

  defp relay_fail_window_minutes do
    :nostr_access
    |> Application.get_env(:relay_health, [])
    |> Keyword.get(:fail_window_minutes, 5)
  end
end
