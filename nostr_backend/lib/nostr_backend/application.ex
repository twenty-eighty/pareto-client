defmodule NostrBackend.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      NostrBackendWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:nostr_backend, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: NostrBackend.PubSub},

      # Cache for articles
      Supervisor.child_spec(
        {Cachex, name: :articles_cache, ttl_interval: :timer.minutes(1440)},
        id: :articles_cache
      ),

      # Cache for communities
      Supervisor.child_spec(
        {Cachex, name: :communities_cache, ttl_interval: :timer.minutes(1440)},
        id: :communities_cache
      ),

      # Cache for profiles
      Supervisor.child_spec(
        {Cachex, name: :profiles_cache, ttl_interval: :timer.minutes(1440)},
        id: :profiles_cache
      ),

      # Cache for profiles
      Supervisor.child_spec(
        {Cachex, name: :nip05_cache, ttl_interval: :timer.minutes(1440)},
        id: :nip05_cache
      ),

      # Cache for notes
      Supervisor.child_spec(
        {Cachex, name: :note_cache, ttl_interval: :timer.minutes(1440)},
        id: :note_cache
      ),
      NostrBackend.PostHogBuffer,

      # Start a worker by calling: NostrBackend.Worker.start_link(arg)
      # {NostrBackend.Worker, arg},
      # Start to serve requests, typically the last entry
      NostrBackendWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: NostrBackend.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    NostrBackendWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
