# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :nostr_backend,
  generators: [timestamp_type: :utc_datetime],
  feed_generator: %{
    source_pubkey: System.get_env("FEED_SOURCE_PUBKEY", "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef"), # authors@pareto.space
    feed_size: String.to_integer(System.get_env("FEED_SIZE", "20")),
    relay_url: System.get_env("FEED_RELAY_URL", "wss://nostr.pareto.space")
  },
  relay_urls: [
    "wss://nostr.pareto.space",
    "wss://nostr.pareto.town",
    "wss://nos.lol",
    "wss://relay.nostr.band",
    "wss://relay.damus.io",
    "wss://pareto.nostr1.com"
  ]

# Configures the endpoint
config :nostr_backend, NostrBackendWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: NostrBackendWeb.ErrorHTML, json: NostrBackendWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: NostrBackend.PubSub,
  live_view: [signing_salt: "WtN3GhBc"]

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.17.11",
  nostr_backend: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :ua_inspector,
  database_path: "priv/ua_inspector",
  http_opts: [recv_timeout: 10_000]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :posthog,
  api_url: System.get_env("POSTHOG_HOST") || "https://eu.i.posthog.com",
  api_key: System.get_env("POSTHOG_API_KEY")

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
