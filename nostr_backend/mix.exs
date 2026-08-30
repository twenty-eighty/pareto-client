defmodule NostrBackend.MixProject do
  use Mix.Project

  def project do
    [
      app: :nostr_backend,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      # applications: [:websockex],
      mod: {NostrBackend.Application, []},
      extra_applications: [:logger, :runtime_tools, :nostr_access]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.8.13"},
      {:phoenix_html, "~> 4.3"},
      {:phoenix_live_reload, "~> 1.7", only: :dev},
      {:phoenix_live_view, "~> 1.2.11"},
      {:phoenix_live_dashboard, "~> 0.9.0"},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 1.0"},
      {:jason, "~> 1.2"},
      {:dns_cluster, "~> 0.3.0"},
      {:bandit, "~> 1.12"},
      {:bech32, "~> 1.0"},
      # websockex/pool management provided by nostr_access
      {:cachex, "~> 4.0"},
      {:uuid, "~> 1.1"},
      {:mdex, "~> 0.13.5"},
      {:browser, "~> 0.5.4"},
      {:ua_inspector, "~> 3.0"},
      {:remote_ip, "~> 1.2"},
      {:posthog, "~> 2.15"},
      {:req, "~> 0.7.4"},
      {:http_cookie, "~> 0.11.0"},
      # ua_inspector still declares hackney ~> 1.x
      {:hackney, "~> 4.7.4", override: true},
      {:brotli, "~> 0.3.0"},
      {:floki, "~> 0.38.0"},
      {:sweet_xml, "~> 0.7.5"},
      {:atomex, "~> 0.4"},
      {:nostr_access, "~> 0.3"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "assets.setup", "assets.build"],
      "assets.setup": ["esbuild.install --if-missing"],
      "assets.build": ["esbuild nostr_backend"],
      "assets.deploy": [
        "esbuild nostr_backend --minify",
        "phx.digest",
        "run priv/scripts/brotli_assets.exs"
      ]
    ]
  end
end
