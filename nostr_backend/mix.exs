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
      extra_applications: [:logger, :runtime_tools, :poolboy]
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
      {:phoenix, "~> 1.7.14"},
      {:phoenix_html, "~> 4.1"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      # TODO bump on release to {:phoenix_live_view, "~> 1.0.0"},
      {:phoenix_live_view, "~> 1.0.0-rc.1", override: true},
      {:phoenix_live_dashboard, "~> 0.8.3"},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.2"},
      {:dns_cluster, "~> 0.1.1"},
      {:bandit, "~> 1.5"},
      {:bech32, "~> 1.0"},
      {:websockex, "~> 0.4.3"},
      {:poolboy, "~> 1.5"},
      {:cachex, "~> 3.4"},
      {:uuid, "~> 1.1"},
      {:httpoison, "~> 1.8"},
      {:earmark, "~> 1.4"},
      {:browser, "~> 0.5.4"},
      {:ua_inspector, "~> 3.0"},
      {:remote_ip, "~> 1.2"},
      {:posthog, "~> 0.1"},
      {:req, "~> 0.5.15"},
      {:http_cookie, "~> 0.9.0"},
      {:brotli, "~> 0.3.0"},
      {:floki, "~> 0.37.0"},
      {:sweet_xml, "~> 0.7.5"},
      {:atomex, "~> 0.4"}
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
        "phx.digest"
      ]
    ]
  end
end
