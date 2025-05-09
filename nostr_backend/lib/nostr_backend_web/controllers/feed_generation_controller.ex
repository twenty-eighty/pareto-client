defmodule NostrBackendWeb.FeedGenerationController do
  use NostrBackendWeb, :controller
  require Logger

  def generate(conn, _params) do
    Logger.info("Manually triggering feed and sitemap generation")

    # Trigger feed generation in a separate process to avoid blocking the request
    spawn(fn ->
      NostrBackend.FeedGenerator.handle_info(:generate, %{})
    end)

    text(conn, "Feed generation started")
  end
end
