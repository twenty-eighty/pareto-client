defmodule NostrBackend.ReadFeedRefresher do
  @moduledoc """
  Periodically refreshes the cached /read feed so new articles are fetched even
  without user traffic.
  """

  use GenServer

  require Logger

  alias NostrBackend.ReadFeed

  @refresh_interval :timer.minutes(5)
  @prefetch_count 4

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  @impl true
  def init(state) do
    schedule_refresh(0)
    {:ok, state}
  end

  @impl true
  def handle_info(:refresh_read_feed, state) do
    refresh()
    schedule_refresh(@refresh_interval)
    {:noreply, state}
  end

  defp refresh do
    case ReadFeed.latest(@prefetch_count) do
      {:ok, feed} ->
        Logger.debug("[ReadFeedRefresher] refreshed #{length(feed.articles)} articles")

      {:error, reason} ->
        Logger.warning("[ReadFeedRefresher] refresh failed: #{inspect(reason)}")
    end
  end

  defp schedule_refresh(interval) do
    Process.send_after(self(), :refresh_read_feed, interval)
  end
end
