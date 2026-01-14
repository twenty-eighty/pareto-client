defmodule NostrBackendWeb.Plugs.AssignFollowListEvent do
  @moduledoc """
  Assigns the configured FOLLOW_LIST_PUBKEY contact-list event (kind 3) into
  `conn.assigns[:nostr_base_events]` so every HTML page can ship the follow list
  to the frontend.
  """

  import Plug.Conn

  alias NostrBackend.FollowListCache

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    base_events =
      case Application.get_env(:nostr_backend, :follow_list_pubkey) do
        pubkey when is_binary(pubkey) and pubkey != "" ->
          case FollowListCache.get_follow_list_event(pubkey) do
            {:ok, %{} = event} -> [event]
            _ -> []
          end

        _ ->
          []
      end

    assign(conn, :nostr_base_events, base_events)
  end
end
