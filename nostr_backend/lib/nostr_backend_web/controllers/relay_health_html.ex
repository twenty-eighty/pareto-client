defmodule NostrBackendWeb.RelayHealthHTML do
  @moduledoc """
  HTML rendering for relay health pages.
  """

  use NostrBackendWeb, :html

  embed_templates("relay_health_html/*")

  def format_ts(nil), do: "-"

  def format_ts(ts) when is_integer(ts) do
    case DateTime.from_unix(ts) do
      {:ok, datetime} -> DateTime.to_string(datetime)
      _ -> Integer.to_string(ts)
    end
  end
end
