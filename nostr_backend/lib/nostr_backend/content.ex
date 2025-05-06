defmodule NostrBackend.Content do
  alias NostrBackend.NostrClient
  require Logger

  # Article functions
  def get_article_with_query(%{kind: kind, identifier: identifier, author: author}) do
    case NostrClient.fetch_article_by_address(kind, author, identifier) do
      {:ok, event} ->
        article_data = parse_article_event(event)
        {:ok, article_data}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_article_by_address(%{kind: kind, identifier: identifier}) do
    case NostrClient.fetch_article_by_address(kind, identifier) do
      {:ok, event} ->
        article_data = parse_article_event(event)
        {:ok, article_data}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Helper functions to parse events
  def parse_article_event(event) when is_map(event) do
    %{
      article_id: event["id"],
      kind: event["kind"],
      author: event["pubkey"],
      identifier: extract_first_tag(event, "d"),
      title: extract_title(event) |> NostrBackend.Substitution.replace_randomly(),
      description: extract_summary(event) |> NostrBackend.Substitution.replace_randomly(),
      content: render_markdown(event["content"]),
      image_url: extract_image_url(event),
      published_at: extract_published_at(event),
      created_at: DateTime.from_unix!(event["created_at"]),
      tags: event["tags"] || []
    }
  end

  def parse_article_event({_type, _id, event}) when is_map(event) do
    parse_article_event(event)
  end

  def parse_article_event(_), do: %{}

  def parse_community_event(event) do
    %{
      community_id: event["pubkey"],
      name: extract_first_tag(event, "d"),
      description: extract_first_tag(event, "description"),
      image: extract_first_tag(event, "image")
    }
  end

  def parse_note_event(event) do
    %{
      note_id: event["id"],
      content: event["content"]
    }
    |> IO.inspect(label: "Parsed note event")
  end

  def parse_profile_event(event) when is_map(event) do
    Logger.debug("Parsing profile event: #{inspect(event)}")

    # Handle both event formats: direct map and tuple with type/id/event
    event_map = case event do
      {_type, _id, event_data} -> event_data
      _ -> event
    end

    content = case event_map do
      %{"content" => content} -> content
      %{content: content} -> content
      _ -> "{}"
    end

    decoded = case Jason.decode(content) do
      {:ok, decoded} ->
        Logger.debug("Decoded profile content: #{inspect(decoded)}")
        decoded
      error ->
        Logger.error("Failed to decode profile content: #{inspect(error)}")
        %{}
    end

    # Get pubkey from either string or atom key
    pubkey = event_map["pubkey"] || event_map[:pubkey]
    Logger.debug("Extracted pubkey: #{inspect(pubkey)}")

    profile = %{
      profile_id: pubkey,
      name: decoded["name"] || decoded["displayName"],
      username: decoded["username"],
      about: decoded["about"],
      banner: decoded["banner"],
      image: decoded["image"] || decoded["picture"],
      display_name: decoded["display_name"] || decoded["displayName"],
      website: decoded["website"],
      lud16: decoded["lud16"],
      nip05: decoded["nip05"],
      picture: decoded["picture"]
    }

    Logger.debug("Parsed profile: #{inspect(profile)}")
    profile
  end

  def parse_profile_event(_), do: %{}

  # Placeholder extract functions
  defp extract_title(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "title" end) do
      ["title", title | _rest] -> title
      # Returns nil if the title tag is not found
      _ -> nil
    end
  end

  defp extract_summary(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "summary" end) do
      ["summary", summary | _rest] -> summary
      # Returns nil if the summary tag is not found
      _ -> nil
    end
  end

  defp extract_image_url(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "image" end) do
      ["image", image | _rest] -> image
      # Returns nil if the image tag is not found
      _ -> nil
    end
  end

  defp extract_published_at(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "published_at" end) do
      ["published_at", timestamp | _rest] ->
        case Integer.parse(timestamp) do
          {timestamp, _} -> DateTime.from_unix!(timestamp)
          :error -> DateTime.from_unix!(event["created_at"])
        end
      _ -> DateTime.from_unix!(event["created_at"])
    end
  end

  defp extract_first_tag(event, name) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == name end) do
      [^name, image | _rest] -> image
      _ -> nil
    end
  end

  defp render_markdown(content) when is_binary(content) do
    content
    |> replace_http_with_https()
    |> Earmark.as_html!()
  end

  defp render_markdown(_), do: ""

  def replace_http_with_https(nil), do: nil

  def replace_http_with_https(text) do
    Regex.replace(~r/http:\/\//, text, "https://")
  end
end
