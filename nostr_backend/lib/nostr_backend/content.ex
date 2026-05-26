defmodule NostrBackend.Content do
  alias NostrBackend.NostrClient
  require Logger

  # Type definitions
  @type nostr_event :: map()
  @type event_tuple :: {binary(), binary(), nostr_event()}
  @type article_query :: %{kind: integer(), identifier: binary(), author: binary()}
  @type address_query :: %{kind: integer(), identifier: binary()}
  @type article :: %{
          article_id: binary(),
          kind: integer(),
          author: binary(),
          identifier: binary() | nil,
          title: binary() | nil,
          description: binary() | nil,
          content: binary(),
          image_url: binary() | nil,
          published_at: DateTime.t(),
          created_at: DateTime.t(),
          tags: list()
        }
  @type community :: %{
          community_id: binary(),
          name: binary() | nil,
          description: binary() | nil,
          image: binary() | nil
        }
  @type note :: %{
          note_id: binary(),
          content: binary()
        }
  @type picture_post :: %{
          note_id: binary(),
          content: binary(),
          image: binary() | nil
        }
  @type profile :: %{
          profile_id: binary() | nil,
          name: binary() | nil,
          username: binary() | nil,
          about: binary() | nil,
          banner: binary() | nil,
          image: binary() | nil,
          display_name: binary() | nil,
          website: binary() | nil,
          lud16: binary() | nil,
          nip05: binary() | nil,
          picture: binary() | nil
        }

  # Article functions
  @spec get_article_with_query(article_query()) :: {:ok, article()} | {:error, binary()}
  def get_article_with_query(%{kind: kind, identifier: identifier, author: author}) do
    case NostrClient.fetch_article_by_address(kind, author, identifier) do
      {:ok, _relay, [event | _]} ->
        {:ok, parse_article_event(event)}

      {:ok, _relay, []} ->
        {:error, "No events found for article"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec get_article_by_address(address_query()) :: {:ok, article()} | {:error, binary()}
  def get_article_by_address(%{kind: kind, identifier: identifier}) do
    case NostrClient.fetch_article_by_address(kind, identifier) do
      {:ok, _relay, [event | _]} ->
        {:ok, parse_article_event(event)}

      {:ok, _relay, []} ->
        {:error, "No events found for article"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Helper functions to parse events
  @spec parse_article_event(nostr_event()) :: article()
  def parse_article_event(%{"id" => id} = event) do
    %{
      article_id: id,
      kind: event["kind"],
      author: event["pubkey"],
      identifier: extract_first_tag(event, "d"),
      title: extract_title(event),
      description: extract_summary(event),
      content: render_markdown(event["content"]),
      image_url: extract_image_url(event),
      published_at: extract_published_at(event),
      created_at: DateTime.from_unix!(event["created_at"]),
      tags: event["tags"] || []
    }
  end

  @spec parse_article_event(event_tuple()) :: article()
  def parse_article_event({_type, _sub_id, %{"id" => _id} = event}) do
    parse_article_event(event)
  end

  @spec parse_article_event(any()) :: map()
  def parse_article_event(_), do: %{}

  @spec parse_community_event(nostr_event()) :: community()
  def parse_community_event(event) do
    %{
      community_id: event["pubkey"],
      name: extract_first_tag(event, "d"),
      description: extract_first_tag(event, "description"),
      image: extract_first_tag(event, "image")
    }
  end

  @spec parse_note_event(nostr_event()) :: note()
  def parse_note_event(event) do
    note = %{
      note_id: event["id"],
      content: event["content"]
    }

    Logger.debug("Parsed note event: #{inspect(note)}")
    note
  end

  @spec parse_picture_post(nostr_event()) :: picture_post()
  def parse_picture_post(event) do
    picture_post = %{
      note_id: event["id"],
      content: event["content"],
      image: extract_first_image(event)
    }

    Logger.debug("Parsed picture post event: #{inspect(picture_post)}")
    picture_post
  end

  @spec parse_profile_event(nostr_event() | event_tuple()) :: profile()
  def parse_profile_event({_, _, event_data}) when is_map(event_data) do
    parse_profile_event(event_data)
  end

  def parse_profile_event(event) when is_map(event) do
    Logger.debug("Parsing profile event: #{inspect(event)}")

    # Early return if event is empty
    if map_size(event) == 0 do
      Logger.warning("Empty event map for profile parsing")
      %{}
    else
      content =
        case event do
          %{"content" => content} when is_binary(content) -> content
          %{content: content} when is_binary(content) -> content
          _ -> "{}"
        end

      decoded =
        case Jason.decode(content) do
          {:ok, decoded} when is_map(decoded) ->
            Logger.debug("Decoded profile content: #{inspect(decoded)}")
            decoded

          {:ok, _} ->
            Logger.warning("Profile content decoded but not a map: #{inspect(content)}")
            %{}

          error ->
            Logger.error("Failed to decode profile content: #{inspect(error)}")
            %{}
        end

      # Get pubkey from either string or atom key
      pubkey = event["pubkey"] || event[:pubkey]
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
  end

  @spec parse_profile_event(any()) :: map()
  def parse_profile_event(_), do: %{}

  # Placeholder extract functions
  @spec extract_title(nostr_event()) :: binary() | nil
  defp extract_title(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "title" end) do
      ["title", title | _rest] -> title
      # Returns nil if the title tag is not found
      _ -> nil
    end
  end

  @spec extract_summary(nostr_event()) :: binary() | nil
  defp extract_summary(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "summary" end) do
      ["summary", summary | _rest] -> summary
      # Returns nil if the summary tag is not found
      _ -> nil
    end
  end

  @spec extract_image_url(nostr_event()) :: binary() | nil
  defp extract_image_url(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "image" end) do
      ["image", image | _rest] -> image
      # Returns nil if the image tag is not found
      _ -> nil
    end
  end

  @spec extract_published_at(nostr_event()) :: DateTime.t()
  defp extract_published_at(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "published_at" end) do
      ["published_at", timestamp | _rest] ->
        case Integer.parse(timestamp) do
          {timestamp, _} ->
            try do
              DateTime.from_unix!(timestamp)
            rescue
              _ -> fallback_to_created_at(event)
            end

          :error ->
            fallback_to_created_at(event)
        end

      _ ->
        fallback_to_created_at(event)
    end
  end

  @spec fallback_to_created_at(nostr_event()) :: DateTime.t()
  defp fallback_to_created_at(event) do
    case event["created_at"] do
      nil ->
        DateTime.utc_now()

      created_at ->
        try do
          DateTime.from_unix!(created_at)
        rescue
          _ -> DateTime.utc_now()
        end
    end
  end

  @spec extract_first_tag(nostr_event(), binary()) :: binary() | nil
  defp extract_first_tag(event, name) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == name end) do
      [^name, image | _rest] -> image
      _ -> nil
    end
  end

  @spec extract_first_image(nostr_event()) :: binary() | nil
  defp extract_first_image(event) do
    tags = event["tags"] || []

    # Look for different image tag patterns commonly used in Kind 20 picture posts
    Enum.find_value(tags, fn tag ->
      case tag do
        # Standard image tag: ["image", "url"]
        ["image", url | _] when is_binary(url) ->
          url

        # URL tag pointing to image: ["url", "image_url"]
        ["url", url | _] when is_binary(url) ->
          if is_image_url?(url), do: url, else: nil

        # Image metadata tag: ["imeta", "url", "image_url", ...]
        ["imeta", "url", url | _] when is_binary(url) ->
          url

        # Fallback for any other pattern starting with known image indicators
        [type, url | _] when type in ["img", "picture"] and is_binary(url) ->
          url

        _ ->
          nil
      end
    end)
  end

  @spec is_image_url?(binary()) :: boolean()
  defp is_image_url?(url) do
    String.contains?(url, ".jpg") or String.contains?(url, ".jpeg") or
      String.contains?(url, ".png") or String.contains?(url, ".gif") or
      String.contains?(url, ".webp") or String.contains?(url, ".bmp") or
      String.contains?(url, ".svg")
  end

  @spec render_markdown(binary()) :: binary()
  defp render_markdown(content) when is_binary(content) do
    content
    |> replace_http_with_https()
    |> Earmark.as_html!()
  end

  @spec render_markdown(any()) :: binary()
  defp render_markdown(_), do: ""

  @spec replace_http_with_https(nil) :: nil
  def replace_http_with_https(nil), do: nil

  @spec replace_http_with_https(binary()) :: binary()
  def replace_http_with_https(text) do
    Regex.replace(~r/http:\/\//, text, "https://")
  end
end
