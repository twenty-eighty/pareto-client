defmodule NostrBackend.Content do
  alias NostrBackend.NostrClient

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
  def parse_article_event(event) do
    %{
      article_id: event["id"],
      title: extract_title(event) |> NostrBackend.Substitution.replace_randomly(),
      description: extract_summary(event) |> NostrBackend.Substitution.replace_randomly(),
      content: render_markdown(event["content"]),
      image_url: extract_image_url(event)
    }
  end

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

  def parse_profile_event(event) do
    content =
      event["content"]
      |> Jason.decode!()

    %{
      profile_id: event["pubkey"],
      name: content["name"],
      username: content["username"],
      about: content["about"],
      banner: content["banner"],
      image: content["image"],
      display_name: content["display_name"],
      website: content["website"],
      lud16: content["lud16"],
      nip05: content["nip05"],
      picture: content["picture"]
    }
  end

  # Placeholder extract functions
  defp extract_title(event) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == "title" end) do
      ["title", title | _rest] -> title
      # Returns nil if the title tag is not found
      _ -> nil
    end
  end

  # ["EVENT","UCYXt9-k",{"content":"### Gratitude\nIn light of all the doom, gloom, and meaningless state sponsored violence in the world these days, I want to practice shedding light on some dope things I have going on lately that I am grateful for. To start, I'm always grateful for my dog's health and companionship. Kaido is a 4 year old 15 lb Chiweeny with an under bite and a big attitude who never lets anybody sneak up behind me.  \n![My dog on my back](https://i.nostr.build/GiDsBzIb4doGBEt2.jpg)\n\n\nI'm grateful for the community I've plugged into since I made my Nostr keys and won a creator account with [nostr.build](https://nostr.build/) in a blog contest last year. I'm coming out of the [Creator Residency at nos.social](https://nos.social/creator-residency) this month with a new group of internet friends and was even just awarded a grant from Yakihonne to spin up a relay for a youth organization I am involved with. \n\nMy grant proposal writing skills are sharpening thanks to my participation in a cohort of young \"World Builders\" who meet weekly via video chat to learn the ins and outs of fundraising and building ethical social impact organizations. A friend of mine I recently reconnected with introduced me to the facilitator of this group, a [professional in the development space](https://www.youtube.com/watch?v=Jfq1Ou9C5Ws) on a mission to \"hack\" the non profit sector by training the next generation of youth leaders. It's exactly what I needed to evolve my writing skills and apply for funding to build the better world I want to see for the next generation.\n\nThe friend who connected me and I met as foster youth members of a California-based social impact organization dedicated to transforming policy and practice in the foster care and juvenile justice systems. We fell out of touch after high school before crossing paths again a few years ago while I was a staff member at the organization. He's now a professional software developer who [runs a learning community](https://gitfitcode.com) dedicated to helping folks learn programming and get into tech roles. My participation in the community has rekindled a passion for computers and the internet that started back in middle school when I first flashed a custom firmware on my Playstation Portable but paused in high school as I became increasingly sick of the internet and more interested in politics and media production. These days, I find myself increasingly sick of the corporate politics and media that define our shared realities and more interested in fixing the internet. \n\n### Crisis\nIn today's perpetual state of polycrisis, systems-involved youth (which include unhoused, foster, and incarcerated youth or orphans) are among the most vulnerable in our society. Higher rates of trauma-induced mental illness culminate into poor life outcomes for this population, including higher rates of unemployment and incarceration. Compounding this is the fact that they use social media at higher rates than other demographics due to a lack of robust social support networks, making them prime targets for bullies and human traffickers on the web. [The Center for Humane Technology reports on their website](https://www.humanetech.com/future-generations) that \"more screen time is associated with mental health and behavioral problems, poor academic performance, and worse sleep\" for youth.  \n\nWe had a situation at the California-based organization I mentioned where a frustrated youth member created a \"finsta\" (fake Instagram account) to drag the Executive Director (one of the founding youth members of the organization in the 80's) and some staff (including me) through the digital mud. The youth tagged the organization's account and spread hurtful memes and rumors throughout our network. It even sparked a staff rebellion in which a disgruntled coordinator sent out a letter to our network detailing their personal beef with management and announcing their departure. A petition of no confidence in the Executive Director was circulated, resulting in their resignation from the organization they helped start decades ago. Needless to say, the whole thing left the community pretty traumatized and we lost a few great staff members.  \n\nI can't help but imagine how the situation would have played out if our organization had something like a white-listed Nostr relay to organize our community around instead of an Instagram account. The burner account would not have been able to post anything to the relay, so the memes and rumors probably wouldn't have spread as far or as quickly as they did. The white-listed youth members of the communications committee I ran could have posted content to the relay to ensure verified information was spread throughout the membership. Maybe this kind of setup would have created an environment for the frustrated member to feel more connected to the organization and it's mission and prevented such a mess from ever taking place. In the moment, I felt powerless to stop it. \n\n### Digital Neighborhoods\nWhen I catch myself feeling down about the state of the world and how powerless I've felt to change it over the last year, I try to remember Rabble's metaphor about building digital neighborhoods in the [recent presentation he gave at Nostriga](https://www.youtube.com/watch?v=XUsk7cqZyKU&t=983s). Before long, I'm reaching for my digital construction hat on my way to my computer to jot some ideas down. I've heard a lot of lip service at different non profits I've worked for over the years be given to the idea of safe physical spaces for youth (like playgrounds and soccer fields), but I never heard this idea extended to the digital spaces where the majority of young people spend an increasing amount of their time. These days, I serve on the board of directors for a different organization that I intend to build a digital neighborhood for with the Yakihonne Relay Grant I was awarded recently. I've been building transnational support networks for youth involved in state care with the International Foster Care Alliance since 2016 when I first trained a delegation of Japanese foster youth on community organizing strategies. There are a few reasons why I think Nostr is a natural fit for systems-involved youth. \n\n![Miguel with IFCA youth](https://i.nostr.build/1hoLlq4q4iA2prIT.jpg)\n\n1. Confidentiality laws surrounding minors in state care make it hard for organizations like ours to engage them in storytelling on social media because of the personal identifying information required to participate on corporate platforms. A Nostr key pair empowers a youth with a layer of anonymity in line with one of the tips in the [Child Welfare Gateway's Social Media Factsheet](https://cwig-prod-prod-drupal-s3fs-us-east-1.s3.amazonaws.com/public/documents/smtips_parent.pdf) about limiting the sharing of personal information. This anonymity grows their capacity to share painful pasts intentionally and with purpose. Since Nostr profiles can't be traced to a personal identity, youth advocates are empowered to use their voice to make a difference.\n\n2. For foster youth who are disempowered in all other areas of their lives, ownership of their identity and data gives them control over how they present themselves and engage with others online. This gives way for an increased sense of connectedness, personal capacity, and resilience in storytelling without fear of being tokenized. Our organization's web presence will evolve into a powerful tool for narrative change and foster deeper connections among youth members globally, strengthening their cultural identities while contributing to a broader conversation on child welfare reform.\n\n3. Next generation features and a [vast app ecosystem](https://nostrapps.com/) will transcend geographical boundaries and time zones, significantly enhancing communication and collaboration between organizations in the alliance. In particular, in app translations provided by many Nostr clients will allow youth to socialize with their counterparts in other countries across language barriers and facilitate smoother communication. \n\nWhat's not to look forward to? I'm grateful to be part of this movement and for the opportunities the community has provided for me up until now. We've come a long way since GeoCities and Myspace but, in the words of Tim Berner's Lee, \"The future is still so much bigger than the past.\" ","created_at":1728647603,"id":"28d34a3c6da219194bbe1f294489c816691bcd3364b5ee2c0560297d263d3a01","kind":30023,"pubkey":"ec965405e11a6a6186b27fa451a2ffc1396ede7883d2ea11c32fbd2c63996966","sig":"63c1f0b0af26fd774394569e5115ed33f44c783fd0a33f9e22d6b04b203d548708b973e9afcd1842e8fe2c5425138c023ea55522540cbb0417ba3a39f4dd9d2f",
  # "tags":[["d","1728621062952"],
  # ["title","For the Youth"],
  # ["summary","I got things to be grateful for."],
  # ["a","34550:731fec28760cf14e5de3e9677f8f728be01b1051adbec22bc42c868597669b82:OrganicBodyPolitic-SelfGoverningCommunityOrganizing"],
  # ["published_at","1728627880"],
  # ["alt","This is a long form article, you can read it in https://habla.news/a/naddr1qvzqqqr4gupzpmyk2sz7zxn2vxrtylay2x30lsfedm083q7jagguxtaa933ej6txqqxnzdej8qmryvfsxcerjdfjrhl2c6"],
  # ["a","34550:731fec28760cf14e5de3e9677f8f728be01b1051adbec22bc42c868597669b82:OrganicBodyPolitic-SelfGoverningCommunityOrganizing"],
  # ["published_at","1728627880"],["t","youth"],["t","organization"],["t","social"],["t","impact"],
  # ["image","https://i.nostr.build/FZiYfwRJngYbe5K4.jpg"],
  # ["a","34550:731fec28760cf14e5de3e9677f8f728be01b1051adbec22bc42c868597669b82:OrganicBodyPolitic-SelfGoverningCommunityOrganizing"],
  # ["published_at","1728627880"]]}]

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

  defp extract_first_tag(event, name) do
    tags = event["tags"] || []

    case Enum.find(tags, fn tag -> List.first(tag) == name end) do
      [^name, image | _rest] -> image
      _ -> nil
    end
  end

  defp render_markdown(content) when is_binary(content) do
    Earmark.as_html!(content)
  end

  defp render_markdown(_), do: ""
end
