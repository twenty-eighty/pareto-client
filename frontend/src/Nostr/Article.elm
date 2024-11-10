module Nostr.Article exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, EventFilter, Kind(..), Tag(..), TagReference(..), numberForKind)
import Nostr.Profile exposing (ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (EventId, PubKey)
import Set
import Time
import Ui.Profile exposing (defaultProfileImage)

type alias Article =
    { author : String
    , id : EventId
    , kind : Kind
    , alt : Maybe String
    , content : String
    , image : Maybe String
    , isValid : Maybe String
    , publishedAt : Maybe Time.Posix
    , summary : Maybe String
    , title : Maybe String
    , url : Maybe String
    , identifier : Maybe String
    , hashtags : List String
    }

-- assume published date is event creation date unless specified explicitely in publishedAt tag
emptyArticle : PubKey -> EventId -> Kind -> Time.Posix -> String -> Article
emptyArticle author eventId kind createdAt content =
    { author = author
    , id = eventId
    , kind = kind
    , alt = Nothing
    , content = content
    , image = Nothing
    , isValid = Nothing
    , publishedAt = Just createdAt
    , summary = Nothing
    , title = Nothing
    , url = Nothing
    , identifier = Nothing
    , hashtags = []
    }


articleFromEvent : Event -> Result (List String) Article
articleFromEvent event =
    let
        articleWithoutTags =
            emptyArticle event.pubKey event.id event.kind event.createdAt event.content

        (builtArticle, buildingErrors) =
            event.tags
            |> List.foldl (\tag (article, errors) ->
                case tag of 
                    HashTag hashtag ->
                        ({ article | hashtags = article.hashtags ++ [hashtag] }, errors)

                    AltTag alt ->
                        ({ article | alt = Just alt }, errors)

                    EventDelegationTag identifier ->
                        ({ article | identifier = Just identifier }, errors)

                    ImageTag image _ ->
                        ({ article | image = Just image }, errors)

                    PublishedAtTag publishedAt ->
                        ({ article | publishedAt = Just publishedAt }, errors)

                    SummaryTag summary ->
                        ({ article | summary = Just summary }, errors)

                    TitleTag title ->
                        ({ article | title = Just title }, errors)

                    _ ->
                        (article, errors)
                    ) (articleWithoutTags, [])
    in
    case (builtArticle, buildingErrors) of
        (article, []) ->
            Ok article
        
        (_, errors) ->
            Err errors


{-
["EVENT","kinds:30023-limit-903",
{"content":"![Image](https://substackcdn.com/image/fetch/w_1456,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2F060476b2-e016-40a3-82de-b0924d7d8def_1120x832.png) _Was überzeugte die Politik von der Beschaffung des Ebola-Medikaments Remdesivir? Das RKI schreibt schon den Namen falsch (Screenshot: Ergebnisprotokoll vom 09.05.2020)_\n\nDas Wichtige findet sich mitunter dort, wo man nicht schaut oder nicht schauen soll. Die im ersten Zug durch Paul Schreyer freigeklagten Protokolle waren teils unvollständig. Das Ergebnisprotokoll vom 09.05.2020 zum Beispiel galt für das RKI als unauffindbar.\n\nNicht jedoch für die Schwarmintelligenz.\n\n[Als vor kurzem alle RKI-Protokolle entschwärzt veröffentlicht wurden](https://www.freischwebende-intelligenz.org/p/rki-protokolle-auf-historischem-blindflug), begann im Netz die Suche nach den Goldnuggets. Der Datenanalyst [Muh](https://twitter.com/MeowMuhCow) fand das Ergebnisprotokoll vom 09.05.2020 im Ordner mit Zusatzmaterial vom 14.05.2020. Ein Glücksfund, der wohl nur möglich ist, weil sich nun jeder selbst ein Bild machen kann und in den Dokumenten herumstöbern kann. Am Ende ist es die Arbeit von vielen, die das ganze Bild entstehen lässt.\n\n* * *\n\n**AUFRUF:** Helfen Sie mit beim Durchstöbern der RKI-Dokumente! Auf der Seite [www.rkileak.com](https://www.rkileak.com/) können Sie die Dokumente nach Stichworten durchsuchen. Sachdienliche Hinweise erbeten an: **kontakt@idw-europe.org**\n\n* * *\n\nWar das Verschwinden des Dokuments Schludrigkeit oder Absicht? Eine versteckte Form der Dokumentenunterdrückung? Mit Suchfunktion wäre das fehlende Protokoll für RKI-Mitarbeiter, die auch über das Zusatzmaterial verfügten, nicht allzu schwer zu finden gewesen. Stattdessen meldete man es als verschollen und gab es auf Klage von Paul Schreyer nicht heraus. Und dann war da ja auch noch der „Serverbrand“ im RKI…\n\n![Image](https://substackcdn.com/image/fetch/w_1456,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2Fa4f9cdd9-3f4c-4554-a630-ef69ca4828f3_1474x812.png) _Sicher nur ein Zufall…(Screenshot RBB24)_\n\nBisher waren vor allem zwei Informationen aus diesem Protokoll bekannt und interessant:\n\n- Zum einen, dass die Inzidenzwerte „politisch gesetzt“ sind. Es war die Politik und nicht die Wissenschaft, die ab einer Inzidenz von 50 positiven Tests auf 100 000 Einwohner (in den Mainstream-Fakemedien flapsig „Ansteckungen“ genannt), automatisch bestimmte Maßnahmen in Kraft setzte.\n\n- Zum anderen, dass es keine Evidenz für Masken im Freien gibt. Von der Politik und den Medien wurde das Gegenteil kolportiert. \n\nEin interessanter Hinweis findet sich im Protokoll auch auf das Medikament Remdesivir von Gilead, über das ich schon mehrfach berichtet habe. Dieses sehr teure Medikament wurde als Behandlung in der Frühphase der Pandemie trotz bedingten Zulassungen u.a. von Anthony Fauci empfohlen, obwohl es früh Hinweise auf die Gefährlichkeit des Medikaments gab. Wie viele Coronatote gehen auf das Konto von Remdesivir und den Einsatz von Beatmungsgeräten?\n\nIn einem [früheren Beitrag](https://www.freischwebende-intelligenz.org/p/guenthard-nzz-remdesivir-tote?utm_source=publication-search) schrieb ich:\n\n_„In der Schweiz und in den USA gab es frühe Notzulassungen. Dabei ist die Studienlage alarmierend. Ursprünglich war das Medikament gegen Ebola im Einsatz. In einer Studie von 2019, die im [New England Journal of Medicine](https://www.nejm.org/doi/full/10.1056/NEJMoa1910993) veröffentlicht wurde, zeigte Remdesivir die höchste Mortalität aller Präparate: 53,1% der mit Remdesivir Behandelten verstarben. In anderen Studien, auch bezüglich Covid, wurde die Effektivität von Remdesivir angezweifelt, selbst das Forbes Magazin und die New York Times berichteten kritisch.“_\n\nEs war Anthony Fauci, der das Medikament Remdesivir in den USA durchdrückte. Und das trotz Hinweisen anderer Forscher, dass die Sterblichkeitsrate in Ländern, wo es im Einsatz war höher war, als dort, wo es nicht benutzt wurde. Die Warnung verhallte folgenlos, wie so oft. Die Belege aus den Mails an Fauci finden Sie hier:\n\nWas bewegte nun die Politik in Deutschland, insbesondere das Bundesgesundheitsministerium, das Medikament Remdesivir einzusetzen?\n\nWar es der Drang, es den USA nachzumachen? Auch hier könnte politischer Druck eine größere Wirkung gehabt haben als die wissenschaftliche Evidenzlage. Eine überzeugende neutrale Studie zu Remdesivir sucht man vergebens, alarmierende Hinweise findet man zuhauf.\n\n**Das bestätigen nun auch die Ergebnisprotokolle des RKI:**\n\n- Am 24.01.2020 ist Remdesivir “im Menschen nicht wirksam”\n\n![Image](https://substackcdn.com/image/fetch/w_1456,c_limit,f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2F419db330-33aa-4a79-b40c-6eec6a7e07c1_1472x266.png) _Ein nicht wirksames Medikament? Lasst uns gleich eine Bestellung aufgeben! (Screenshot: RKI-Protokoll vom 24.01.2020)_\n- Am 27.01.2020 wird zur Wirksamkeit eine „dünne Evidenzlage“ vermerkt.\n\n- Am 24.04.2020 vermerkte das RKI, dass Studien „nicht so vielversprechend“ seien. \n\n- Am 02.05. 2020 wusste man, dass in einer chinesischen Studie der Nutzen in schweren Fällen sehr gering sei. Deutschland hat trotzdem 1000 Einheiten Remdesivir von Gilead erstanden. \n\n- Am 09.05.2020 notierte das RKI zu Remdesivir:\n\n- Besonders sticht der Satz ins Auge: „Für die Verwendung wäre klinische Studie eher wünschenswert“. Kam diese Studie irgendwann? Oder war sie am Ende dann eher politisch doch nicht so wünschenswert?\n\n- Im Juli 2020 folgte die bedingte Zulassung von Remdesivir. \n\nDies wirft eine ganze Reihe von Fragen auf: Warum bestellt die Politik ein Medikament, von dessen Wirkung die mit Sachverstand ausgestattete Behörde (RKI) ganz offensichtlich nicht überzeugt ist, bzw. deren Gefährlichkeit bekannt ist und welches auch noch extrem teuer ist? Mehrtägige Behandlungen kosten Tausende von Euros.\n\nWo sind die angeblich wünschenswerten Studien zu Remdesivir, nachdem die bisherige Studienlage ziemlich desaströs aussah? Wieso hat das RKI nicht auf einer neuen Studie bestanden?\n\nAll das sind Fragen aus einer Zeit, als man noch daran glauben durfte, die Politik betreibe tatsächlich so etwas wie Gesundheitsschutz der Bevölkerung.\n\nAus heutiger Sicht ist eine andere Erklärung plausibler. Eine Pandemie ohne Panik kommt nicht weit. Für Panik braucht es angstgenerierende Ereignisse. „Die Toten von Bergamo“ waren so ein Ereignis. Die Gabe von Medikamenten wie Remdesivir in Verbindung mit Beatmungen könnte geholfen haben, die Todesstatistiken in die Höhe zu treiben, um Schocknachrichten über die ansonsten kaum ausgelasteten Intensivstationen verbreiten zu können. Es musste ja irgendwie in die Köpfe, dass Corona angeblich eine höchst gefährliche Seuche ist. Hinzu kommt: Medikamentengaben sind eine der häufigsten Todesursachen überhaupt und Obduktionen sind systematisch unterblieben. Jeder Tote mit positivem Corona-Test galt als Corona-Toter.\n\nIch wiederhole daher meine Frage aus meinem früheren Beitrag: Wie viele Coronatote waren Remdesivir-Tote?\n\nAuch um diese Blindstelle wird eine Aufarbeitung nicht herum kommen.\n\n* * *\n\n_ **ANZEIGE: Da ich oft gefragt werde, wie man am einfachsten Bitcoin kauft:** Mit der erfolgreichen Schweizer **[App Relai](https://relai.app/de/?af_xp=custom&source_caller=ui&pid=INFLUENCER&is_retargeting=true&af_click_lookback=7d&shortlink=eo5zpzew&af_channel=branded_url&af_inactivity_window=30d&c=Milosz%20Matuszek)** geht es in wenigen Schritten und ohne komplizierte Anmeldung. Man kann dort auch einfache Sparpläne einrichten. Niemand hat Zugriff auf Ihre Bitcoin, außer Sie selbst. Mit dem Referral-Code **REL54052** sparen Sie Gebühren. (keine Finanzberatung)._\n\n* * *\n\n**Stromaufwärts zur Quelle geht es weiter. Sehen wir uns auf der Sommer-Lesereise? Ich besuche diesmal vor allem den Norden Deutschlands, [alle Termine finden Sie hier](https://nuudel.digitalcourage.de/Rm6yhkCPIrCtvdLZ) (bitte eintragen).**\n\n* * *\n\n_ **Herzlichen Dank, dass Sie meine Arbeit unterstützen!** _\n\n_Ich kann Ihnen auch manuell einen Zugang zur Publikation einrichten, wenn Sie lieber per [Paypal](https://www.paypal.com/paypalme/cancelculture?locale.x=de_DE), Überweisung oder Bitcoin (einmal Jahresbeitrag, ewiger Zugang) bezahlen. ","created_at":1725833172,"id":"f14630a0498c3685656d5ae833b31f44b9e132d8ccf286439bfe27427c0c3824","kind":30023,"pubkey":"dfe02069220e3f7a0ba6f641fc36cbbde3a42044a6cc9aebc4a44993d3e2fa29","sig":"3fd130ee3394fd6b1b76ba3cdf1f8a16f3bbd4d51ab85f1f32c6900855138c084ead647778eb1b54c3e980371466b68f25371e1628501b771d9523934f6c5147"
,"tags":[
    ["d","147330735"],
    ["title","RKI-Protokolle: Das verschollene Ergebnisprotokoll"],
    ["summary","Ein verloren geglaubtes RKI-Ergebnisprotokoll ist Dank des neuesten Leaks aufgetaucht. Was steht drin?"],
    ["published_at","1722833370"],
    ["language_tags","de"],
    ["t","pareto"],
    ["image","https://substackcdn.com/image/fetch/f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2F060476b2-e016-40a3-82de-b0924d7d8def_1120x832.png"],
    ["a","34550:8127df93d8453767aa11e74206f48aeea30d3d65a383c98d243b031fc7446afb:Pareto"],
    ["published_at","1722833370"],
    ["alt","This is a long form article, you can read it in https://pareto-showcase.onrender.com/milosz/147330735"]
    ]
}]
-}


tagReference : Article -> TagReference
tagReference article =
    case article.identifier of
        Just identifier ->
            TagReferenceCode KindLongFormContent article.author identifier

        Nothing ->
            TagReferenceEventId article.id


filterMatchesArticle : EventFilter -> Article -> Bool
filterMatchesArticle filter article =
    True
    |> filterMatchesAuthor filter article
    -- TODO: implement for other filter criteria


filterMatchesAuthor : EventFilter -> Article -> Bool -> Bool
filterMatchesAuthor filter article result =
    if result then
        filter.authors
        |> Maybe.map (List.member article.author)
        |> Maybe.withDefault True
    else
        False

removeLeadingHash : String -> String
removeLeadingHash tag =
    if String.startsWith "#" tag then
        String.dropLeft 1 tag
    else
        tag

uniqueArticleAuthors : List Article -> List String
uniqueArticleAuthors articles =
    articles
    |> List.map .author
    |> Set.fromList
    |> Set.toList


addArticles : List Article -> List Article -> List Article
addArticles articleList newArticles =
    List.foldl addArticle articleList newArticles

addArticle : Article -> List Article -> List Article
addArticle newArticle articleList =
    if List.any (isArticleWithIdAndAuthor newArticle.author newArticle.id) articleList then
        newArticle :: articleList
    else
        newArticle :: articleList

isArticleWithIdAndAuthor : PubKey -> EventId -> Article -> Bool
isArticleWithIdAndAuthor author articleId article =
    article.author == author && article.id == articleId

decodeUnixTime : Decoder Time.Posix
decodeUnixTime =
    Decode.int 
        |> Decode.map (\timeInt -> Time.millisToPosix (timeInt * 1000)
        )
