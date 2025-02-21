module Oembed.ProviderTemp exposing (..)

import List.Extra
import Regex exposing (Regex)



-- TODO: Keep this file unless https://github.com/dillonkearns/elm-oembed/pull/22 is merged,
-- then use original package for check


hardcodedMatches : String -> Bool
hardcodedMatches url =
    lookup all url /= Nothing


lookup : List { schemes : List Regex, url : String } -> String -> Maybe String
lookup customProviders inputUrl =
    (customProviders ++ all)
        |> List.Extra.find
            (\provider ->
                provider.schemes
                    |> List.any (\scheme -> Regex.contains scheme inputUrl)
            )
        |> Maybe.map .url


type alias Provider =
    { schemes : List Regex
    , url : String
    }


regex : String -> Regex
regex string =
    string
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


all : List Provider
all =
    [ { url = "http://www.23hq.com/23/oembed"
      , schemes = [ regex "http://www\\.23hq\\.com/.*/photo/.*" ]
      }
    , { url = "http://play.adpaths.com/oembed/*"
      , schemes = [ regex "http://play\\.adpaths\\.com/experience/.*" ]
      }
    , { url = "https://alpha-api.app.net/oembed"
      , schemes = [ regex "https://alpha\\.app\\.net/.*/post/.*", regex "https://photos\\.app\\.net/.*/.*" ]
      }
    , { url = "https://api.altrulabs.com/social/oembed"
      , schemes = [ regex "https://app\\.altrulabs\\.com/.*/.*\\?answer_id=.*" ]
      }
    , { url = "https://live.amcharts.com/oembed"
      , schemes = [ regex "http://live\\.amcharts\\.com/.*", regex "https://live\\.amcharts\\.com/.*" ]
      }
    , { url = "https://animatron.com/oembed/json"
      , schemes = [ regex "https://www\\.animatron\\.com/project/.*", regex "https://animatron\\.com/project/.*" ]
      }
    , { url = "http://animoto.com/oembeds/create"
      , schemes = [ regex "http://animoto\\.com/play/.*" ]
      }
    , { url = "https://display.apester.com/oembed"
      , schemes = [ regex "https://renderer\\.apester\\.com/v2/.*\\?preview=true&iframe_preview=true" ]
      }
    , { url = "https://app.archivos.digital/oembed/"
      , schemes = [ regex "https://app\\.archivos\\.digital/app/view/.*" ]
      }
    , { url = "https://audioclip.naver.com/oembed"
      , schemes = [ regex "https://audioclip\\.naver\\.com/channels/.*/clips/.*", regex "https://audioclip\\.naver\\.com/audiobooks/.*" ]
      }
    , { url = "https://www.audiomack.com/oembed"
      , schemes = [ regex "https://www\\.audiomack\\.com/song/.*", regex "https://www\\.audiomack\\.com/album/.*", regex "https://www\\.audiomack\\.com/playlist/.*" ]
      }
    , { url = "http://audiosnaps.com/service/oembed"
      , schemes = [ regex "http://audiosnaps\\.com/k/.*" ]
      }
    , { url = "https://backtracks.fm/oembed"
      , schemes = [ regex "https://backtracks\\.fm/.*/.*/e/.*", regex "https://backtracks\\.fm/.*", regex "http://backtracks\\.fm/.*" ]
      }
    , { url = "https://www.beautiful.ai/api/oembed"
      , schemes = []
      }
    , { url = "https://blackfire.io/oembed"
      , schemes = [ regex "https://blackfire\\.io/profiles/.*/graph", regex "https://blackfire\\.io/profiles/compare/.*/graph" ]
      }
    , { url = "http://boxofficebuz.com/oembed"
      , schemes = []
      }
    , { url = "https://view.briovr.com/api/v1/worlds/oembed/"
      , schemes = [ regex "https://view\\.briovr\\.com/api/v1/worlds/oembed/.*" ]
      }
    , { url = "https://buttondown.email/embed"
      , schemes = [ regex "https://buttondown\\.email/.*" ]
      }
    , { url = "https://cmc.byzart.eu/oembed/"
      , schemes = [ regex "https://cmc\\.byzart\\.eu/files/.*" ]
      }
    , { url = "http://cacoo.com/oembed.json"
      , schemes = [ regex "https://cacoo\\.com/diagrams/.*" ]
      }
    , { url = "http://carbonhealth.com/oembed"
      , schemes = [ regex "https://carbonhealth\\.com/practice/.*" ]
      }
    , { url = "http://img.catbo.at/oembed.json"
      , schemes = [ regex "http://img\\.catbo\\.at/.*" ]
      }
    , { url = "http://view.ceros.com/oembed"
      , schemes = [ regex "http://view\\.ceros\\.com/.*" ]
      }
    , { url = "http://embed.chartblocks.com/1.0/oembed"
      , schemes = [ regex "http://public\\.chartblocks\\.com/c/.*" ]
      }
    , { url = "http://chirb.it/oembed.json"
      , schemes = [ regex "http://chirb\\.it/.*" ]
      }
    , { url = "https://www.circuitlab.com/circuit/oembed/"
      , schemes = [ regex "https://www\\.circuitlab\\.com/circuit/.*" ]
      }
    , { url = "https://www.clipland.com/api/oembed"
      , schemes = [ regex "http://www\\.clipland\\.com/v/.*", regex "https://www\\.clipland\\.com/v/.*" ]
      }
    , { url = "http://api.clyp.it/oembed/"
      , schemes = [ regex "http://clyp\\.it/.*", regex "http://clyp\\.it/playlist/.*" ]
      }
    , { url = "https://codehs.com/api/sharedprogram/*/oembed/"
      , schemes = [ regex "https://codehs\\.com/editor/share_abacus/.*" ]
      }
    , { url = "http://codepen.io/api/oembed"
      , schemes = [ regex "http://codepen\\.io/.*", regex "https://codepen\\.io/.*" ]
      }
    , { url = "https://codepoints.net/api/v1/oembed"
      , schemes = [ regex "http://codepoints\\.net/.*", regex "https://codepoints\\.net/.*", regex "http://www\\.codepoints\\.net/.*", regex "https://www\\.codepoints\\.net/.*" ]
      }
    , { url = "https://codesandbox.io/oembed"
      , schemes = [ regex "https://codesandbox\\.io/s/.*", regex "https://codesandbox\\.io/embed/.*" ]
      }
    , { url = "http://www.collegehumor.com/oembed.json"
      , schemes = [ regex "http://www\\.collegehumor\\.com/video/.*" ]
      }
    , { url = "https://commaful.com/api/oembed/"
      , schemes = [ regex "https://commaful\\.com/play/.*" ]
      }
    , { url = "http://coub.com/api/oembed.json"
      , schemes = [ regex "http://coub\\.com/view/.*", regex "http://coub\\.com/embed/.*" ]
      }
    , { url = "http://crowdranking.com/api/oembed.json"
      , schemes = [ regex "http://crowdranking\\.com/.*/.*" ]
      }
    , { url = "https://staging.cyranosystems.com/oembed"
      , schemes = [ regex "https://staging\\.cyranosystems\\.com/msg/.*", regex "https://app\\.cyranosystems\\.com/msg/.*" ]
      }
    , { url = "http://api.dailymile.com/oembed?format=json"
      , schemes = [ regex "http://www\\.dailymile\\.com/people/.*/entries/.*" ]
      }
    , { url = "https://www.dailymotion.com/services/oembed"
      , schemes = [ regex "https://www\\.dailymotion\\.com/video/.*" ]
      }
    , { url = "https://embed.deseretnews.com/"
      , schemes = [ regex "https://.*\\.deseretnews\\.com/.*" ]
      }
    , { url = "http://backend.deviantart.com/oembed"
      , schemes = [ regex "http://.*\\.deviantart\\.com/art/.*", regex "http://.*\\.deviantart\\.com/.*#/d.*", regex "http://fav\\.me/.*", regex "http://sta\\.sh/.*", regex "https://.*\\.deviantart\\.com/art/.*", regex "https://.*\\.deviantart\\.com/.*/art/.*", regex "https://sta\\.sh/.*\",", regex "https://.*\\.deviantart\\.com/.*#/d.*\"" ]
      }
    , { url = "https://*.didacte.com/cards/oembed'"
      , schemes = [ regex "https://.*\\.didacte\\.com/a/course/.*" ]
      }
    , { url = "https://www.ultimedia.com/api/search/oembed"
      , schemes = [ regex "https://www\\.ultimedia\\.com/central/video/edit/id/.*/topic_id/.*/", regex "https://www\\.ultimedia\\.com/default/index/videogeneric/id/.*/showtitle/1/viewnc/1", regex "https://www\\.ultimedia\\.com/default/index/videogeneric/id/.*" ]
      }
    , { url = "http://www.dipity.com/oembed/timeline/"
      , schemes = [ regex "http://www\\.dipity\\.com/.*/.*/" ]
      }
    , { url = "https://www.docdroid.net/api/oembed"
      , schemes = [ regex "https://.*\\.docdroid\\.net/.*", regex "http://.*\\.docdroid\\.net/.*", regex "https://docdro\\.id/.*", regex "http://docdro\\.id/.*" ]
      }
    , { url = "http://dotsub.com/services/oembed"
      , schemes = [ regex "http://dotsub\\.com/view/.*" ]
      }
    , { url = "https://api.d.tube/oembed"
      , schemes = [ regex "https://d\\.tube/v/.*" ]
      }
    , { url = "http://edocr.com/api/oembed"
      , schemes = [ regex "http://edocr\\.com/docs/.*" ]
      }
    , { url = "https://www.edumedia-sciences.com/oembed.json"
      , schemes = []
      }
    , { url = "https://www.edumedia-sciences.com/oembed.xml"
      , schemes = []
      }
    , { url = "http://egliseinfo.catholique.fr/api/oembed"
      , schemes = [ regex "http://egliseinfo\\.catholique\\.fr/.*" ]
      }
    , { url = "http://embedarticles.com/oembed/"
      , schemes = [ regex "http://embedarticles\\.com/.*" ]
      }
    , { url = "http://api.embed.ly/1/oembed"
      , schemes = []
      }
    , { url = "https://ethfiddle.com/services/oembed/"
      , schemes = [ regex "https://ethfiddle\\.com/.*" ]
      }
    , { url = "https://eyrie.io/v1/oembed"
      , schemes = [ regex "https://eyrie\\.io/board/.*", regex "https://eyrie\\.io/sparkfun/.*" ]
      }
    , { url = "https://www.facebook.com/plugins/post/oembed.json"
      , schemes = [ regex "https://www\\.facebook\\.com/.*/posts/.*", regex "https://www\\.facebook\\.com/photos/.*", regex "https://www\\.facebook\\.com/.*/photos/.*", regex "https://www\\.facebook\\.com/photo\\.php.*", regex "https://www\\.facebook\\.com/photo\\.php", regex "https://www\\.facebook\\.com/.*/activity/.*", regex "https://www\\.facebook\\.com/permalink\\.php", regex "https://www\\.facebook\\.com/media/set\\?set=.*", regex "https://www\\.facebook\\.com/questions/.*", regex "https://www\\.facebook\\.com/notes/.*/.*/.*" ]
      }
    , { url = "https://www.facebook.com/plugins/video/oembed.json"
      , schemes = [ regex "https://www\\.facebook\\.com/.*/videos/.*", regex "https://www\\.facebook\\.com/video\\.php" ]
      }
    , { url = "https://app.getfader.com/api/oembed"
      , schemes = [ regex "https://app\\.getfader\\.com/projects/.*/publish" ]
      }
    , { url = "https://faithlifetv.com/api/oembed"
      , schemes = [ regex "https://faithlifetv\\.com/items/.*", regex "https://faithlifetv\\.com/items/resource/.*/.*", regex "https://faithlifetv\\.com/media/.*", regex "https://faithlifetv\\.com/media/assets/.*", regex "https://faithlifetv\\.com/media/resource/.*/.*" ]
      }
    , { url = "https://www.fite.tv/oembed"
      , schemes = [ regex "https://www\\.fite\\.tv/watch/.*" ]
      }
    , { url = "https://flat.io/services/oembed"
      , schemes = [ regex "https://flat\\.io/score/.*", regex "https://.*\\.flat\\.io/score/.*" ]
      }
    , { url = "https://www.flickr.com/services/oembed/"
      , schemes = [ regex "http://.*\\.flickr\\.com/photos/.*", regex "http://flic\\.kr/p/.*", regex "https://.*\\.flickr\\.com/photos/.*", regex "https://flic\\.kr/p/.*" ]
      }
    , { url = "https://app.flourish.studio/api/v1/oembed"
      , schemes = [ regex "https://public\\.flourish\\.studio/visualisation/.*", regex "https://public\\.flourish\\.studio/story/.*" ]
      }
    , { url = "https://oembed.fontself.com/"
      , schemes = [ regex "https://catapult\\.fontself\\.com/.*" ]
      }
    , { url = "https://fiso.foxsports.com.au/oembed"
      , schemes = [ regex "http://fiso\\.foxsports\\.com\\.au/isomorphic-widget/.*", regex "https://fiso\\.foxsports\\.com\\.au/isomorphic-widget/.*" ]
      }
    , { url = "https://framebuzz.com/oembed/"
      , schemes = [ regex "http://framebuzz\\.com/v/.*", regex "https://framebuzz\\.com/v/.*" ]
      }
    , { url = "http://www.funnyordie.com/oembed.json"
      , schemes = [ regex "http://www\\.funnyordie\\.com/videos/.*" ]
      }
    , { url = "http://api.geograph.org.uk/api/oembed"
      , schemes = [ regex "http://.*\\.geograph\\.org\\.uk/.*", regex "http://.*\\.geograph\\.co\\.uk/.*", regex "http://.*\\.geograph\\.ie/.*", regex "http://.*\\.wikimedia\\.org/.*_geograph\\.org\\.uk_.*" ]
      }
    , { url = "http://www.geograph.org.gg/api/oembed"
      , schemes = [ regex "http://.*\\.geograph\\.org\\.gg/.*", regex "http://.*\\.geograph\\.org\\.je/.*", regex "http://channel-islands\\.geograph\\.org/.*", regex "http://channel-islands\\.geographs\\.org/.*", regex "http://.*\\.channel\\.geographs\\.org/.*" ]
      }
    , { url = "http://geo.hlipp.de/restapi.php/api/oembed"
      , schemes = [ regex "http://geo-en\\.hlipp\\.de/.*", regex "http://geo\\.hlipp\\.de/.*", regex "http://germany\\.geograph\\.org/.*" ]
      }
    , { url = "http://embed.gettyimages.com/oembed"
      , schemes = [ regex "http://gty\\.im/.*" ]
      }
    , { url = "https://api.gfycat.com/v1/oembed"
      , schemes = [ regex "http://gfycat\\.com/.*", regex "http://www\\.gfycat\\.com/.*", regex "https://gfycat\\.com/.*", regex "https://www\\.gfycat\\.com/.*" ]
      }
    , { url = "https://www.gifnote.com/services/oembed"
      , schemes = [ regex "https://www\\.gifnote\\.com/play/.*" ]
      }
    , { url = "https://giphy.com/services/oembed"
      , schemes = [ regex "https://giphy\\.com/gifs/.*", regex "http://gph\\.is/.*", regex "https://media\\.giphy\\.com/media/.*/giphy\\.gif" ]
      }
    , { url = "https://gloria.tv/oembed/"
      , schemes = []
      }
    , { url = "https://api.luminery.com/oembed"
      , schemes = [ regex "https://gtchannel\\.com/watch/.*" ]
      }
    , { url = "https://api.gyazo.com/api/oembed"
      , schemes = [ regex "https://gyazo\\.com/.*" ]
      }
    , { url = "https://hearthis.at/oembed/"
      , schemes = [ regex "https://hearthis\\.at/.*/.*/" ]
      }
    , { url = "http://huffduffer.com/oembed"
      , schemes = [ regex "http://huffduffer\\.com/.*/.*" ]
      }
    , { url = "http://www.hulu.com/api/oembed.json"
      , schemes = [ regex "http://www\\.hulu\\.com/watch/.*" ]
      }
    , { url = "http://www.ifixit.com/Embed"
      , schemes = [ regex "http://www\\.ifixit\\.com/Guide/View/.*" ]
      }
    , { url = "http://www.ifttt.com/oembed/"
      , schemes = [ regex "http://ifttt\\.com/recipes/.*" ]
      }
    , { url = "https://player.indacolive.com/services/oembed"
      , schemes = [ regex "https://player\\.indacolive\\.com/player/jwp/clients/.*" ]
      }
    , { url = "https://infogr.am/oembed"
      , schemes = [ regex "https://infogr\\.am/.*" ]
      }
    , { url = "https://infoveave.net/services/oembed/"
      , schemes = [ regex "https://.*\\.infoveave\\.net/E/.*", regex "https://.*\\.infoveave\\.net/P/.*" ]
      }
    , { url = "https://www.injurymap.com/services/oembed"
      , schemes = [ regex "https://www\\.injurymap\\.com/exercises/.*" ]
      }
    , { url = "https://www.inoreader.com/oembed/api/"
      , schemes = [ regex "https://www\\.inoreader\\.com/oembed/" ]
      }
    , { url = "http://api.inphood.com/oembed"
      , schemes = [ regex "http://.*\\.inphood\\.com/.*" ]
      }
    , { url = "https://api.instagram.com/oembed"
      , schemes = [ regex "http://instagram\\.com/p/.*", regex "http://instagr\\.am/p/.*", regex "http://www\\.instagram\\.com/p/.*", regex "http://www\\.instagr\\.am/p/.*", regex "https://instagram\\.com/p/.*", regex "https://instagr\\.am/p/.*", regex "https://www\\.instagram\\.com/p/.*", regex "https://www\\.instagr\\.am/p/.*" ]
      }
    , { url = "https://www.isnare.com/oembed/"
      , schemes = [ regex "https://www\\.isnare\\.com/.*" ]
      }
    , { url = "https://issuu.com/oembed"
      , schemes = [ regex "https://issuu\\.com/.*/docs/.*" ]
      }
    , { url = "https://music.ivlis.kr/oembed"
      , schemes = []
      }
    , { url = "https://tv.kakao.com/oembed"
      , schemes = [ regex "https://tv\\.kakao\\.com/channel/.*/cliplink/.*", regex "https://tv\\.kakao\\.com/channel/v/.*", regex "https://tv\\.kakao\\.com/channel/.*/livelink/.*", regex "https://tv\\.kakao\\.com/channel/l/.*" ]
      }
    , { url = "http://www.kickstarter.com/services/oembed"
      , schemes = [ regex "http://www\\.kickstarter\\.com/projects/.*" ]
      }
    , { url = "https://www.kidoju.com/api/oembed"
      , schemes = [ regex "https://www\\.kidoju\\.com/en/x/.*/.*", regex "https://www\\.kidoju\\.com/fr/x/.*/.*" ]
      }
    , { url = "https://embed.kit.com/oembed"
      , schemes = [ regex "http://kit\\.com/.*/.*", regex "https://kit\\.com/.*/.*" ]
      }
    , { url = "http://www.kitchenbowl.com/oembed"
      , schemes = [ regex "http://www\\.kitchenbowl\\.com/recipe/.*" ]
      }
    , { url = "https://jdr.knacki.info/oembed"
      , schemes = [ regex "http://jdr\\.knacki\\.info/meuh/.*", regex "https://jdr\\.knacki\\.info/meuh/.*" ]
      }
    , { url = "http://learningapps.org/oembed.php"
      , schemes = [ regex "http://learningapps\\.org/.*" ]
      }
    , { url = "https://pod.univ-lille.fr/oembed"
      , schemes = [ regex "https://pod\\.univ-lille\\.fr/video/.*" ]
      }
    , { url = "https://livestream.com/oembed"
      , schemes = [ regex "https://livestream\\.com/accounts/.*/events/.*", regex "https://livestream\\.com/accounts/.*/events/.*/videos/.*", regex "https://livestream\\.com/.*/events/.*", regex "https://livestream\\.com/.*/events/.*/videos/.*", regex "https://livestream\\.com/.*/.*", regex "https://livestream\\.com/.*/.*/videos/.*" ]
      }
    , { url = "https://app.ludus.one/oembed"
      , schemes = [ regex "https://app\\.ludus\\.one/.*" ]
      }
    , { url = "http://mathembed.com/oembed"
      , schemes = [ regex "http://mathembed\\.com/latex\\?inputText=.*", regex "http://mathembed\\.com/latex\\?inputText=.*" ]
      }
    , { url = "https://my.matterport.com/api/v1/models/oembed/"
      , schemes = []
      }
    , { url = "https://me.me/oembed"
      , schemes = [ regex "https://me\\.me/i/.*" ]
      }
    , { url = "https://medienarchiv.zhdk.ch/oembed.json"
      , schemes = [ regex "https://medienarchiv\\.zhdk\\.ch/entries/.*" ]
      }
    , { url = "https://api.meetup.com/oembed"
      , schemes = [ regex "http://meetup\\.com/.*", regex "https://www\\.meetup\\.com/.*", regex "https://meetup\\.com/.*", regex "http://meetu\\.ps/.*" ]
      }
    , { url = "https://www.mixcloud.com/oembed/"
      , schemes = [ regex "http://www\\.mixcloud\\.com/.*/.*/", regex "https://www\\.mixcloud\\.com/.*/.*/" ]
      }
    , { url = "http://api.mobypicture.com/oEmbed"
      , schemes = [ regex "http://www\\.mobypicture\\.com/user/.*/view/.*", regex "http://moby\\.to/.*" ]
      }
    , { url = "https://portal.modelo.io/oembed"
      , schemes = [ regex "https://beta\\.modelo\\.io/embedded/.*" ]
      }
    , { url = "https://m-roll.morphcast.com/service/oembed"
      , schemes = [ regex "https://m-roll\\.morphcast\\.com/mroll/.*" ]
      }
    , { url = "https://musicboxmaniacs.com/embed/"
      , schemes = [ regex "https://musicboxmaniacs\\.com/explore/melody/.*" ]
      }
    , { url = "https://mybeweeg.com/services/oembed"
      , schemes = [ regex "https://mybeweeg\\.com/w/.*" ]
      }
    , { url = "https://namchey.com/api/oembed"
      , schemes = [ regex "https://namchey\\.com/embeds/.*" ]
      }
    , { url = "https://www.nanoo.tv/services/oembed"
      , schemes = [ regex "http://.*\\.nanoo\\.tv/link/.*", regex "http://nanoo\\.tv/link/.*", regex "http://.*\\.nanoo\\.pro/link/.*", regex "http://nanoo\\.pro/link/.*", regex "https://.*\\.nanoo\\.tv/link/.*", regex "https://nanoo\\.tv/link/.*", regex "https://.*\\.nanoo\\.pro/link/.*", regex "https://nanoo\\.pro/link/.*", regex "http://media\\.zhdk\\.ch/signatur/.*", regex "http://new\\.media\\.zhdk\\.ch/signatur/.*", regex "https://media\\.zhdk\\.ch/signatur/.*", regex "https://new\\.media\\.zhdk\\.ch/signatur/.*" ]
      }
    , { url = "https://api.nb.no/catalog/v1/oembed"
      , schemes = [ regex "https://www\\.nb\\.no/items/.*" ]
      }
    , { url = "https://naturalatlas.com/oembed.json"
      , schemes = [ regex "https://naturalatlas\\.com/.*", regex "https://naturalatlas\\.com/.*/.*", regex "https://naturalatlas\\.com/.*/.*/.*", regex "https://naturalatlas\\.com/.*/.*/.*/.*" ]
      }
    , { url = "http://www.nfb.ca/remote/services/oembed/"
      , schemes = [ regex "http://.*\\.nfb\\.ca/film/.*" ]
      }
    , { url = "https://www.odds.com.au/api/oembed/"
      , schemes = [ regex "https://www\\.odds\\.com\\.au/.*", regex "https://odds\\.com\\.au/.*" ]
      }
    , { url = "http://official.fm/services/oembed.json"
      , schemes = [ regex "http://official\\.fm/tracks/.*", regex "http://official\\.fm/playlists/.*" ]
      }
    , { url = "https://omniscope.me/_global_/oembed/json"
      , schemes = [ regex "https://omniscope\\.me/.*" ]
      }
    , { url = "http://on.aol.com/api"
      , schemes = [ regex "http://on\\.aol\\.com/video/.*" ]
      }
    , { url = "https://www.ora.tv/oembed/*?format=json"
      , schemes = []
      }
    , { url = "http://orbitvu.co/service/oembed"
      , schemes = [ regex "https://orbitvu\\.co/001/.*/ov3601/view", regex "https://orbitvu\\.co/001/.*/ov3601/.*/view", regex "https://orbitvu\\.co/001/.*/ov3602/.*/view", regex "https://orbitvu\\.co/001/.*/2/orbittour/.*/view", regex "https://orbitvu\\.co/001/.*/1/2/orbittour/.*/view", regex "http://orbitvu\\.co/001/.*/ov3601/view", regex "http://orbitvu\\.co/001/.*/ov3601/.*/view", regex "http://orbitvu\\.co/001/.*/ov3602/.*/view", regex "http://orbitvu\\.co/001/.*/2/orbittour/.*/view", regex "http://orbitvu\\.co/001/.*/1/2/orbittour/.*/view" ]
      }
    , { url = "https://www.oumy.com/oembed"
      , schemes = [ regex "https://www\\.oumy\\.com/v/.*" ]
      }
    , { url = "https://outplayed.tv/oembed"
      , schemes = [ regex "https://outplayed\\.tv/media/.*" ]
      }
    , { url = "https://overflow.io/services/oembed"
      , schemes = [ regex "https://overflow\\.io/s/.*", regex "https://overflow\\.io/embed/.*" ]
      }
    , { url = "https://www.pastery.net/oembed"
      , schemes = [ regex "http://pastery\\.net/.*", regex "https://pastery\\.net/.*", regex "http://www\\.pastery\\.net/.*", regex "https://www\\.pastery\\.net/.*" ]
      }
    , { url = "https://beta.pingvp.com.kpnis.nl/p/oembed.php"
      , schemes = []
      }
    , { url = "https://store.pixdor.com/oembed"
      , schemes = [ regex "https://store\\.pixdor\\.com/place-marker-widget/.*/show", regex "https://store\\.pixdor\\.com/map/.*/show" ]
      }
    , { url = "https://api.podbean.com/v1/oembed"
      , schemes = [ regex "https://.*\\.podbean\\.com/e/.*", regex "http://.*\\.podbean\\.com/e/.*" ]
      }
    , { url = "http://polldaddy.com/oembed/"
      , schemes = [ regex "http://.*\\.polldaddy\\.com/s/.*", regex "http://.*\\.polldaddy\\.com/poll/.*", regex "http://.*\\.polldaddy\\.com/ratings/.*" ]
      }
    , { url = "https://api.sellwithport.com/v1.0/buyer/oembed"
      , schemes = [ regex "https://app\\.sellwithport\\.com/#/buyer/.*" ]
      }
    , { url = "https://api.portfolium.com/oembed"
      , schemes = [ regex "https://portfolium\\.com/entry/.*" ]
      }
    , { url = "http://posixion.com/services/oembed/"
      , schemes = [ regex "https://posixion\\.com/question/.*", regex "https://posixion\\.com/.*/question/.*" ]
      }
    , { url = "http://www.quiz.biz/api/oembed"
      , schemes = [ regex "http://www\\.quiz\\.biz/quizz-.*\\.html" ]
      }
    , { url = "http://www.quizz.biz/api/oembed"
      , schemes = [ regex "http://www\\.quizz\\.biz/quizz-.*\\.html" ]
      }
    , { url = "https://rapidengage.com/api/oembed"
      , schemes = [ regex "https://rapidengage\\.com/s/.*" ]
      }
    , { url = "https://www.reddit.com/oembed"
      , schemes = [ regex "https://reddit\\.com/r/.*/comments/.*/.*", regex "https://www\\.reddit\\.com/r/.*/comments/.*/.*" ]
      }
    , { url = "http://publisher.releasewire.com/oembed/"
      , schemes = [ regex "http://rwire\\.com/.*" ]
      }
    , { url = "https://repl.it/data/oembed"
      , schemes = [ regex "https://repl\\.it/@.*/.*" ]
      }
    , { url = "http://repubhub.icopyright.net/oembed.act"
      , schemes = [ regex "http://repubhub\\.icopyright\\.net/freePost\\.act\\?.*" ]
      }
    , { url = "https://www.reverbnation.com/oembed"
      , schemes = [ regex "https://www\\.reverbnation\\.com/.*", regex "https://www\\.reverbnation\\.com/.*/songs/.*" ]
      }
    , { url = "https://www.riffreporter.de/service/oembed"
      , schemes = []
      }
    , { url = "http://roomshare.jp/en/oembed.json"
      , schemes = [ regex "http://roomshare\\.jp/post/.*", regex "http://roomshare\\.jp/en/post/.*" ]
      }
    , { url = "https://roosterteeth.com/oembed"
      , schemes = [ regex "https://roosterteeth\\.com/.*" ]
      }
    , { url = "https://rumble.com/api/Media/oembed.json"
      , schemes = []
      }
    , { url = "http://videos.sapo.pt/oembed"
      , schemes = [ regex "http://videos\\.sapo\\.pt/.*" ]
      }
    , { url = "https://api.screen9.com/oembed"
      , schemes = [ regex "https://console\\.screen9\\.com/.*", regex "https://.*\\.screen9\\.tv/.*" ]
      }
    , { url = "https://api.screencast.com/external/oembed"
      , schemes = []
      }
    , { url = "http://www.screenr.com/api/oembed.json"
      , schemes = [ regex "http://www\\.screenr\\.com/.*/" ]
      }
    , { url = "https://scribblemaps.com/api/services/oembed.json"
      , schemes = [ regex "http://www\\.scribblemaps\\.com/maps/view/.*", regex "https://www\\.scribblemaps\\.com/maps/view/.*", regex "http://scribblemaps\\.com/maps/view/.*", regex "https://scribblemaps\\.com/maps/view/.*" ]
      }
    , { url = "http://www.scribd.com/services/oembed/"
      , schemes = [ regex "http://www\\.scribd\\.com/doc/.*" ]
      }
    , { url = "https://embed.sendtonews.com/services/oembed"
      , schemes = [ regex "https://embed\\.sendtonews\\.com/oembed/.*" ]
      }
    , { url = "https://www.shortnote.jp/oembed/"
      , schemes = [ regex "https://www\\.shortnote\\.jp/view/notes/.*" ]
      }
    , { url = "http://shoudio.com/api/oembed"
      , schemes = [ regex "http://shoudio\\.com/.*", regex "http://shoud\\.io/.*" ]
      }
    , { url = "https://showtheway.io/oembed"
      , schemes = [ regex "https://showtheway\\.io/to/.*" ]
      }
    , { url = "https://simplecast.com/oembed"
      , schemes = [ regex "https://simplecast\\.com/s/.*" ]
      }
    , { url = "https://onsizzle.com/oembed"
      , schemes = [ regex "https://onsizzle\\.com/i/.*" ]
      }
    , { url = "http://sketchfab.com/oembed"
      , schemes = [ regex "http://sketchfab\\.com/models/.*", regex "https://sketchfab\\.com/models/.*", regex "https://sketchfab\\.com/.*/folders/.*" ]
      }
    , { url = "http://www.slideshare.net/api/oembed/2"
      , schemes = [ regex "http://www\\.slideshare\\.net/.*/.*", regex "http://fr\\.slideshare\\.net/.*/.*", regex "http://de\\.slideshare\\.net/.*/.*", regex "http://es\\.slideshare\\.net/.*/.*", regex "http://pt\\.slideshare\\.net/.*/.*" ]
      }
    , { url = "https://smashnotes.com/services/oembed"
      , schemes = [ regex "https://smashnotes\\.com/p/.*", regex "https://smashnotes\\.com/p/.*/e/.* - https://smashnotes\\.com/p/.*/e/.*/s/.*" ]
      }
    , { url = "http://api.smugmug.com/services/oembed/"
      , schemes = [ regex "http://.*\\.smugmug\\.com/.*" ]
      }
    , { url = "https://www.socialexplorer.com/services/oembed/"
      , schemes = [ regex "https://www\\.socialexplorer\\.com/.*/explore", regex "https://www\\.socialexplorer\\.com/.*/view", regex "https://www\\.socialexplorer\\.com/.*/edit", regex "https://www\\.socialexplorer\\.com/.*/embed" ]
      }
    , { url = "https://song.link/oembed"
      , schemes = [ regex "https://song\\.link/.*" ]
      }
    , { url = "https://soundcloud.com/oembed"
      , schemes = [ regex "http://soundcloud\\.com/.*", regex "https://soundcloud\\.com/.*" ]
      }
    , { url = "https://play.soundsgood.co/oembed"
      , schemes = [ regex "https://play\\.soundsgood\\.co/playlist/.*", regex "https://soundsgood\\.co/playlist/.*" ]
      }
    , { url = "https://speakerdeck.com/oembed.json"
      , schemes = [ regex "http://speakerdeck\\.com/.*/.*", regex "https://speakerdeck\\.com/.*/.*" ]
      }
    , { url = "https://api.bespotful.com/oembed"
      , schemes = [ regex "http://play\\.bespotful\\.com/.*" ]
      }
    , { url = "https://embed.spotify.com/oembed/"
      , schemes = [ regex "https://.*\\.spotify\\.com/.*", regex "spotify:.*" ]
      }
    , { url = "https://api.spreaker.com/oembed"
      , schemes = [ regex "http://.*\\.spreaker\\.com/.*", regex "https://.*\\.spreaker\\.com/.*" ]
      }
    , { url = "https://purl.stanford.edu/embed.json"
      , schemes = [ regex "https://purl\\.stanford\\.edu/.*" ]
      }
    , { url = "https://api.streamable.com/oembed.json"
      , schemes = [ regex "http://streamable\\.com/.*", regex "https://streamable\\.com/.*" ]
      }
    , { url = "https://content.streamonecloud.net/oembed"
      , schemes = [ regex "https://content\\.streamonecloud\\.net/embed/.*" ]
      }
    , { url = "https://www.sutori.com/api/oembed"
      , schemes = [ regex "https://www\\.sutori\\.com/story/.*" ]
      }
    , { url = "https://sway.com/api/v1.0/oembed"
      , schemes = [ regex "https://sway\\.com/.*", regex "https://www\\.sway\\.com/.*" ]
      }
    , { url = "https://www.ted.com/talks/oembed.json"
      , schemes = [ regex "http://ted\\.com/talks/.*", regex "https://ted\\.com/talks/.*", regex "https://www\\.ted\\.com/talks/.*" ]
      }
    , { url = "https://www.nytimes.com/svc/oembed/json/"
      , schemes = [ regex "https://www\\.nytimes\\.com/svc/oembed", regex "https://nytimes\\.com/.*", regex "https://.*\\.nytimes\\.com/.*" ]
      }
    , { url = "https://theysaidso.com/extensions/oembed/"
      , schemes = [ regex "https://theysaidso\\.com/image/.*" ]
      }
    , { url = "https://www.tickcounter.com/oembed"
      , schemes = [ regex "http://www\\.tickcounter\\.com/countdown/.*", regex "http://www\\.tickcounter\\.com/countup/.*", regex "http://www\\.tickcounter\\.com/ticker/.*", regex "http://www\\.tickcounter\\.com/worldclock/.*", regex "https://www\\.tickcounter\\.com/countdown/.*", regex "https://www\\.tickcounter\\.com/countup/.*", regex "https://www\\.tickcounter\\.com/ticker/.*", regex "https://www\\.tickcounter\\.com/worldclock/.*" ]
      }
    , { url = "https://widget.toornament.com/oembed"
      , schemes = [ regex "https://www\\.toornament\\.com/tournaments/.*/information", regex "https://www\\.toornament\\.com/tournaments/.*/registration/", regex "https://www\\.toornament\\.com/tournaments/.*/matches/schedule", regex "https://www\\.toornament\\.com/tournaments/.*/stages/.*/" ]
      }
    , { url = "http://www.topy.se/oembed/"
      , schemes = [ regex "http://www\\.topy\\.se/image/.*" ]
      }
    , { url = "https://www.tuxx.be/services/oembed"
      , schemes = [ regex "https://www\\.tuxx\\.be/.*" ]
      }
    , { url = "http://www.tvcf.co.kr/services/oembed"
      , schemes = [ regex "http://www\\.tvcf\\.co\\.kr/v/.*" ]
      }
    , { url = "https://api.twitch.tv/v4/oembed"
      , schemes = [ regex "http://clips\\.twitch\\.tv/.*", regex "https://clips\\.twitch\\.tv/.*", regex "http://www\\.twitch\\.tv/.*", regex "https://www\\.twitch\\.tv/.*", regex "http://twitch\\.tv/.*", regex "https://twitch\\.tv/.*" ]
      }
    , { url = "https://publish.twitter.com/oembed"
      , schemes = [ regex "https://twitter\\.com/.*/status/.*", regex "https://.*\\.twitter\\.com/.*/status/.*" ]
      }
    , { url = "https://play.typecast.ai/oembed"
      , schemes = [ regex "https://play\\.typecast\\.ai/s/.*", regex "https://play\\.typecast\\.ai/e/.*", regex "https://play\\.typecast\\.ai/.*" ]
      }
    , { url = "https://player.ubideo.com/api/oembed.json"
      , schemes = [ regex "https://player\\.ubideo\\.com/.*" ]
      }
    , { url = "https://map.cam.ac.uk/oembed/"
      , schemes = [ regex "https://map\\.cam\\.ac\\.uk/.*" ]
      }
    , { url = "https://mais.uol.com.br/apiuol/v3/oembed/view"
      , schemes = [ regex "https://.*\\.uol\\.com\\.br/view/.*", regex "https://.*\\.uol\\.com\\.br/video/.*" ]
      }
    , { url = "http://www.ustream.tv/oembed"
      , schemes = [ regex "http://.*\\.ustream\\.tv/.*", regex "http://.*\\.ustream\\.com/.*" ]
      }
    , { url = "https://www.utposts.com/api/oembed"
      , schemes = [ regex "https://www\\.utposts\\.com/products/.*", regex "http://www\\.utposts\\.com/products/.*", regex "https://utposts\\.com/products/.*", regex "http://utposts\\.com/products/.*" ]
      }
    , { url = "http://uttles.com/api/reply/oembed"
      , schemes = [ regex "http://uttles\\.com/uttle/.*" ]
      }
    , { url = "https://api.veer.tv/oembed"
      , schemes = [ regex "http://veer\\.tv/videos/.*" ]
      }
    , { url = "https://api.veervr.tv/oembed"
      , schemes = [ regex "http://veervr\\.tv/videos/.*" ]
      }
    , { url = "http://verse.com/services/oembed/"
      , schemes = []
      }
    , { url = "https://www.vevo.com/oembed"
      , schemes = [ regex "http://www\\.vevo\\.com/.*", regex "https://www\\.vevo\\.com/.*" ]
      }
    , { url = "http://www.videojug.com/oembed.json"
      , schemes = [ regex "http://www\\.videojug\\.com/film/.*", regex "http://www\\.videojug\\.com/interview/.*" ]
      }
    , { url = "https://api.vidl.it/oembed"
      , schemes = [ regex "https://vidl\\.it/.*" ]
      }
    , { url = "https://app-v2.vidmizer.com/api/oembed"
      , schemes = [ regex "https://players\\.vidmizer\\.com/.*" ]
      }
    , { url = "https://api.vidyard.com/dashboard/v1.1/oembed"
      , schemes = [ regex "http://embed\\.vidyard\\.com/.*", regex "http://play\\.vidyard\\.com/.*", regex "http://share\\.vidyard\\.com/.*", regex "http://.*\\.hubs\\.vidyard\\.com/.*" ]
      }
    , { url = "https://vimeo.com/api/oembed.json"
      , schemes = [ regex "https://vimeo\\.com/.*", regex "https://vimeo\\.com/album/.*/video/.*", regex "https://vimeo\\.com/channels/.*/.*", regex "https://vimeo\\.com/groups/.*/videos/.*", regex "https://vimeo\\.com/ondemand/.*/.*", regex "https://player\\.vimeo\\.com/video/.*" ]
      }
    , { url = "http://viziosphere.com/services/oembed/"
      , schemes = [ regex "http://viziosphere\\.com/3dphoto.*" ]
      }
    , { url = "https://vlipsy.com/oembed"
      , schemes = [ regex "https://vlipsy\\.com/.*" ]
      }
    , { url = "https://www.vlive.tv/oembed"
      , schemes = [ regex "https://www\\.vlive\\.tv/video/.*" ]
      }
    , { url = "https://vlurb.co/oembed.json"
      , schemes = [ regex "http://vlurb\\.co/video/.*", regex "https://vlurb\\.co/video/.*" ]
      }
    , { url = "https://data.voxsnap.com/oembed"
      , schemes = [ regex "https://article\\.voxsnap\\.com/.*/.*" ]
      }
    , { url = "http://play.wecandeo.com/oembed"
      , schemes = []
      }
    , { url = "http://*.wiredrive.com/present-oembed/"
      , schemes = [ regex "https://.*\\.wiredrive\\.com/.*" ]
      }
    , { url = "https://fast.wistia.com/oembed.json"
      , schemes = [ regex "https://fast\\.wistia\\.com/embed/iframe/.*", regex "https://fast\\.wistia\\.com/embed/playlists/.*", regex "https://.*\\.wistia\\.com/medias/.*" ]
      }
    , { url = "http://app.wizer.me/api/oembed.json"
      , schemes = [ regex "http://.*\\.wizer\\.me/learn/.*", regex "https://.*\\.wizer\\.me/learn/.*", regex "http://.*\\.wizer\\.me/preview/.*", regex "https://.*\\.wizer\\.me/preview/.*" ]
      }
    , { url = "http://www.wootled.com/oembed"
      , schemes = []
      }
    , { url = "http://public-api.wordpress.com/oembed/"
      , schemes = []
      }
    , { url = "http://yesik.it/s/oembed"
      , schemes = [ regex "http://yesik\\.it/.*", regex "http://www\\.yesik\\.it/.*" ]
      }
    , { url = "http://www.yfrog.com/api/oembed"
      , schemes = [ regex "http://.*\\.yfrog\\.com/.*", regex "http://yfrog\\.us/.*" ]
      }
    , { url = "https://www.youtube.com/oembed"
      , schemes = [ regex "https://.*\\.youtube\\.com/watch.*", regex "https://.*\\.youtube\\.com/v/.*", regex "https://youtu\\.be/.*" ]
      }
    , { url = "https://api.znipe.tv/v3/oembed/"
      , schemes = [ regex "https://.*\\.znipe\\.tv/.*" ]
      }
    , { url = "http://api.provider.com/oembed.json"
      , schemes = [ regex "https://reports\\.zoho\\.com/ZDBDataSheetView\\.cc\\?OBJID=1432535000000003002&STANDALONE=true&INTERVAL=120&DATATYPESYMBOL=false&REMTOOLBAR=false&SEARCHBOX=true&INCLUDETITLE=true&INCLUDEDESC=true&SHOWHIDEOPT=true" ]
      }
    ]
