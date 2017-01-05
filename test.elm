import Json.Decode exposing (int, string, float, list, bool, decodeString, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Html exposing (..)
import Html.Attributes exposing (value, type_, placeholder)
import Html.Events exposing (..)
import Http
import Regex exposing (..)
import Dict exposing (Dict)

type FuriganaSource = OriginalFurigana | AutoFurigana | HumanEditorFurigana

type JString
  = Plain String
  | Furigana String String FuriganaSource

type alias LangId = String

type alias JmdictEntry = {id: Int, printable: JString, kanjiId: Maybe Int, kanaId: Maybe Int}

type VocabSource = Jmdict JmdictEntry | MakinoTsutsui String
-- MakinoTsutsui -> Seiichi Makino & Michio Tsutsui’s series of dictionaries
-- starting with *A Dictionary of Basic Japanese Grammar*.

type SentenceSource = TonoYamazakiMaekawa | KTS | UnknownSource String
-- TonoYamazakiMaekawa -> Tono, Yamazaki, and Maekawa’s *A Frequency Dictionary
-- of Japanese*

type alias Sentence =
  { source: SentenceSource
  , contents: List JString
  , vocabs: List (List Int, VocabSource)
  , translations: Dict LangId (List String)
  }

k : JString
k = Furigana "😘" "kissy" OriginalFurigana

renderJString : JString -> Html msg
renderJString frag
  = case frag of
    Plain s -> text s
    Furigana a b _ -> ruby [] [text a, rt [] [text b]]

-- KANJI AND KANA


-- Via xregexp
hanRegex : Regex
hanRegex = regex "[⺀-⺙⺛-⻳⼀-⿕々〇〡-〩〸-〻㐀-䶵一-鿕豈-舘並-龎]"

renderMorpheme : Morpheme -> Html Msg
renderMorpheme morpheme =
  if contains hanRegex morpheme.literal then
    renderJString (Furigana morpheme.literal morpheme.literalPronunciation AutoFurigana)

  else
    text morpheme.literal

-- KUROMOJI

type alias Morpheme =
  { literal: String
  , literalPronunciation: String
  , writtenForm: String
  , writtenBaseForm: String
  , lemma: String
  , lemmaReading: String
  , lemmaPronunciation: String
  , partOfSpeech : List String
  , conjugation: List String
  , conjugationType: List String
  , position: Int
  , languageType : String
  -- , initialSoundAlternationType: String
  -- , initialSoundAlternationForm: String
  -- , finalSoundAlternationType: String
  -- , finalSoundFlternationForm: String
  }

morphemeDecoder : Decoder Morpheme
morphemeDecoder =
  decode Morpheme
    |> required "literal" string
    |> required "literal-pronunciation" string
    |> required "written-form" string
    |> required "written-base-form" string
    |> required "lemma" string
    |> required "lemma-reading" string
    |> required "lemma-pronunciation" string
    |> required "part-of-speech" (list string)
    |> required "conjugation" (list string)
    |> required "conjugation-type" (list string)
    |> required "position" int
    |> required "language-type" string
    -- |> required "initial-sound-alternation-type" string
    -- |> required "initial-sound-alternation-form" string
    -- |> required "final-sound-alternation-type" string
    -- |> required "final-sound-alternation-form" string

type alias Morphemes = List Morpheme

morphemesDecoder : Decoder Morphemes
morphemesDecoder = list morphemeDecoder

-- MODEL

type alias Model =
  { raw : String
  , morphemes : Morphemes
  }

init : (Model, Cmd Msg)
init =
  ( Model "" []
  , Cmd.none
  )

-- UPDATE

type Msg
  = Raw String
  | Submit
  | Parse (Result Http.Error Morphemes)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Raw raw -> ({ model | raw = raw }, Cmd.none)
    Submit -> (model, sendToParse model.raw)
    Parse (Ok morphemes) ->({ model | morphemes = morphemes}, Cmd.none)
    Parse (Err err) -> ({ model | raw = "err" }, Cmd.none)


sendToParse : String -> Cmd Msg
sendToParse s =
  let
    url = "http://localhost:3600/parse/" ++ s
  in
    Http.send Parse (Http.get url morphemesDecoder)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MAIN

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- VIEW

testNode : Morphemes -> Html Msg
testNode morphemes =
  div []
    [ div [] [text (toString k)]
    , div [] [renderJString k]
    , div [] [renderJString (Plain "Juice?")]
    -- , div [] [text (toString allResult)]
    -- , ol [] (List.map
    --           (\o -> li [] [text (toString o)])
    --           (Result.withDefault [] allResult))
    , ol [] (List.map
              (\o -> li [] [text (toString o)])
              morphemes)
    , ol [] (List.map
          (\o -> li [] [renderMorpheme o])
          morphemes)
    ] -- renderMorpheme

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.raw, type_ "text", placeholder "Text", onInput Raw ] []
    , button [ onClick Submit ] [ text "Submit" ]
    , testNode model.morphemes
    ]

-- TESTING

kuro : String
kuro = """[{"language-type":"和","part-of-speech":["pronoun"],"final-sound-alternation-type":"*","known?":true,"lemma-pronunciation":"ナン","written-base-form":"何","lemma-reading":"ナニ","written-form":"何","lemma":"何","initial-sound-alternation-form":"*","final-sound-alternation-form":"*","user?":false,"conjugation-type":[],"conjugation":["uninflected"],"literal":"何","initial-sound-alternation-type":"*","all-features":["代名詞","*","*","*","*","*","ナニ","何","何","ナン","何","ナン","和","*","*","*","*"],"position":0,"literal-pronunciation":"ナン"},{"language-type":"和","part-of-speech":["verb","bound"],"final-sound-alternation-type":"*","known?":true,"lemma-pronunciation":"デキル","written-base-form":"できる","lemma-reading":"デキル","written-form":"でき","lemma":"出来る","initial-sound-alternation-form":"*","final-sound-alternation-form":"*","user?":false,"conjugation-type":["kamiichidan-verb-i-row","ka-column"],"conjugation":["continuative","general"],"literal":"でき","initial-sound-alternation-type":"*","all-features":["動詞","非自立可能","*","*","上一段-カ行","連用形-一般","デキル","出来る","でき","デキ","できる","デキル","和","*","*","*","*"],"position":1,"literal-pronunciation":"デキ"},{"language-type":"和","part-of-speech":["auxiliary-verb"],"final-sound-alternation-type":"*","known?":true,"lemma-pronunciation":"タ","written-base-form":"た","lemma-reading":"タ","written-form":"た","lemma":"た","initial-sound-alternation-form":"*","final-sound-alternation-form":"*","user?":false,"conjugation-type":["auxiliary","ta"],"conjugation":["conclusive","general"],"literal":"た","initial-sound-alternation-type":"*","all-features":["助動詞","*","*","*","助動詞-タ","終止形-一般","タ","た","た","タ","た","タ","和","*","*","*","*"],"position":3,"literal-pronunciation":"タ"},{"language-type":"記号","part-of-speech":["supplementary-symbol","period"],"final-sound-alternation-type":"*","known?":true,"lemma-pronunciation":"","written-base-form":"？","lemma-reading":"","written-form":"？","lemma":"？","initial-sound-alternation-form":"*","final-sound-alternation-form":"*","user?":false,"conjugation-type":[],"conjugation":["uninflected"],"literal":"？","initial-sound-alternation-type":"*","all-features":["補助記号","句点","*","*","*","*","","？","？","","？","","記号","*","*","*","*"],"position":4,"literal-pronunciation":""}]"""

allResult : Result String Morphemes
allResult = decodeString (list morphemeDecoder) kuro
