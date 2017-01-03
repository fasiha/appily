import Html exposing (Html, ol, li, text, div, ruby, rt)
import String
import Dict exposing (Dict)

type alias KanaString = String

type alias KanjiString = String

type JString
  = Plain String
  | Furigana KanjiString KanaString

type alias LangId = String

type alias Vocab =
  { id: Int
  , strings: List JString
  , senses: Dict LangId (List String)
  }

type alias S2VLink =
  { indexes: List Int
  , vocab: Vocab
  , stringid: Maybe Int
  , senseid: Maybe (LangId, Int)
  }

type SentenceSource = TonoYamazakiMaekawa | KTS | UnknownSource

type alias Sentence =
  { raw: String
  , contents: List JString
  , translations: Dict LangId (List String)
  , vocabs: List S2VLink
  , source: SentenceSource
  }

kana : JString
kana = Furigana "😘" "kissy"

renderFragment : JString -> Html msg
renderFragment frag
  = case frag of
    Plain s -> text s
    Furigana a b -> ruby [] [text a, rt [] [text b]]

main =
  div []
    [ div [] [text (toString kana)]
    , div [] [renderFragment kana]
    , div [] [renderFragment (Plain "Juice?")]
    ]
