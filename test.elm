import Html exposing (Html, ol, li, text, div, ruby, rt)
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

main : Html msg
main =
  div []
    [ div [] [text (toString k)]
    , div [] [renderJString k]
    , div [] [renderJString (Plain "Juice?")]
    ]
