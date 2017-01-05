module Main exposing (..)

import Json.Decode exposing (int, string, float, list, bool, decodeString, oneOf, andThen, map2, field, succeed, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Html exposing (..)
import Html.Attributes exposing (value, type_, placeholder)
import Html.Events exposing (..)
import Http
import Regex exposing (..)
import Dict exposing (Dict)


type FuriganaSource
    = OriginalFurigana
    | AutoFurigana
    | HumanEditorFurigana


type JString
    = Plain String
    | Furigana String String FuriganaSource


type alias LangId =
    String


type alias JmdictEntry =
    { id : Int, printable : JString, kanjiId : Maybe Int, kanaId : Maybe Int }


type VocabSource
    = Jmdict JmdictEntry
    | MakinoTsutsui String


{-| MakinoTsutsui -> Seiichi Makino & Michio Tsutsui’s series of dictionaries starting with *A Dictionary of Basic Japanese Grammar*.
-}
type SentenceSource
    = TonoYamazakiMaekawa
    | KTS
    | UnknownSource String


{-| TonoYamazakiMaekawa -> Tono, Yamazaki, and Maekawa’s *A Frequency Dictionary of Japanese*.
-}
type alias Sentence =
    { source : SentenceSource
    , contents : List JString
    , vocabs : List ( List Int, VocabSource )
    , translations : Dict LangId (List String)
    }


renderJString : JString -> Html msg
renderJString frag =
    case frag of
        Plain s ->
            text s

        Furigana a b _ ->
            ruby [] [ text a, rt [] [ text b ] ]



-- KANJI AND KANA


{-| Via xregexp
-}
hanRegex : Regex
hanRegex =
    regex "[⺀-⺙⺛-⻳⼀-⿕々〇〡-〩〸-〻㐀-䶵一-\x9FD5豈-舘並-龎]"


renderMorpheme : Morpheme -> Html Msg
renderMorpheme morpheme =
    if contains hanRegex morpheme.literal then
        renderJString (Furigana morpheme.literal morpheme.literalPronunciation AutoFurigana)
    else
        text morpheme.literal



-- KUROMOJI


type alias Ruby =
    { ruby : String, rt : String }


rubyDecoder : Decoder Ruby
rubyDecoder =
    map2 Ruby
        (field "ruby" string)
        (field "rt" string)


furigana : Decoder JString
furigana =
    map2 Ruby
        (field "ruby" string)
        (field "rt" string)
        |> andThen ruby2furi


ruby2furi : Ruby -> Decoder JString
ruby2furi r =
    succeed (Furigana r.ruby r.rt AutoFurigana)


furiganasDecoder : Decoder (List JString)
furiganasDecoder =
    (list (oneOf [ furigana, plain ]))


plain : Decoder JString
plain =
    Json.Decode.map Plain string


type alias Morpheme =
    { literal : String
    , literalPronunciation : String
    , writtenForm : String
    , writtenBaseForm : String
    , lemma : String
    , lemmaReading : String
    , lemmaPronunciation : String
    , partOfSpeech : List String
    , conjugation : List String
    , conjugationType : List String
    , position : Int
    , languageType : String
    , furigana :
        Maybe (List JString)
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
        |> required "furigana" (nullable furiganasDecoder)


type alias Morphemes =
    List Morpheme


morphemesDecoder : Decoder Morphemes
morphemesDecoder =
    list morphemeDecoder



-- MODEL


type alias Model =
    { raw : String
    , morphemes : Morphemes
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Raw String
    | Submit
    | Parse (Result Http.Error Morphemes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Raw raw ->
            ( { model | raw = raw }, Cmd.none )

        Submit ->
            ( model, sendToParse model.raw )

        Parse (Ok morphemes) ->
            ( { model | morphemes = morphemes }, Cmd.none )

        Parse (Err err) ->
            ( { model | raw = (toString err) }, Cmd.none )


sendToParse : String -> Cmd Msg
sendToParse s =
    let
        url =
            "http://localhost:3600/parse-furigana/" ++ s
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
        [ div [] [ text (toString k) ]
        , div [] [ renderJString k ]
        , div [] [ renderJString (Plain "Juice?") ]
        , ol []
            (List.map
                (\o -> li [] [ text (toString o) ])
                morphemes
            )
        , ol []
            (List.map
                (\o -> li [] [ renderMorpheme o ])
                morphemes
            )
        ]



-- renderMorpheme


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.raw, type_ "text", placeholder "Text", onInput Raw ] []
        , button [ onClick Submit ] [ text "Submit" ]
        , testNode model.morphemes
        ]



-- TESTING


k : JString
k =
    Furigana "😘" "kissy" OriginalFurigana
