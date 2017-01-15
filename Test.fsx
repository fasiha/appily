type FuriganaSource = OriginalFurigana | AutoFurigana | HumanEditorFurigana

type JString = Plain of string | Furigana of string * string * FuriganaSource

type LangId = Lang of string

type JmdictEntry = { Id : int; KanjiId : int option; KanaId : int option; SenseId : int option }

type Vocab = Jmdict of JmdictEntry | MakinoTsutsui of string

type SentenceSource = TonoYamazakiMaekawa | KTS | OtherSource of string

type VocabUsed = { Pos : int list; Vocab : Vocab }

type Sentence =
    { Source : SentenceSource
    ; Contents : JString list
    ; Vocabs : VocabUsed list
    ; Translations : string list
    }

let s = [Plain "Whee"; Furigana("😂", "rolf", OriginalFurigana); Plain "!"]
printfn "%A" s
printfn "%A" s.[0]

#r "./FSharp.Data.2.3.2/lib/net40/FSharp.Data.dll"
open FSharp.Data
type Simple = JsonProvider<""" {  "words" : [ {
    "id" : 1000000, 
    "kanji" : [  ], 
    "kana" : [ {
      "common" : false, 
      "text" : "ヽ", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    }, {
      "common" : false, 
      "text" : "くりかえし", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    } ], 
    "sense" : [ {
      "partOfSpeech" : [ "n" ], 
      "appliesToKanji" : [ "*" ], 
      "appliesToKana" : [ "*" ], 
      "related" : [  ], 
      "antonym" : [  ], 
      "field" : [  ], 
      "dialect" : [  ], 
      "misc" : [  ], 
      "info" : [  ], 
      "languageSource" : [  ], 
      "gloss" : [ {
        "lang" : "eng", 
        "text" : "repetition mark in katakana"
      } ]
    } ]
  }, {
    "id" : 1000010, 
    "kanji" : [  ], 
    "kana" : [ {
      "common" : false, 
      "text" : "ヾ", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    }, {
      "common" : false, 
      "text" : "くりかえし", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    } ], 
    "sense" : [ {
      "partOfSpeech" : [ "?" ], 
      "appliesToKanji" : [ "*" ], 
      "appliesToKana" : [ "*" ], 
      "related" : [  ], 
      "antonym" : [  ], 
      "field" : [  ], 
      "dialect" : [  ], 
      "misc" : [  ], 
      "info" : [  ], 
      "languageSource" : [  ], 
      "gloss" : [ {
        "lang" : "eng", 
        "text" : "voiced repetition mark in katakana"
      } ]
    } ]
  }, {
    "id" : 1000020, 
    "kanji" : [  ], 
    "kana" : [ {
      "common" : false, 
      "text" : "ゝ", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    }, {
      "common" : false, 
      "text" : "くりかえし", 
      "tags" : [  ], 
      "appliesToKanji" : [ "*" ]
    } ], 
    "sense" : [ {
      "partOfSpeech" : [ "?" ], 
      "appliesToKanji" : [ "*" ], 
      "appliesToKana" : [ "*" ], 
      "related" : [  ], 
      "antonym" : [  ], 
      "field" : [  ], 
      "dialect" : [  ], 
      "misc" : [  ], 
      "info" : [  ], 
      "languageSource" : [  ], 
      "gloss" : [ {
        "lang" : "eng", 
        "text" : "repetition mark in hiragana"
      } ]
    } ]
  }]
} """>
// let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)
let simple = Simple.Load("./data/jmdict_eng.json")
let w = simple.Words
// printfn "%A" simple.Words
printfn "%A" w.[123]
