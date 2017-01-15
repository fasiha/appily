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