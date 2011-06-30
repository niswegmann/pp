{-|
License     :  CC0 1.0 Universal Public Domain Dedication
Maintainer  :  niswegmann@gmail.com
Stability   :  provisional
Portability :  portable (Haskell 2010)

An optimized pretty-printer that doesn't care about pagewidths.
-}

module Text.PP
  ( Doc (..)
  , empty
  , string
  , ($$)
  , (<>)
  , (<+>)
  , indent
  , concatV
  , concatH
  , render
  ) where

import Data.DText (DText)
import qualified Data.DText as DText
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------

-- | A document.
data Doc
  = Empty
  | Text Text
  | Indent Int Doc
  | Doc `Above`  Doc
  | Doc `Beside` Doc

--------------------------------------------------------------------------------

-- | Creates an empty document.
empty :: Doc
empty = Empty

-- | Creates a document from a string.
string :: String -> Doc
string = Text . Text.pack

{-| Horizontal composition of two documents.

E.g.

> doc1 <> doc2

...is rendered as:

> +--------+
> |        |
> |  doc1  |
> |        |
> |  +-----+
> |  |
> +--+
> +--------+
> |        |
> |  doc2  |
> |        |
> |  +-----+
> |  |
> +--+
-}
($$) :: Doc -> Doc -> Doc
($$) = Above

{-| Vertical composition of two documents.

E.g.

> doc1 <> doc2

... is rendered as:

> +--------+
> |        |
> |  doc1  |
> |        |
> |  +-----+
> |  |+--------+
> +--+|        |
>     |  doc2  |
>     |        |
>     |  +-----+
>     |  |
>     +--+
-}
(<>) :: Doc -> Doc -> Doc
(<>) = Beside

-- | Vertical composition of two documents, width a space in beetween.
(<+>) :: Doc -> Doc -> Doc
doc1 <+> doc2 = doc1 <> string " " <> doc2

{-| Indents a document.

E.g.

> doc1 $$ indent n doc2

...is rendered as:

> +--------+
> |        |
> |  doc1  |
> |        |
> |  +-----+
> |  |
> +--+
>          +--------+
>          |        |
>          |  doc2  |
> <---n--->|        |
>          |  +-----+
>          |  |
>          +--+
-}
indent :: Int -> Doc -> Doc
indent = Indent

-- | Vertical concatenation.
--
-- A short-hand for
--
-- > foldr (<>) empty
concatV :: [Doc] -> Doc
concatV = List.foldr (<>) empty

-- | Horizontal concatenation.
--
-- A short-hand for
--
-- > foldr ($$) empty
concatH :: [Doc] -> Doc
concatH = List.foldr ($$) empty

-- | Renders a document.
render :: Doc -> Text
render = textBoxToText . render_tbx

--------------------------------------------------------------------------------

type TextBox =
  ( [DText] -- body
  , DText   -- last line
  , Int     -- length of last line
  )

indentDText :: Int -> DText -> DText
indentDText = DText.append . flip DText.replicate ' '

empty_tbx :: TextBox
empty_tbx =
  ( []
  , DText.empty
  , 0
  )

text_tbx :: Text -> TextBox
text_tbx cs =
  ( []
  , DText.fromText cs
  , Text.length cs
  )

indent_tbx :: Int -> TextBox -> TextBox
indent_tbx k (b, l, z) =
  ( map (indentDText k) b
  , indentDText k l
  , k + z
  )

above_tbx :: TextBox -> TextBox -> TextBox
above_tbx (b1, l1, _) (b2, l2, z2) =
  ( b1 ++ l1:b2
  , l2
  , z2
  )

beside_tbx :: TextBox -> TextBox -> TextBox
beside_tbx (b1, l1, z1) ([], l2, z2) =
  ( b1
  , DText.append l1 l2
  , z1 + z2
  )
beside_tbx (b1, l1, z1) (b2 : b2s, l2, z2) =
  ( b1 ++ (DText.append l1 b2 : map (indentDText z1) b2s)
  , indentDText z1 l2
  , z1 + z2
  )

render_tbx :: Doc -> TextBox
render_tbx doc0 =
  case doc0 of
    Empty              -> empty_tbx
    Text cs            -> text_tbx cs
    Indent k doc       -> indent_tbx k (render_tbx doc)
    doc1 `Above`  doc2 -> render_tbx doc1 `above_tbx`  render_tbx doc2
    doc1 `Beside` doc2 -> render_tbx doc1 `beside_tbx` render_tbx doc2

textBoxToText :: TextBox -> Text
textBoxToText (b, l, _) = Text.unlines $ map DText.toText (b ++ [l])
