{-|
License     :  CC0 1.0 Universal Public Domain Dedication
Maintainer  :  niswegmann@gmail.com
Stability   :  provisional
Portability :  portable (Haskell 2010)

An optimized pretty-printer that doesn't care about pagewidths.

-}

-- This module provides an implementation of optimal pretty-printing as described
-- in:
--
--   * \"/Optimal Pretty-Printing Combinators/\", Pablo R. Azero and Doaitse
--     Swierstra, ICFP '98

module Text.PP
  ( Doc (..)
  , PageWidth
  , Indent
  , empty
  , string
  , ($$)
  , (<>)
  , (<+>)
  , indent
--  , (??)
  , render
--  , renderWithPageWidth
  ) where

import Data.DText (DText)
import qualified Data.DText as DText
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------

type PageWidth = Int
type Indent    = Int

-- | A document.
data Doc
  = Empty
  | Text Text
  | Indent Indent Doc
  | Doc `Above`  Doc
  | Doc `Beside` Doc
  | Doc `Choice` Doc

--------------------------------------------------------------------------------

-- | Creates an empty document.
empty :: Doc
empty = Empty

-- | Creates a document from a string.
string :: String -> Doc
string = Text . Text.pack

{-| Horizontal composition of two documents.

E.g.

> doc1 <-> doc2

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

> doc1 <|> doc2

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

> doc1 <-> indent n doc2

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
indent :: Indent -> Doc -> Doc
indent = Indent

-- -- | Optional composition. Picks one of the two input documents.
--(??) :: Doc -> Doc -> Doc
--(??) = Choice

-- | Renders a document.
render :: Doc -> Text
render = formatToText . eval render_fmt

-- -- | Renders a document with a maximum page-width.
--renderWithPageWidth :: PageWidth -> Doc -> Maybe Text
--renderWithPageWidth = error "not implemented yet"

--------------------------------------------------------------------------------

type Render a =
  ( a             -- empty
  , Text -> a     -- text
  , Int -> a -> a -- indent
  , a -> a -> a   -- above
  , a -> a -> a   -- beside
  , a -> a -> a   -- choice
  )

eval :: Render a -> Doc -> a
eval
  r@( empty_r
    , text_r
    , indent_r
    , above_r
    , beside_r
    , choice_r
    ) doc0 =
        case doc0 of
          Empty              -> empty_r
          Text cs            -> text_r cs
          Indent k doc       -> indent_r k (eval r doc)
          doc1 `Above`  doc2 -> eval r doc1 `above_r`  eval r doc2
          doc1 `Beside` doc2 -> eval r doc1 `beside_r` eval r doc2
          doc1 `Choice` doc2 -> eval r doc1 `choice_r` eval r doc2

--------------------------------------------------------------------------------

type TextBox =
  ( [DText] -- body
  , (DText) -- last line
  )

indentDText :: Int -> DText -> DText
indentDText = DText.append . flip DText.replicate ' '

empty_tbx :: TextBox
empty_tbx =
  ( []
  , DText.empty
  )

text_tbx :: Text -> TextBox
text_tbx cs =
  ( []
  , DText.fromText cs
  )

indent_tbx :: Indent -> TextBox -> TextBox
indent_tbx k (b, l) =
  ( map (indentDText k) b
  , indentDText k l
  )

above_tbx :: TextBox -> TextBox -> TextBox
above_tbx (b1, l1) (b2, l2) =
  ( b1 ++ l1:b2
  , l2
  )

beside_tbx :: Indent -> TextBox -> TextBox -> TextBox
beside_tbx _ (b1, l1) ([], l2) = (b1, DText.append l1 l2)
beside_tbx k (b1, l1) (b2 : b2s, l2) =
  ( b1 ++ (DText.append l1 b2 : map (indentDText k) b2s)
  , indentDText k l2
  )

textBoxToText :: TextBox -> Text
textBoxToText (b, l) = Text.unlines $ map DText.toText (b ++ [l])

--------------------------------------------------------------------------------

type Format =
  ( Int     -- height
  , Int     -- width
  , Int     -- last line height
  , TextBox -- text-box
  )

render_fmt :: Render Format
render_fmt =
  ( empty_fmt
  , text_fmt
  , indent_fmt
  , above_fmt
  , beside_fmt
  , choice_fmt
  )

empty_fmt :: Format
empty_fmt =
  ( 0
  , 0
  , 0
  , empty_tbx
  )

text_fmt :: Text -> Format
text_fmt cs =
  ( 1
  , Text.length cs
  , Text.length cs
  , text_tbx cs
  )

indent_fmt :: Indent -> Format -> Format
indent_fmt _ fmt@(0, 0, 0, _) = fmt
indent_fmt k (h, w, z, b) =
  ( h
  , k + w
  , k + z
  , indent_tbx k b
  )

above_fmt :: Format -> Format -> Format
(0, 0, 0, _) `above_fmt` fmt2 = fmt2
fmt1 `above_fmt` (0, 0, 0, _) = fmt1
(h1, w1, _, b1) `above_fmt` (h2, w2, z2, b2) =
  ( h1 + h2
  , w1 `max` w2
  , z2
  , above_tbx b1 b2
  )

beside_fmt :: Format -> Format -> Format
(0, 0, 0, _) `beside_fmt` fmt2 = fmt2
fmt1 `beside_fmt` (0, 0, 0, _) = fmt1
(h1, w1, z1, b1) `beside_fmt` (h2, w2, z2, b2) =
  ( h1 + h2 - 1
  , w1 `max` (z1 + w2)
  , z1 + z2
  , beside_tbx z1 b1 b2
  )

choice_fmt :: Format -> Format -> Format
choice_fmt fmt _ = fmt

formatToText :: Format -> Text
formatToText (_, _, _, tb) = textBoxToText tb

--------------------------------------------------------------------------------

{-
type Search = [Format]

render_src :: PageWidth -> Render Search
render_src pw =
  ( empty_src  pw
  , text_src   pw
  , indent_src pw
  , above_src  pw
  , beside_src pw
  , choice_src pw
  )

empty_src :: PageWidth -> Search
empty_src _ = [ empty_fmt ]

text_src :: PageWidth -> Text -> Search
text_src pw cs
  | pw > Text.length cs = []
  | otherwise           = [ text_fmt cs ]

indent_src :: PageWidth -> Indent -> Search -> Search
indent_src = undefined

above_src :: PageWidth -> Search -> Search -> Search
above_src  pw = undefined

beside_src :: PageWidth -> Search -> Search -> Search
beside_src pw = undefined

choice_src :: PageWidth -> Search -> Search -> Search
choice_src pw = undefined
-}
