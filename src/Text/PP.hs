{-|
License     :  CC0 1.0 Universal Public Domain Dedication
Maintainer  :  niswegmann@gmail.com
Stability   :  provisional
Portability :  portable (Haskell 2010)

A highly optimized easy-to-use pretty-printer that doesn't care about
page-widths.
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

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid (Monoid (..))
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
render = tbx2text . doc2tbx

--------------------------------------------------------------------------------

indentDText :: Int -> DText -> DText
indentDText = mappend . flip DText.replicate ' '

--------------------------------------------------------------------------------

type TextBox =
  ( DList DText -- body
  , DText       -- last line
  , Bool        -- body is nill
  , Int         -- length of last line
  )

empty_tbx :: TextBox
empty_tbx =
  ( DList.empty
  , DText.empty
  , True
  , 0
  )

text_tbx :: Text -> TextBox
text_tbx cs =
  ( DList.empty
  , DText.fromText cs
  , True
  , Text.length cs
  )

indent_tbx :: Int -> TextBox -> TextBox
indent_tbx k (b, l, v, z) =
  ( fmap (indentDText k) b
  , indentDText k l
  , v
  , k + z
  )

above_tbx :: TextBox -> TextBox -> TextBox
(_, _, True, 0) `above_tbx` tbx2            = tbx2
tbx1            `above_tbx` (_, _, True, 0) = tbx1
(b1, l1, _, _)  `above_tbx` (b2, l2, _, z2) =
  ( b1 `mappend` (l1 `DList.cons` b2)
  , l2
  , False
  , z2
  )

beside_tbx :: TextBox -> TextBox -> TextBox
(_, _, True, 0)  `beside_tbx` tbx2              = tbx2
tbx1             `beside_tbx` (_, _, True, 0)   = tbx1
(b1, l1, v1, z1) `beside_tbx` (_, l2, True, z2) =
  ( b1
  , l1 `mappend` l2
  , v1
  , z1 + z2
  )
beside_tbx (b1, l1, _, z1) (b2, l2, False, z2)  =
  let
    b2hd = DList.head b2
    b2tl = DList.tail b2
  in
    ( b1 `mappend` (mappend l1 b2hd `DList.cons` fmap (indentDText z1) b2tl)
    , indentDText z1 l2
    , False
    , z1 + z2
    )

--------------------------------------------------------------------------------

doc2tbx :: Doc -> TextBox
doc2tbx doc0 =
  case doc0 of
    Empty              -> empty_tbx
    Text cs            -> text_tbx cs
    Indent k doc       -> indent_tbx k (doc2tbx doc)
    doc1 `Above`  doc2 -> doc2tbx doc1 `above_tbx`  doc2tbx doc2
    doc1 `Beside` doc2 -> doc2tbx doc1 `beside_tbx` doc2tbx doc2

tbx2text :: TextBox -> Text
tbx2text (b, l, _, _) =
  Text.unlines $
    DList.toList $
      fmap DText.toText $
        b `mappend` DList.singleton l
