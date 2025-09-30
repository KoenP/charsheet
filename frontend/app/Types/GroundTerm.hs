module Types.GroundTerm where

--------------------------------------------------------------------------------
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP
--------------------------------------------------------------------------------

data GroundTerm = Atom !Text
                | Int !Int
                | List ![GroundTerm]
                | Compound { functor :: !Text, args :: ![GroundTerm]}
  deriving Show

termP :: ReadP GroundTerm
termP = do { atom <- atomP; fmap (Compound atom) argsP <++ return (Atom atom) }
  <++ fmap Int intP
  <++ fmap List listP

atomP :: ReadP Text
atomP = fmap Text.pack $ bareWordP <++ quotedAtomP <++ operatorSequenceP
  where
    bareWordP :: ReadP String
    bareWordP = do
      c  <- satisfy isLower
      cs <- munch (\x -> isAlphaNum x || x == '_')
      return (c:cs)

    operatorSequenceP :: ReadP String
    operatorSequenceP = munch1 (`Text.elem` operatorChars)

    quotedAtomP :: ReadP String
    quotedAtomP = char '\'' *> many quotedAtomCharP <* char '\''

    quotedAtomCharP :: ReadP Char
    quotedAtomCharP = (char '\\' *> get) <++ satisfy (\c -> c /= '\\' && c /= '\'')

    operatorChars :: Text
    operatorChars = "+-*/^:=<>?@$"

intP :: ReadP Int
intP = option id (negate <$ char '-') <*> fmap read (munch1 isDigit)

listP :: ReadP [GroundTerm]
listP = char '[' *> (termP `sepBy` char ',') <* char ']'

argsP :: ReadP [GroundTerm]
argsP = char '(' *> (termP `sepBy1` char ',') <* char ')'
