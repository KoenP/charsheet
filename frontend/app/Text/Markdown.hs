-- | A quickly made and poorly tested parser for some unspecified subset of Markdown.
--   Using a library pulls in too many dependencies and balloons the js file size.
module Text.Markdown where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Char
import Data.Kind
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Optics

import Text.Parser
--------------------------------------------------------------------------------

parseMarkdown :: Text -> Either String Markdown
parseMarkdown = runMarkdownParser' markdownP <=< mapM (runParser' markdownLineP) . Text.lines

type Markdown = [Block]

data Block = ListBlock !EnumStyle ![[Block]] | Leaf !LeafBlock
  deriving (Show, Generic)

data LeafBlock = Paragraphs ![[MarkdownText]]
               | Table { headers :: !(Maybe [MarkdownText]), rows :: ![[MarkdownText]] }
               | ATXHeading !Int !MarkdownText
               | ThematicBreak
               deriving (Show, Generic)

type MonadMarkdownParser m = (MonadParser m [MarkdownLine] MarkdownLine, MonadReader Int m)

runMarkdownParser :: ReaderT Int (Parser [MarkdownLine]) a -> Int -> [MarkdownLine] -> (Either String a, [MarkdownLine])
runMarkdownParser p indentation md = runParser (runReaderT p indentation) md

runMarkdownParser' :: ReaderT Int (Parser [MarkdownLine]) a -> [MarkdownLine] -> Either String a
runMarkdownParser' p md = fst $ runMarkdownParser p 0 md

markdownP :: MonadMarkdownParser m => m Markdown
markdownP = many (many (char BlankLine) *> blockP <* many (char BlankLine))

blockP :: MonadMarkdownParser m => m Block
blockP = blockAtIndentationP
  <|> (Leaf . uncurry ATXHeading <$> satisfyMaybe (preview $ #_BlockSeparatorLine % #_ATXHeadingLine))
  <|> (Leaf ThematicBreak <$ satisfyMaybe (preview $ #_BlockSeparatorLine % #_ThematicBreakLine))

blockAtIndentationP :: MonadMarkdownParser m => m Block
blockAtIndentationP = do
  indentation <- ask
  guard . fromMaybe False . fmap (not . endOfBlock indentation) =<< peek

  fmap (uncurry ListBlock) listBlockP <|> fmap (Leaf . uncurry Table) tableP <|> fmap (Leaf . Paragraphs) paragraphsP

  where
    endOfBlock _ (BlockSeparatorLine _) = True
    endOfBlock ambientIndent (MergeableLine lineIndent _) = lineIndent < ambientIndent
    endOfBlock _ _ = False

listBlockP :: MonadMarkdownParser m => m (EnumStyle, [[Block]])
listBlockP = do
  (indentation, enumStyle, firstLine) <- satisfyMaybe $ \case MergeableLine i (ListElementLine s t) -> Just (i,s,t)
                                                              _                                     -> Nothing
  let newIndentation = indentation + case enumStyle of
        Numbers   -> 3
        Bullets _ -> 2
  push (MergeableLine newIndentation (TextLine firstLine))
  currentBullet <- local (const $ newIndentation) $ many (many (char BlankLine) *> blockAtIndentationP)

  -- TODO gobbling up different enum styles here as well.
  (_, otherBullets) <- (many (char BlankLine) *> listBlockP) <|> pure (enumStyle, [])

  return (enumStyle, currentBullet : otherBullets)

paragraphsP :: forall m. MonadMarkdownParser m => m [[MarkdownText]]
paragraphsP = do
  ambientIndentation <- ask
  paragraphP ambientIndentation `sepBy1` many1 (char BlankLine)

  where
    paragraphP :: Int -> m [MarkdownText]
    paragraphP ambientIndentation = many1 $ satisfyMaybe $ \case
      MergeableLine indentation (TextLine text) | indentation >= ambientIndentation -> Just text
      _ -> Nothing

tableP :: MonadMarkdownParser m => m (Maybe [MarkdownText], [[MarkdownText]])
tableP = many1 (satisfyMaybe $ preview $ #_MergeableLine % _2 % #_TableLine) >>= \case
  TableRow header : TableHeaderSep : rows -> pure (Just header, [row | TableRow row <- rows])
  rows -> pure (Nothing, [row | TableRow row <- rows])

data MarkdownLine = BlockSeparatorLine !BlockSeparatorLine
                  | MergeableLine {indent :: !Int, line :: !MergeableLine}
                  | BlankLine
  deriving (Eq, Show, Generic)

data BlockSeparatorLine = ATXHeadingLine !Int !MarkdownText
                        | ThematicBreakLine
  deriving (Eq, Show, Generic)

data MergeableLine = ListElementLine !EnumStyle !MarkdownText
                   | TextLine !MarkdownText
                   | TableLine !TableLine
  deriving (Eq, Show, Generic)

data TableLine = TableRow [MarkdownText]
               | TableHeaderSep
  deriving (Eq, Show, Generic)

data EnumStyle = Numbers | Bullets !Char
  deriving (Eq, Show, Generic)

type MarkdownText = [Snippet]

data Snippet = Snippet !Markup !Text
  deriving (Eq, Show, Generic)

data Markup = Plain | Bold
  deriving (Eq, Show, Generic)

markdownTextP :: Parser Text MarkdownText
markdownTextP = many (plainTextP <|> boldTextP)
  where
    plainTextP = mkSnippet Plain <$> many1 (notUnescapedTerminatorP '\\' '*')

    -- TODO took a lazy shortcut to parse multiple *, this doesn't check if the number is matched.
    boldTextP = mkSnippet Bold <$> (many1 (char '*') *> many (notUnescapedTerminatorP '\\' '*') <* many1 (char '*'))

    mkSnippet markup = Snippet markup . Text.pack

markdownLineP :: Parser Text MarkdownLine
markdownLineP = do
  indent <- length <$> munch isSpace
  atxHeadingLineP indent
    <|> thematicBreakLineP indent
    <|> (BlankLine <$ eof)
    <|> fmap (MergeableLine indent . TableLine) tableLineP
    <|> listElementLineP indent
    <|> textLineP indent

listElementLineP :: Int -> Parser Text MarkdownLine
listElementLineP indent = numberedListElP <|> bulletListElP
  where
    numberedListElP = satisfy isNumber *> satisfy (`elem` ['.', ')']) *> satisfy isSpace
      *> fmap (MergeableLine indent . ListElementLine Numbers) markdownTextP
    bulletListElP = do
      bulletChar <- satisfy (`elem` ['-','+','*'])
      void $ satisfy isSpace
      MergeableLine indent . ListElementLine (Bullets bulletChar) <$> markdownTextP

maxLeadingWhitespaceP :: Int -> Parser Text ()
maxLeadingWhitespaceP n = guard =<< (<= n) . length <$> munch isSpace

thematicBreakLineP :: Int -> Parser Text MarkdownLine
thematicBreakLineP indent = do
  guard (indent <= 4)
  guard =<< (`elem` ["***", "---", "___"]) <$> replicateM 3 nibble
  guard =<< Text.all isSpace <$> devour
  return (BlockSeparatorLine ThematicBreakLine)

atxHeadingLineP :: Int -> Parser Text MarkdownLine
atxHeadingLineP indent = do
  guard (indent <= 3)
  headerLevel <- length <$> munch1 (=='#')
  guard $ headerLevel <= 6
  remainder <- markdownTextP

  let headingText = case unsnoc remainder of
        Just (snippets, Snippet Plain lastSnippet)
          | Just (ws, w) <- unsnoc (Text.words lastSnippet), Text.all (=='#') w ->
            snippets <> [Snippet Plain (Text.unwords ws)]
        _ -> remainder

  return $ BlockSeparatorLine $ ATXHeadingLine headerLevel headingText


textLineP :: Int -> Parser Text MarkdownLine
textLineP indent = MergeableLine indent . TextLine <$> markdownTextP

notUnescapedTerminatorP :: (MonadParser m t c, Eq c) => c -> c -> m c
notUnescapedTerminatorP escapeChar terminatorChar = (char escapeChar *> nibble) <|> satisfy (/= terminatorChar)

tableLineP :: Parser Text TableLine
tableLineP = char '|' *> ((TableHeaderSep <$ headerSepP) <|> fmap TableRow rowP) <* char '|' <* eof
  where
    rowPlainTextP = many1 (notUnescapedTerminatorP '\\' '|') `sepBy1` char '|'

    -- TODO bold text
    rowP :: Parser Text [MarkdownText]
    rowP = do
      plainText <- map Text.pack <$> rowPlainTextP
      mapM (embed markdownTextP) plainText
      -- map ((:[]) . Snippet Plain . Text.pack) <$> 

    headerSepP = headerSepCellP `sepBy1` char '|'
    headerSepCellP = do
      void $ munch isSpace
      n <- length <$> many (char '-')
      guard (n >= 3)
      void $ munch isSpace
