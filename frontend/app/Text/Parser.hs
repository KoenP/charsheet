module Text.Parser where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Kind
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
--------------------------------------------------------------------------------

data Parser t a = Parser (t -> (Either String a, t))
  deriving Functor

inform :: e -> Maybe a -> Either e a
inform e Nothing  = Left e
inform _ (Just a) = Right a

runParser :: Parser t a -> t -> (Either String a, t)
runParser (Parser p) t = p t

runParser' :: (Monoid t, Eq t, Show t) => Parser t a -> t -> Either String a
runParser' p t = case runParser p t of
  (Left err, _)                -> Left err
  (Right x, t') | t' == mempty -> Right x
                | otherwise    -> Left ("Unconsumed input: " <> show t')

embed :: (Monoid t', Eq t', Show t') => Parser t' a -> t' -> Parser t a
embed p t' = Parser $ \t -> case runParser' p t' of Left err -> (Left err, t)
                                                    Right x  -> (Right x, t)

instance Applicative (Parser txt) where
  pure x = Parser (\t -> (Right x, t))
  (<*>) = ap

instance Alternative (Parser txt) where
  empty = Parser $ \t -> (Left "Unknown error", t)
  p <|> q = Parser $ \t -> case runParser p t of
    (Left err, _ ) -> runParser q t
    result         -> result

instance Monad (Parser txt) where
  return = pure
  p >>= k = Parser $ \t -> case runParser p t of
    (Right x , t') -> runParser (k x) t'
    (Left err, t') -> (Left err, t')

class (Monad m, Alternative m) => MonadParser (m :: Type -> Type) t c | m -> t, m -> c where
  peek   :: m (Maybe c)
  look   :: m t

  nibble :: m c
  devour :: m t

  push   :: c -> m ()

  eof    :: m ()
  eof    = guard =<< fmap isNothing peek

many1 :: Alternative f => f a -> f [a]
many1 p = (:) <$> p <*> many p

satisfy :: MonadParser m t c => (c -> Bool) -> m c
satisfy p = do
  c <- nibble
  if p c then return c else empty

char :: (MonadParser m t c, Eq c) => c -> m ()
char c = void $ satisfy (== c)

satisfyMaybe :: MonadParser m t c => (c -> Maybe r) -> m r
satisfyMaybe test = do
  c <- nibble
  case test c of Just r  -> return r
                 Nothing -> empty

sepBy :: MonadParser m t c => m a -> m b -> m [a]
sepBy elP sepP = sepBy1 elP sepP <|> pure []

sepBy1 :: MonadParser m t c => m a -> m b -> m [a]
sepBy1 elP sepP = (:) <$> elP <*> many (sepP *> elP)

munch :: MonadParser m t c => (c -> Bool) -> m [c]
munch p = munch1 p <|> pure []

munch1 :: MonadParser m t c => (c -> Bool) -> m [c]
munch1 p = (:) <$> satisfy p <*> munch p

instance MonadParser (Parser Text) Text Char where
  nibble = Parser $ \t -> case Text.uncons t of Nothing     -> (Left "unexpected end of input", t)
                                                Just (c,cs) -> (Right c, cs)

  peek = Parser $ \txt -> case Text.uncons txt of Just (c,_) -> (Right (Just c), txt)
                                                  Nothing    -> (Right Nothing, txt)

  look = Parser $ (\t -> (Right t, t))

  devour = Parser $ (\t -> (Right t, mempty))

  push c = Parser $ (\t -> (Right (), Text.cons c t))

instance MonadParser (Parser [a]) [a] a where
  nibble = Parser $ \case []     -> (Left "unexpected end of input", [])
                          (c:cs) -> (Right c, cs)
  peek = Parser $ \case txt@(c:_) -> (Right (Just c), txt)
                        []        -> (Right Nothing, [])
  look = Parser $ (\t -> (Right t, t))
  devour = Parser $ (\t -> (Right t, mempty))

  push x = Parser $ \xs -> (Right (), x:xs)

instance (Alternative (t m), MonadTrans t, MonadParser m txt c) => MonadParser (t m) txt c where
  nibble = lift nibble
  peek = lift peek
  look = lift look
  devour = lift devour
  push = lift . push
