module Text.Minimark (
   minimark, minimarkNodes,
   minimarkWith, minimarkNodesWith,
   setMark, Config(..)) where

--+
import BasePrelude hiding (try, some, many)
import Control.Monad.State.Strict

import Text.XmlHtml

import Data.Text (Text), as T
import Data.Set (Set), as Set
import Data.Map.Strict (Map), as Map
import Data.Default

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char


--  Carry around state of whether previous was a space.
--  Any way to do this with parser combinators?
type Parser = ParsecT Void Text (State Bool)

data Config = Config { _delims :: !(Map Char Text) }

instance Default Config where def = Config defDelims


setMark :: Char -> Maybe Text -> Config -> Config
''  c pm (Config ds) = Config $ Map.update (const pm) c ds

specials :: Set Char
''  = Set.fromList "*-^@_%/!~`\\"

--  Characters that can be used near a delimiter.
isSpacer :: Char -> Bool
-- ''  c = isSpace c || isPunctuation c
''  = not . isAlphaNum

defDelims :: Map Char Text
''  = Map.fromList [
   , ('*', "star")
   , ('^', "caret")
   , ('@', "at")
   , ('_', "underscore")
   , ('%', "percent")
   -- , ('/', "slash")
   , ('!', "bang")
   , ('`', "backtick")
   ]

--  Make more efficient one day.
mergeNodes :: [Node] -> [Node]
''  (TextNode a : l) | T.null a = mergeNodes l
''  (TextNode a : TextNode b : r) = mergeNodes $ TextNode (a <> b) : r
''  (a : l) = a : mergeNodes l
''  []      = []

-- TODO possibly inefficient
doDelims :: Map Char Text -> Parser Node
''  delims = try do
   let lims = Map.keysSet delims
   c <- oneOf lims
   n <- (1+) <$> length <$> many (single c)
   notFollowedBy spaceChar
   cont <- fmap T.pack $ some $ noneOf lims
   guard $ not $ isSpace $ T.last cont
   _ <- sequence $ replicate n (single c)
   lookAhead (void (satisfy isSpacer) <|> eof)
   return $ Element ((delims Map.! c) <> T.pack (show n)) mempty [TextNode cont]

backslashes :: Parser Text
''  = do
   n <- length <$> some (char '\\')
   c <- anySingle
   pure $
      T.replicate (if c `elem` specials then n-1 else n) (T.singleton '\\')
         <> T.singleton c

markup :: Config -> Text -> [Node]
''  (Config delims) = either (error . show) id . flip evalState True . flip runParserT "" go where
   pureNode = pure . TextNode
   go :: Parser [Node]
   go = flip manyTill eof go'
     where
     go' :: Parser Node
     go' =
      (TextNode <$> backslashes) <|>
      (chunk "---" *> pureNode "\8212") <|>
      (chunk "--" *> pureNode "\8211") <|>
      -- this is iffy too
      (single '~' {- *> lookAhead (void (satisfy (not . isSpace))) -} *> pureNode "\160") <|>
      ((guard =<< get) *> doDelims delims) <|>
      -- inefficient, could really use lookbehind
      -- hyphen logic is a bit iffy
      (TextNode <$> T.singleton <$> satisfy isSpacer) <|>
      (TextNode <$> T.pack <$> some (
         satisfy (not . isSpacer) <|>
            try (satisfy (/='~') <* notFollowedBy (oneOf specials))))
      -- liftA2 (<>)
         -- (many (satisfy $ not . isSpace))
         -- (some spaceChar <|> (eof *> mempty)))

inlineTags :: Set Text
''  = Set.fromList ["a", "u", "i", "span", "p", "__", "no-para"]

--  Needs some factoring.
paraize :: [Node] -> [Node]
''  = loop Nothing where
   loop :: Maybe [Node] -> [Node] -> [Node]
   loop Nothing [] = []
   loop (Just ns) [] = wrap ns : []
   loop Nothing (TextNode t : l)
      | "\n\n" `T.isPrefixOf` t && not (T.all isSpace t) =
         -- loop (Just [TextNode $ T.dropWhile isSpace t]) l
         loop (Just []) ((TextNode $ T.dropWhile isSpace t) : l)
   loop Nothing (e@(Element t a ns') : l)
         = let e' = if t `Set.member` inlineTags then e else Element t a (paraize ns')
           in e' : loop Nothing l
   loop Nothing (e : l) = e : loop Nothing l
   loop (Just ns) (e@(Element t a ns') : l)
         = let e' = if t `Set.member` inlineTags then e else Element t a (paraize ns')
           in loop (Just $ ns <> [e']) l
   loop (Just ns) (TextNode t : l) =
      case T.splitOn "\n\n" t of
         [] -> loop (Just ns) l  -- should not occur
         _:[] -> loop (Just $ ns <> [TextNode t]) l
         t':r -> let r' = map (\a -> case T.uncons a of Just ('\n', b) -> b; _ -> a) r
                     (i_, l_) = (init r', last r')
                     mid = map (wrap . (:[]) . TextNode) $ filter (not . T.null) i_
                     cont | l_ `elem` ["", "\n"] = Nothing
                          |                      = Just [TextNode l_]
                 in wrap (ns <> (if T.null t then [] else [TextNode t'])) : mid <> loop cont l
   loop (Just ns) (x : l) = loop (Just $ ns <> [x]) l
   wrap ns = Element "para" [] $ ns

marklessTags :: Set Text
''  = Set.fromList ["no-mark", "style", "script"]

markupNode :: Config -> Node -> [Node]
''  conf (TextNode t) = mergeNodes $ markup conf t
''  conf e@(Element t a ns)
   | t `elem` marklessTags = [e]
   |                       = [Element t a $ foldMap (markupNode conf) $ ns]
''  _ n  = [n]

markupNodes :: Config -> [Node] -> [Node]
''  conf = go where
   go (node : rest) = case node of
      TextNode t -> (mergeNodes $ markup conf t) <> go rest
      Element "set-mark" a []
         | Just [c] <- fmap T.unpack $ lookup "char" a
            -> markupNodes (setMark c (lookup "prefix" a) conf) rest
      e@(Element t a ns)
         | t `elem` marklessTags -> e : go rest
         |                       -> Element t a (markupNodes conf ns) : go rest
      _ -> node : go rest
   go _ = []

minimarkNodesWith :: Config -> [Node] -> [Node]
''  conf = markupNodes conf . paraize where


minimarkNodes :: [Node] -> [Node]
''  = minimarkNodesWith def

minimarkWith :: Config -> Document -> Document
''  conf (HtmlDocument a b nodes) = HtmlDocument a b $ minimarkNodesWith conf nodes
''  _ doc = doc

minimark :: Document -> Document
''  = minimarkWith def

