module UnitParty.Parser (parseUnit, parseAmount) where

import UnitParty.Types
import UnitParty.Ops
import UnitParty.Units

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (many, optional)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Char (toLower)
import Control.Monad

import qualified Data.Map as M

parseUnit :: String -> Either ParseError Unit
parseUnit = parse (unit <* eof) "unit" . map toLower
  where
    unit = liftM2 (foldl $ flip ($)) base (many $ try bin2) <* spaces
    simple = many1 letter >>= maybe (fail "Unknown unit") return . getUnit
    group = string "(" *> unit <* string ")"
    expop = (group <|> simple) >>= \u ->
      spaces >> char '^' >> spaces >> number >>= return . (u**~)
    number =  fmap (read . concat) $ sequence [opt (string "-"), many1 digit]
    binop = spaces >> (char '*' >> return (*~)) <|> (char '/' >> return (/~))
    base = spaces >> (try expop <|> group <|> simple)
    bin2 = liftM2 (($) . flip) binop base

-- parser for quantities
parseAmount :: String -> Either ParseError Double
parseAmount = parse (num <* eof) "quantity"
  where num = fmap (read . concat) $ sequence
          [opt (string "-"), many1 digit, opt (string "."), opt (many1 digit)]

getUnit :: String -> Maybe Unit
getUnit u =  M.lookup u units <|>
  (getPrefix u >>= \(v, r) -> fmap v $ getUnit r)

getPrefix :: String -> Maybe (Unit -> Unit, String)
getPrefix s = do
  (p, v) <- find ((`isPrefixOf` s) . fst) (M.toList metricPrefixes)
  fmap ((,) v) $ stripPrefix p s

opt = fmap (maybe "" id) . optionMaybe
