module UnitParty.Parser (parseUnit, parseAmount) where

import UnitParty.Types
import UnitParty.Units

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (many, optional)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Char (toLower)

import qualified Data.Map as M

parseUnit :: String -> Either ParseError Unit
parseUnit = parse (unit <* eof) "" . map toLower
  where
    unit = spaces *> (compound <|> group <|> simple) <* spaces

    simple = many1 letter >>= maybe (fail $ "Unknown unit") return . getUnit

    group = string "(" *> unit <* string ")"

    compound = try binop <|> try expop

    expop = (group <|> simple) >>= \u ->
      spaces >> char '^' >> spaces >> number >>= return . (u**~)

    binop = flip ($) <$> ((group <|> try expop <|> simple) <* spaces)
                     <*> ((char '*' >> return (*~)) <|> (char '/' >> return (/~)))
                     <*> unit

    number =  fmap (read . concat) $ sequence [opt (string "-"), many1 digit]

-- parser for quantities
parseAmount :: String -> Either ParseError Double
parseAmount = parse (num <* eof) ""
  where num = fmap (read . concat) $ sequence
          [opt (string "-"), many1 digit, opt (string "."), opt (many1 digit)]

getUnit :: String -> Maybe Unit
getUnit u =  M.lookup u units <|>
  (getPrefix u >>= \(v, s') -> fmap v $ getUnit s')

getPrefix :: String -> Maybe (Unit -> Unit, String)
getPrefix s = do
  (p, v) <- find ((`isPrefixOf` s) . fst) (M.toList metricPrefixes)
  fmap ((,) v) $ stripPrefix p s

opt = fmap (maybe "" id) . optionMaybe

