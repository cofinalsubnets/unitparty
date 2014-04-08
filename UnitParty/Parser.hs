module UnitParty.Parser (parseUnit, parseAmount) where

import UnitParty.Types
import UnitParty.Units

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (many, optional)
import Control.Monad ((>=>))
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

    expop = do
      u <- group <|> simple
      spaces
      char '^'
      spaces
      n <- number
      return $ u **~ n

    binop = do
      u1 <- group <|> try expop <|> simple
      spaces
      op <- (char '*' >> return (*~)) <|> (char '/' >> return (/~))
      spaces
      u2 <- unit
      return $ u1 `op` u2

    number =  (char '-' >> fmap (negate . read) (many1 digit))
          <|> fmap read (many1 digit)

-- parser for quantities
parseAmount :: String -> Either ParseError Double
parseAmount = parse (amount <* eof)""
  where
    amount = signed <|> unsigned
    signed = char '-' >> fmap negate unsigned
    unsigned = try dec <|> (int >>= return . read)
    int = many1 digit
    dec = int >>= \i1 -> char '.' >> int >>= \i2 ->
      return $ read i1 + read i2 * (10 ** fromIntegral (negate $ length i2))

getUnit :: String -> Maybe Unit
getUnit u =  M.lookup u units <|>
  (getPrefix u >>= \(v, s') -> getUnit s' >>= return . v)

getPrefix :: String -> Maybe (Unit -> Unit, String)
getPrefix s = do
  (p, v) <- find ((`isPrefixOf` s) . fst) (M.toList metricPrefixes)
  s' <- stripPrefix p s
  return (v, s')

