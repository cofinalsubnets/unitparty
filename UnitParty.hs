import UnitParty.Types
import UnitParty.Parser
import UnitParty.Convert
import UnitParty.Units
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Monad
import System.Random
import Data.Map (findMax, keys, toList)

data Opts = Opts { from    :: Maybe NamedUnit
                 , to      :: Maybe NamedUnit
                 , amount  :: Double
                 , doConversion :: Bool
                 , actions :: [Action]
                 }

data Action = Convert NamedUnit NamedUnit Double
            | Analyze NamedUnit | List | DYK | Help

data NamedUnit = NU { name :: String, unit :: Unit }

main :: IO ()
main = fmap parseArgs getArgs >>= either doError go
  where go x = if null x then printUsage else mapM_ doAction x

parseArgs :: [String] -> Either String [Action]
parseArgs args = case getOpt Permute options args of
  (o,_,[]) -> fmap actions $ foldM (flip ($)) defaults o >>= addConv
  (_,_,es) -> Left $ concat es

  where
    defaults = Opts Nothing Nothing 1 False []
    addConv o@(Opts f t a True as) = case (f, t) of
      (Just f', Just t') -> return o { actions = Convert f' t' a : as }
      (Just _, _)        -> Left "Conversion error: Missing destination unit"
      _                  -> Left "Conversion error: Missing source unit"
    addConv o = return o

doAction :: Action -> IO ()
doAction action = case action of

  Help -> printUsage

  List -> mapM_ putStrLn $ keys baseUnits

  Analyze (NU n (U u)) -> putStrLn $ n ++ ": " ++ show (fst $ findMax u)

  Convert f t a -> case convert (unit f) (unit t) of
    Left err -> doError $ show err
    Right c  -> putStrLn $ unwords [show a, name f, "=", show (c a), name t]

  DYK -> do putStrLn "Did you know..."
            randomUnits >>= doAction . ($1) . uncurry Convert
  where
    randomUnits = do
      [(n1,u1), (n2,u2)] <- mapM (sample . toList) [baseUnits, pluralUnits]
      if equidimensional u1 u2 then return (NU n1 u1, NU n2 u2) else randomUnits

    sample l = fmap (l!!) $ randomRIO (0, length l - 1)

doError :: String -> IO ()
doError s = hPutStrLn stderr s >> exitFailure

printUsage :: IO ()
printUsage = getProgName >>=
  putStr . ("Usage: "++) . (++ usageInfo " [OPTIONS]" options)

options :: [OptDescr (Opts -> Either String Opts)]
options =
  [ Option "f" ["from"]    (ReqArg setFrom "FROM")    "unit to convert from"
  , Option "t" ["to"]      (ReqArg setTo   "TO")      "unit to convert to"
  , Option "a" ["amount"]  (ReqArg setAmt  "AMOUNT")  "amount to convert"
  , Option ""  ["analyze"] (ReqArg setAna  "ANALYZE") "print dimensions"
  , Option ""  ["dyk"]     (NoArg setDyk)             "print a random did-you-know"
  , Option ""  ["list"]    (NoArg setList)            "list known units"
  , Option "h" ["help"]    (NoArg setHelp)            "show this message"
  ]
  where
    setFrom f o = getParsed parseUnit f >>= \u ->
                    return o{from = Just $ NU f u, doConversion = True}
    setTo t o   = getParsed parseUnit t >>= \u ->
                    return o{to = Just $ NU t u, doConversion = True}
    setAmt a o  = getParsed parseAmount a >>= \q ->
                    return o{amount = q, doConversion = True}
    setAna a o  = getParsed parseUnit a >>= \u ->
                    return o{actions = Analyze (NU a u):actions o}

    setDyk  o = return o{actions=DYK:actions o}
    setList o = return o{actions=List:actions o}
    setHelp o = return o{actions=Help:actions o}

    getParsed = (either (Left . show) Right .)
