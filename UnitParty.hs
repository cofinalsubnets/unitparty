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
import qualified Data.Map as M

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
main = getArgs >>= \args -> case parseArgs args of
  Left err -> doError err
  Right [] -> usage >>= putStr
  Right as -> mapM_ doAction as

doError :: String -> IO ()
doError s = hPutStrLn stderr s >> exitFailure

usage :: IO String
usage = getProgName >>= return . ("Usage: "++) . (++ usageInfo header options)
  where header = " [OPTIONS]"

parseArgs :: [String] -> Either String [Action]
parseArgs args = case getOpt Permute options args of
  (o,_,[]) -> do
    opts <- foldM (flip ($)) defaults o
    fmap actions $ setConv opts
  (_,_,es) -> Left . init $ concat es

  where
    setConv o@(Opts f t a True as) = case (f, t) of
      (Just f', Just t') -> return o { actions = Convert f' t' a : as }
      (Just _, _)        -> Left "Missing destination unit"
      _                  -> Left "Missing source unit"
    setConv o = return o

doAction :: Action -> IO ()
doAction action = case action of

  Help -> usage >>= putStr

  Convert f t a -> case convert (unit f) (unit t) of
    Left err -> doError $ show err
    Right c  -> putStrLn $ unwords [show a, name f, "=", show $ c a, name t]

  List -> mapM_ putStrLn $ M.keys baseUnits

  Analyze u -> putStrLn $
    name u ++ ": " ++ show (fst . (\(U n) -> M.findMax n) $ unit u)

  DYK -> do putStrLn "Did you know... "
            randomUnits >>= doAction . ($1) . uncurry Convert
  where
    randomUnits = do
      [(n1,u1), (n2,u2)] <- mapM (sample . M.toList) $ [baseUnits, pluralUnits]
      if equidimensional u1 u2 then return (NU n1 u1, NU n2 u2) else randomUnits
    sample l = fmap (l!!) $ randomRIO (0, length l - 1)


defaults :: Opts
defaults = Opts Nothing Nothing 1 False []

options :: [OptDescr (Opts -> Either String Opts)]
options =
  [ Option "f" ["from"]    (ReqArg setFrom "FROM")    "unit to convert from"
  , Option "t" ["to"]      (ReqArg setTo   "TO")      "unit to convert to"
  , Option "a" ["amount"]  (ReqArg setAmt  "AMOUNT")  "amount to convert"
  , Option []  ["analyze"] (ReqArg setAna  "ANALYZE") "print dimensions"
  , Option []  ["dyk"]     (NoArg setDyk)             "print a random did-you-know"
  , Option []  ["list"]    (NoArg setList)            "list known units"
  , Option "h" ["help"]    (NoArg setHelp)            "show this message"
  ]
  where
    setFrom f o = getParsed parseUnit f >>= \u ->
                    return o{from = Just $ NU f u} >>= setConv
    setTo t o   = getParsed parseUnit t >>= \u ->
                    return o{to = Just $ NU t u} >>= setConv
    setAmt a o  = getParsed parseAmount a >>= \q ->
                    return o{amount = q} >>= setConv
    setAna a o  = getParsed parseUnit a >>= \u ->
                    return o{actions=Analyze (NU a u):actions o}

    setDyk  o = return o{actions=DYK:actions o}
    setList o = return o{actions=List:actions o}
    setHelp o = return o{actions=Help:actions o}
    setConv o = return o{doConversion=True}

    getParsed p u = case p u of
      Left err -> Left $ show err
      Right u' -> return u'

