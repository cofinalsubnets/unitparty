module UnitParty.Units
( units
, baseUnits
, pluralUnits
, metricPrefixes
) where

import qualified Data.Map as M
import Data.List
import UnitParty.Types
import UnitParty.Ops

-- THE BIG UNIT LIST
--
-- SI base units
kelvin   = unit Temperature
second   = unit Time
ampere   = unit Current
candela  = unit LuminousIntensity
metre    = unit Distance
mole     = unit Quantity
kilogram = unit Mass

-- SI derived units
celsius   = kelvin -~ constant 273.15
hertz     = constant 1 /~ second
newton    = kilogram *~ metre /~ second **~ 2
pascal    = newton /~ metre **~ 2
joule     = newton *~ metre
watt      = joule /~ second
coulomb   = ampere *~ second
volt      = watt /~ ampere
farad     = coulomb /~ volt
ohm       = volt /~ ampere
siemens   = constant 1 /~ ohm
weber     = joule /~ ampere
tesla     = weber /~ metre **~ 2
henry     = ohm *~ second
lux       = candela /~ metre **~ 2
gray      = joule /~ kilogram
katal     = mole /~ second
becquerel = hertz   -- same dimensions
sievert   = gray    -- different
lumen     = candela -- semantics

-- Imperial / Customary units
-- these are kind of a mess on account of "imperial" is not even one standard.
-- in general i have tried to use US imperial units, but there are almost
-- certainly inconsistencies. some units are also very specific to a time and
-- place, and poorly (or not at all) attested otherwise. but that is what makes
-- it a unit party

-- length
planckLength = constant (1.616199 * 10 ** (-35)) *~ metre
thou    = constant 0.0000254 *~ metre
inch    = constant 1000 *~ thou
foot    = constant 12 *~ inch
yard    = constant 3 *~ foot
chain   = constant 22 *~ yard
furlong = constant 10 *~ chain
mile    = constant 8 *~ furlong
league  = constant 3 *~ mile
fathom  = constant 2 *~ yard
cable   = constant 100 *~ fathom
link    = constant (1/100) *~ chain
rod     = constant 25 *~ link
hand    = constant 0.1016 *~ metre
nauticalMile = constant 10 *~ cable
cubit   = constant 18 *~ inch
ell     = constant 45 *~ inch
finger  = constant (7/8) *~ inch
fermi   = constant (10 ** (-15)) *~ metre
micron  = constant (10 ** (-6)) *~ metre

-- area
perch    = rod **~ 2
rood     = furlong *~ rod
acre     = furlong *~ chain
section  = constant 640 *~ acre
township = constant 36 *~ section

-- volume
fluidOunce = constant 0.0284130625 *~ litre
gill       = constant 5 *~ fluidOunce
pint       = constant 4 *~ gill
quart      = constant 2 *~ pint
gallon     = constant 4 *~ quart
dram       = constant (1/8) *~ fluidOunce
scruple    = constant (1/3) *~ dram
minim      = constant (1/20) *~ scruple
teaspoon   = constant 60 *~ minim
tablespoon = constant 3 *~ teaspoon
shot       = constant 3 *~ tablespoon
hogshead   = constant 63 *~ gallon
barrel     = constant (1/2) *~ hogshead
peck       = constant 2 *~ gallon
bushel     = constant 4 *~ peck
tun        = constant 252 *~ gallon
pipe       = constant 126 *~ gallon
puncheon   = constant 84 *~ gallon
tierce     = constant 42 *~ gallon
rundlet    = constant 18 *~ gallon
firkin     = constant 9 *~ gallon
dessertspoon = constant (1/12) *~ gill

-- weight
pound   = constant 0.45359238 *~ kilogram
grain   = constant (1/7000) *~ pound
drachm  = constant (1/256) *~ pound
ounce   = constant (1/16) *~ pound
stone   = constant 14 *~ pound
quarter = constant 2 *~ stone
ton     = constant 20 *~ hundredweight
wey     = constant 18 *~ stone
pennyweight   = constant 24 *~ grain
hundredweight = constant 4 *~ quarter

-- temperature
rankine    = constant (5/9) *~ kelvin
fahrenheit = rankine -~ constant 459.67

-- other units
btu        = constant 1055 *~ joule
calorie    = constant 4.184 *~ joule
horsepower = constant 745.69987158 *~ watt
poundForce = constant 32.174049 *~ pound *~ foot /~ second **~ 2
slug       = poundForce *~ second **~ 2 /~ foot

-- still other units
-- length
angstrom  = constant (10 ** (-10)) *~ metre
lightYear = constant (9.4607 * 10 ** 15) *~ metre
parsec    = constant (648000/pi) *~ astronomicalUnit
astronomicalUnit = constant 149597870.7 *~ kilo metre
-- area
are     = constant 100 *~ metre **~ 2
barn    = constant (10 ** (-28)) *~ metre **~ 2
hectare = constant 10000 *~ metre **~ 2
dunam    = constant 1000 *~ metre **~ 2 -- ottoman
guntha   = constant 121 *~ yard **~ 2   -- indian
-- pressure
bar   = constant 100000 *~ pascal
barye = constant (1/10) *~ kilogram /~ (metre *~ second **~ 2)
mmHg  = constant 133.322387415 *~ pascal
torr  = constant 101325 *~ pascal
atmosphere = constant 760 *~ torr
-- energy
erg = constant (10 ** (-7)) *~ joule
electronVolt = constant (1.602176565 * 10 ** (-19)) *~ joule
-- force
dyne = constant (10 ** (-5)) *~ newton
-- time
planckTime = constant (5.39106 * 10 ** (-44)) *~ second
minute = constant 60 *~ second
hour   = constant 60 *~ minute
day    = constant 24 *~ hour
week   = constant 7  *~ day
year   = constant 365.25 *~ day -- julian
svedberg = constant (10 ** (-13)) *~ second
fortnight = constant 2 *~ week

-- volume
litre  = constant 0.001 *~ metre **~ 3
hobbit = constant 2.5 *~ bushel -- welsh. really!
-- mass
gram    = constant (1/1000) *~ kilogram
dalton  = constant (1.66053886 * 10 ** (-27)) *~ kilogram
tonne   = constant 1000 *~ kilogram
carat   = constant (1/5000) *~ kilogram
shekel  = constant 180 *~ grain
quintil = constant 100 *~ ton -- french, used to quantify wine grape production

-- celestial bodies
sun     = constant 332946 *~ earth
mercury = constant 0.055 *~ earth
venus   = constant 0.815 *~ earth
earth   = constant (5.97219 * 10 ** 24) *~ kilogram
moon    = constant 0.0123 *~ earth
mars    = constant 0.107 *~ earth
jupiter = constant 317.8 *~ earth
saturn  = constant 95.16 *~ earth
uranus  = constant 14.536 *~ earth
neptune = constant 17.147 *~ earth
pluto   = constant 0.00218 *~ earth -- you'll always be a planet to me old friend

-- nothing else is like a sverdrup <3
sverdrup = constant 1000000 *~ metre **~ 3 /~ second

-- scaling functions
yotta = (constant (10 ** 24) *~)
zetta = (constant (10 ** 21) *~)
exa   = (constant (10 ** 18) *~)
peta  = (constant (10 ** 15) *~)
tera  = (constant (10 ** 12) *~)
giga  = (constant (10 ** 9) *~)
mega  = (constant (10 ** 6) *~)
kilo  = (constant (10 ** 3) *~)
hecto = (constant (10 ** 2) *~)
deca  = (constant (10 ** 1) *~)
deci  = (constant (10 ** (-1)) *~)
centi = (constant (10 ** (-2)) *~)
milli = (constant (10 ** (-3)) *~)
micro = (constant (10 ** (-6)) *~)
nano  = (constant (10 ** (-9)) *~)
pico  = (constant (10 ** (-12)) *~)
femto = (constant (10 ** (-15)) *~)
atto  = (constant (10 ** (-18)) *~)
zepto = (constant (10 ** (-21)) *~)
yocto = (constant (10 ** (-24)) *~)

baseUnits :: Map String Unit
baseUnits = M.fromList $
  [ ("kelvin", kelvin)
  , ("second", second)
  , ("ampere", ampere)
  , ("candela", candela)
  , ("metre", metre)
  , ("mole", mole)
--  , ("kilogram", kilogram) -- parser can read "kilo""gram"
  , ("celsius", celsius)
  , ("hertz", hertz)
  , ("newton", newton)
  , ("pascal", pascal)
  , ("joule", joule)
  , ("watt", watt)
  , ("coulomb", coulomb)
  , ("volt", volt)
  , ("farad", farad)
  , ("ohm", ohm)
  , ("siemens", siemens)
  , ("weber", weber)
  , ("tesla", tesla)
  , ("henry", henry)
  , ("lumen", lumen)
  , ("lux", lux)
  , ("becquerel", becquerel)
  , ("gray", gray)
  , ("sievert", sievert)
  , ("katal", katal)
  , ("thou", thou)
  , ("inch", inch)
  , ("foot", foot)
  , ("yard", yard)
  , ("chain", chain)
  , ("furlong", furlong)
  , ("mile", mile)
  , ("league", league)
  , ("fathom", fathom)
  , ("cable", cable)
  , ("nauticalmile", nauticalMile)
  , ("link", link)
  , ("rod", rod)
  , ("hand", hand)
  , ("perch", perch)
  , ("rood", rood)
  , ("acre", acre)
  , ("section", section)
  , ("township", township)
  , ("fluidounce", fluidOunce)
  , ("gill", gill)
  , ("pint", pint)
  , ("quart", quart)
  , ("gallon", gallon)
  , ("dram", dram)
  , ("scruple", scruple)
  , ("minim", minim)
  , ("teaspoon", teaspoon)
  , ("tablespoon", tablespoon)
  , ("shot", shot)
  , ("hogshead", hogshead)
  , ("barrel", barrel)
  , ("peck", peck)
  , ("bushel", bushel)
  , ("tun", tun)
  , ("pipe", pipe)
  , ("butt", pipe)
  , ("puncheon", puncheon)
  , ("tierce", tierce)
  , ("rundlet", rundlet)
  , ("firkin", firkin)
  , ("dessertspoon", dessertspoon)
  , ("pound", pound)
  , ("grain", grain)
  , ("drachm", drachm)
  , ("ounce", ounce)
  , ("stone", stone)
  , ("quarter", quarter)
  , ("hundredweight", hundredweight)
  , ("ton", ton)
  , ("pennyweight", pennyweight)
  , ("rankine", rankine)
  , ("fahrenheit", fahrenheit)
  , ("btu", btu)
  , ("calorie", calorie)
  , ("horsepower", horsepower)
  , ("poundforce", poundForce)
  , ("slug", slug)
  , ("angstrom", angstrom)
  , ("lightyear", lightYear)
  , ("parsec", parsec)
  , ("astronomicalunit", astronomicalUnit)
  , ("are", are)
  , ("barn", barn)
  , ("hectare", hectare)
  , ("bar", bar)
  , ("atmosphere", atmosphere)
  , ("barye", barye)
  , ("mmhg", mmHg)
  , ("torr", torr)
  , ("erg", erg)
  , ("electronvolt", electronVolt)
  , ("dyne", dyne)
  , ("minute", minute)
  , ("hour", hour)
  , ("day", day)
  , ("week", week)
  , ("fortnight", fortnight)
  , ("year", year)
  , ("litre", litre)
  , ("hobbit", hobbit)
  , ("gram", gram)
  , ("dalton", dalton)
  , ("tonne", tonne)
  , ("carat", carat)
  , ("shekel", shekel)
  , ("wey", wey)
  , ("quintil", quintil)
  , ("sun", sun)
  , ("mercury", mercury)
  , ("venus", venus)
  , ("mars", mars)
  , ("earth", earth)
  , ("moon", moon)
  , ("jupiter", jupiter)
  , ("saturn", saturn)
  , ("uranus", uranus)
  , ("neptune", neptune)
  , ("pluto", pluto)
  , ("sverdrup", sverdrup)
  , ("svedberg", svedberg)
  , ("cubit", cubit)
  , ("ell", ell)
  , ("finger", finger)
  , ("fermi", fermi)
  , ("micron", micron)
  , ("dunam", dunam)
  , ("stremma", dunam)
  , ("guntha", guntha)
  , ("plancktime", planckTime)
  , ("plancklength", planckLength)

  -- alternate spellings
  , ("meter", metre)
  , ("liter", litre)
  ]

metricPrefixes :: Map String (Unit -> Unit)
metricPrefixes = M.fromList $
  [ ("yotta", yotta)
  , ("zetta", zetta)
  , ("exa",   exa)
  , ("peta",  peta)
  , ("tera",  tera)
  , ("giga",  giga)
  , ("mega",  mega)
  , ("kilo",  kilo)
  , ("hecto", hecto)
  , ("deca",  deca)
  , ("deci",  deci)
  , ("centi", centi)
  , ("milli", milli)
  , ("micro", micro)
  , ("nano",  nano)
  , ("pico",  pico)
  , ("femto", femto)
  , ("atto",  atto)
  , ("zepto", zepto)
  , ("yocto", yocto)
  ]

pluralUnits :: Map String Unit
pluralUnits = M.union atypicalPlurals plurals
  where

    atypicalPlurals = M.fromList [ ("feet", foot), ("grays", gray) ]
    falsePlurals = ["foots", "graies"]

    plurals = M.fromList . map (\(k,v) -> (pluralize k,v)) . M.toList $
      foldl (flip M.delete) baseUnits falsePlurals

    pluralize s | "us" `isSuffixOf` s = init (init s) ++ "i"
                | any (flip isSuffixOf s) ["s", "x", "z", "ch"] = s ++ "es"
                | "y"  `isSuffixOf` s = init s ++ "ies"
                | "um" `isSuffixOf` s = init (init s) ++ "a"
                | otherwise = s ++ "s"

units :: Map String Unit
units = M.union baseUnits pluralUnits

unit :: Dim -> Unit
unit d = U $ M.fromList [(D $ M.fromList [(d, 1)], 1)]
