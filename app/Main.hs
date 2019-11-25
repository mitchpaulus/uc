module Main where

import Lib
import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let numArgs = length args
    let result = if any isHelpParameter args then helpText
                 else if any isPrintOption args then printOptions
                 else if any isCompleteOption args then printCompletion
                 else if any isMarkdownTableOption args then markdownTable
                 else if numArgs /= 3 then "3 arguments required. Received " ++ (show numArgs) ++ "."
                 else do
                    let value = read (args !! 0)
                    let originalUnit = args !! 1
                    let newUnit = args !! 2

                    runConversion value originalUnit newUnit

    putStrLn result

isPrintOption :: String -> Bool
isPrintOption option = option == "-p" || option == "--print"

isCompleteOption :: String -> Bool
isCompleteOption option = option == "-c" || option == "--complete"

isHelpParameter :: String -> Bool
isHelpParameter param = param == "-h" || param == "--help"

isMarkdownTableOption :: String -> Bool
isMarkdownTableOption option = option == "-m" || option == "--markdown"


runConversion :: Double -> String -> String -> String
runConversion value originalUnit newUnit
        | (firstUnit == Nothing) && (secondUnit == Nothing) = "Could not match either unit " ++ originalUnit ++ " or " ++ newUnit
        | firstUnit == Nothing = "Could not match unit " ++ originalUnit
        | secondUnit == Nothing = "Could not match unit " ++ newUnit
        | otherwise = convert value (fromJust firstUnit) (fromJust secondUnit)

        where firstUnit = lookupUnit originalUnit
              secondUnit = lookupUnit newUnit

convert :: Double -> Unit ->  Unit -> String
convert x unit1 unit2
    | (unitType unit1) == (unitType unit2) = show $ (x * (factor unit1) + (bias unit1) - (bias unit2)) / (factor unit2)
    | otherwise = incompatibleTypingsError unit1 unit2


incompatibleTypingsError unit1 unit2 = "Units have incompatible types. " ++ (show $ unitType unit1) ++ " " ++ (show $ unitType unit2)

lookupUnit :: String -> Maybe Unit
lookupUnit unit = Map.lookup unit unitMap

data UnitType =
 Temperature |
 Area |
 Length |
 Power |
 Energy |
 PowerPerArea |
 VolumetricFlow |
 Enthalpy
 deriving (Enum, Eq, Show)

data Unit = Unit { variable :: String, name :: String, factor :: Double, bias :: Double, unitType :: UnitType  }

instance Eq Unit where (==) unit1 unit2 = (variable unit1) == (variable unit2)

instance Show Unit where show unit = (variable unit) ++ ": " ++ (name unit)

-- Base Units:
-- Temperature: F

fahrenheit = Unit "F" "Fahrenheit" 1 0 Temperature
celcius = Unit "C" "Celsius" (9/5) 32 Temperature
kelvin = Unit "K" "Kelvin" (factor celcius) ((bias celcius) - (factor celcius)*273.15) Temperature

-- Area: m²


-- Length: m
feetPerMeter = 10000 / 3048

inchesPerFoot = 12

-- PowerPerArea: W/ft²
-- Power: W
wattsPerBtuh = 1 / 3.41214
-- Energy: kBtu

-- VolumetricFlow: m3/s

-- Enthalpy: kJ/kg
kj_kgPerBtu_lbm = 2.326


units :: [Unit]
units =
 [
    fahrenheit,
    celcius,
    kelvin,
    Unit "R" "Rankine" 1 459.67 Temperature,
    Unit "m2" "Square meters" 1 0 Area,
    Unit "ft2" "Square feet" (145161 / 1562500) 0 Area,
    Unit "in2" "Square inches" (1 / (feetPerMeter*feetPerMeter*inchesPerFoot*inchesPerFoot)) 0 Area,
    Unit "km2" "Square kilometers" (1000000) 0 Area,
    Unit "m" "Meters" 1 0 Length,
    Unit "ft" "Feet" (1/feetPerMeter) 0 Length,

    -- These energy unit conversions come from the EnergyStar Portfolio Manager
    energyUnit "kBtu" "Thousand BTU" 1,
    energyUnit "MMBtu" "Million BTU" 1000,
    energyUnit "kWh" "Kilowatt hour" 3.412,
    energyUnit "MWh" "Megawatt hour" 3412,
    energyUnit "GJ" "Gigajoules" 947.817,

    energyUnit "ng_ccf" "Hundred cubic feet natural gas" 102.6,
    energyUnit "ng_kcf" "Thousand cubic feet natural gas" 1026,
    energyUnit "ng_mcf" "Million cubic feet natural gas" 1026000,
    energyUnit "ng_m3" "Cubic meters natural gas" 36.303,
    energyUnit "Therms" "Therms" 100,

    energyUnit "steam_lb" "lbs of steam" 1.194,
    energyUnit "steam_klb" "Thousand lbs of steam" 1194,
    energyUnit "steam_Mlb" "Million lbs of steam" 1194000,
    energyUnit "steam_kg" "kg of steam" 2.632,

    energyUnit "ton-hrs" "Ton hours" 12,

    volumetricFlow "m3/s" "Cubic meters per second" 1,
    volumetricFlow "gpm" "Gallons per minute" (1/15850.372483753),
    volumetricFlow "cfm" "Cubic feet per minute" (1/feetPerMeter/feetPerMeter/feetPerMeter/60),

    powerUnit "W" "Watt" 1,
    powerUnit "kW" "Kilowatt" (1000),
    powerUnit "MW" "Megawatt" (100000),

    powerUnit "Btu/h" "BTU per hour" wattsPerBtuh,
    powerUnit "kBtu/h" "Thousand BTU per hour" (wattsPerBtuh * 1000),
    powerUnit "ton" "Tons" (wattsPerBtuh * 12000),
    powerUnit "hp" "Horsepower" (2545 * wattsPerBtuh),

    enthalpyUnit "kJ/kg" "Kilojoules per Kilogram" 1,
    enthalpyUnit "Btu/lbm" "BTU per pound mass" kj_kgPerBtu_lbm

 ]

energyUnit unit name factor = Unit unit name factor 0 Energy
volumetricFlow unit name factor = Unit unit name factor 0 VolumetricFlow
powerUnit unit name factor = Unit unit name factor 0 Power
enthalpyUnit unit name factor = Unit unit name factor 0 Enthalpy

unitToKey unit = (variable unit, unit)
unitMap = Map.fromList (map unitToKey units)

allUnits :: [String]
allUnits = map show units

allVariables = map variable units

-- Prints a simple list of the available units. Used
-- to help build word list for completion
printOptions :: String
printOptions = unlines $ map variable units

maxUnitVariableNameLength :: Int
maxUnitVariableNameLength = maximum $ map length allVariables

printMarkdownRow :: Unit -> String
printMarkdownRow unit = printf "`%s` | %s" (variable unit) (name unit)

markdownTable :: String
markdownTable = unlines $ (("Argument | Unit Name" ) : ("------|-------") : map printMarkdownRow units)

-- This prints the bash command to add completions. Just
-- need to run the command wrapped in an 'eval'
printCompletion = "complete -W \"$(uc -p)\" uc"

helpText = unlines $ [
    "uc - unit converter",
    "",
    "USAGE:",
    "  uc val fromUnit toUnit",
    "  uc [-h | --help]",
    "  uc [-p | --print]",
    "  uc [-c | --completion]",
    "",
    "OPTIONS:",
    "",
    " -h, --help         Show this help and exit",
    " -h, --print        Print list of available units",
    " -c, --completion   Print bash completion command. Run as: eval \"$(uc -c)\"",
    "",
    "Examples:",
    "",
    "70°F to °C",
    " uc 70 F C",
    "",
    "Available Units:",
    ""
 ] ++ allUnits
