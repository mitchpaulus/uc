module Main where

import Lib
import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    let numArgs = length args
    let result = if any isHelpParameter args then helpText
                 else if numArgs /= 3 then "3 arguments required. Received " ++ (show numArgs) ++ "."
                 else do
                    let value = read (args !! 0)
                    let originalUnit = args !! 1
                    let newUnit = args !! 2

                    runConversion value originalUnit newUnit

    putStrLn result

isHelpParameter :: String -> Bool
isHelpParameter param = param == "-h" || param == "--help"

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

data UnitType = Temperature | Area | Length | Power | Energy | PowerPerArea deriving (Enum, Eq, Show)

data Unit = Unit { variable :: String, name :: String, factor :: Double, bias :: Double, unitType :: UnitType  }

instance Eq Unit where (==) unit1 unit2 = (variable unit1) == (variable unit2)

instance Show Unit where show unit = (variable unit) ++ ": " ++ (name unit)

-- Base Units:
-- Temperature: F

fahrenheit = Unit "F" "Fahrenheit" 1 0 Temperature
celcius = Unit "C" "Celcius" (9/5) 32 Temperature
kelvin = Unit "K" "Kelvin" (factor celcius) ((bias celcius) - (factor celcius)*273.15) Temperature

-- Area: m²


-- Length: m
feetPerMeter = 10000 / 3048
inchesPerFoot = 12

-- PowerPerArea: W/ft²
-- Power: W
-- Energy: kBtu

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

    energyUnit "ton-hrs" "Ton hours" 12
 ]

energyUnit unit name factor = Unit unit name factor 0 Energy

unitToKey unit = (variable unit, unit)
unitMap = Map.fromList (map unitToKey units)

allUnits :: [String]
allUnits = map show units

helpText = unlines $ [
    "uc - unit converter",
    "",
    "USAGE:",
    "  uc val fromUnit toUnit",
    "",
    "Examples:",
    "",
    "70°F to °C",
    " uc 70 F C",
    "",
    "Available Units:",
    ""
 ] ++ allUnits
