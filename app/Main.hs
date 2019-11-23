module Main where

import Lib
import System.Environment

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


isHelpParameter param = param == "-h" || param == "--help"

runConversion value originalUnit newUnit =
        case convertUnit value originalUnit newUnit of
                        Just convertedValue -> show convertedValue
                        Nothing             -> "No conversion from " ++ originalUnit ++ " to " ++ newUnit



convertUnit :: Double -> String -> String -> Maybe Double
convertUnit val "C" "F" = Just (val * 9 / 5 + 32)
convertUnit val "F" "C" = Just ((val - 32) * 5 / 9)
convertUnit val "m2" "ft2" = Just (val * 1562500 / 145161)
convertUnit val "ft2" "m2" = Just (val * 145161 / 1562500)
convertUnit val "W/ft2" "W/m2" = Just (val * 1562500 / 145161)
convertUnit val "W/m2" "W/ft2" = Just (val * 145161 / 1562500)
convertUnit _ _ _ = Nothing

data UnitTypes = Temperature | Area | PowerPerArea

data Unit = Unit { variable :: String, name :: String, factor :: Double, bias :: Double }
    deriving Show

-- Base Units:
-- Temperature: F
-- Area: m2


units :: [Unit]
units =
 [
    Unit "F" "Fahrenheit" 1 0,
    Unit "C" "Celcius" (9/5) 32
 ]


helpText = unlines [
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
            ]




