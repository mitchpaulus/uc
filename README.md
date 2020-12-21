# `uc` - Unit Conversions from the Shell!

`uc` is a fun little utility that lets you do a quick unit conversion
on the fly. As an engineer, I find this quite useful to have at my
fingertips in the terminal. (Note: this was a small project to try out Haskell - in 
reality I typically use the [GNU `units`](https://www.gnu.org/software/units/) utility, which is
far more expansive and battle tested than this). 

The basic usage is:

`uc value fromUnit toUnit`

and it works like:

```
$ uc 10 m2 ft2
107.63910416709722
```

## Tab Completion

Tab completion for the units is available in bash. You just need to
source the line

```sh
eval "$(uc -c)"
```

somewhere in your `.bashrc` file.

## Available Units

Argument | Unit Name
------|-------
`F` | Fahrenheit
`C` | Celsius
`K` | Kelvin
`R` | Rankine
`m2` | Square meters
`ft2` | Square feet
`in2` | Square inches
`km2` | Square kilometers
`m` | Meters
`ft` | Feet
`kBtu` | Thousand BTU
`MMBtu` | Million BTU
`kWh` | Kilowatt hour
`MWh` | Megawatt hour
`GJ` | Gigajoules
`ng_ccf` | Hundred cubic feet natural gas
`ng_kcf` | Thousand cubic feet natural gas
`ng_mcf` | Million cubic feet natural gas
`ng_m3` | Cubic meters natural gas
`Therms` | Therms
`steam_lb` | lbs of steam
`steam_klb` | Thousand lbs of steam
`steam_Mlb` | Million lbs of steam
`steam_kg` | kg of steam
`ton-hrs` | Ton hours
`m3/s` | Cubic meters per second
`gpm` | Gallons per minute
`cfm` | Cubic feet per minute
`W` | Watt
`kw` | Kilowatt
`MW` | Megawatt
`Btu/h` | BTU per hour
`kBtu/h` | Thousand BTU per hour
`ton` | Tons
`hp` | Horsepower
`kJ/kg` | Kilojoules per Kilogram
`Btu/lbm` | BTU per pound mass

