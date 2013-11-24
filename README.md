# unitparty #

algebraic unit converter.

## usage ##
```shell
$ unitparty --from fahrenheit \
            --to celsius \
            --amount 82    # boring unit conversions
82.0 fahrenheit = 27.77777777777783 celsius

$ unitparty --from earth \
            --to marses    # less boring unit conversions
1.0 earth = 9.345794392523365 marses

$ unitparty --from shekels/coulomb \
            --to kilotesla*fortnights \
            --amount 22.3  # arbitrary algebraic units
22.3 shekels/coulomb = 2.1503210191326532e-10 kilotesla*fortnights

$ unitparty --from kilometers --to pounds/inch^2 # knows what's what
Incommensurable units: m, kg*m^-2

$ unitparty --analyze "weber * calories^3 / lightyear"
weber * calories^3 / lightyear: kg^4*m^7*s^-8*A^-1

$ unitparty --dyk   # FUN FACTS. UNIT PARTY.
Did you know ...
  1.0 meter = 6.684587122268445e-12 astronomicalunits
```

