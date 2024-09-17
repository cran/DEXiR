# DEXiR

'DEXiR' is a software package for using DEXi models in 'R'.

DEXi models are hierarchical qualitative multi-criteria decision models developed according to the
method DEX ([Decision EXpert](https://en.wikipedia.org/wiki/Decision_EXpert)),
using the program [DEXi](https://kt.ijs.si/MarkoBohanec/dexi.html) or
[DEXiWin](https://dex.ijs.si/dexisuite/dexiwin.html).

A typical workflow with 'DEXiR' consists of:
1. reading a .dxi file, previously made using the DEXi software (function read_dexi()),
2. creating one or more data frames containing input values of decision alternatives,
3. evaluating those alternatives (function evaluate()).
4. analyzing alternatives (selective_explanation(), plus_minus(), compare_alternatives()),
5. making charts.


'DEXiR' is restricted to using models produced externally by the 'DEXi' software and does not
provide functionality for creating and/or editing DEXi models directly in 'R'.

## Current version

1.0.1, initial version, being submitted to CRAN.
Bug reports and suggestions are appreciated.

## Installation

```{R}
# From files in dexir/packages/:

install.packages("dexir/packages/DEXiR_1.0.1.tar.gz", repos = NULL, type = "source")

# or

install.packages("dexir/packages/DEXiR_1.0.1.zip", repos = NULL)
```

## License
[MIT](https://choosealicense.com/licenses/mit/)
