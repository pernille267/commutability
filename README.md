# commutability
An R package with C++ back-end for the purpose of commutability evaluation. Tools for differences in non-selectivity may also be found here. Are you you lazy, or do you not wish to do data analysis on a commutability study?
Then, this package is for you. This package includes high-performance and user-friendly functions doing most of the analysis for you. The newest resarch on external quality assessment commutability included in this package. If your R skills are non-existing,
you can use the even more user-friendly app named "Commutability evaluation of external quality assessment" found at http://qualitylife.shinyapps.io/Commutability-evaluation. The following functions is included in the commutability package:
  - **check_data()**: Checks whether input data is on the correct form
  - **repair_data()**: Repairs data so that it is on the correct form
  - **MS_wise()**: Converts a data on wide format to the equivalent data on long format keyed by unique IVD-MD comparisons, **comparison**
  - **estimate_zeta_data()**: Estimates bootstrap confidence intervals and more concerning the difference in non-selectivity measure $\zeta$
  - **estimate_imprecison_data()**: Estimates bootstrap confidence intervals and more concerning the imprecision estimate

# Installation instructions

The following packages **must** be installed before installing *commutability*:

  - devtools
  - fasteqa (install by using devtools)

The remaining required packages will be requested if you do not have them installed already.
