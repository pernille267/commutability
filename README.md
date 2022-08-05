# commutability
An R package with C++ back-end for the purpose of commutability evaluation. Tools for differences in non-selectivity may also be found here. Are you you lazy, or do you not wish to do data analysis on a commutability study?
Then, this package is for you. This package includes high-performance and user-friendly functions doing most of the analysis for you. The newest resarch on external quality assessment commutability included in this package. If your R skills are non-existing,
you can use the even more user-friendly app named "Commutability evaluation of external quality assessment" found at http://qualitylife.shinyapps.io/Commutability-evaluation. The following functions is included in the commutability package:
  - **check_data()**: Checks whether input data is on the correct form
  - **repair_data()**: Repairs data so that it is on the correct form
  - **MS_wise()**: Converts a data on wide format to the equivalent data on long format keyed by unique IVD-MD comparisons, **comparison**
  - **MS_wise_imprecision()** Modifies the layout of the output of **estimate_imprecision_data** so that each IVD-MD's imprecision estimates will be given in one row.
  - **estimate_zeta_data()**: Estimates bootstrap confidence intervals and more concerning the difference in non-selectivity measure $\zeta$
  - **estimate_imprecison_data()**: Estimates bootstrap confidence intervals and more concerning the imprecision estimate
  - **estimate_prediction_data()**: Estimates prediction intervals and prediction bands based on clinical sample data and potentially EQAM data
  - **do_commutability_evaluation()**: Combines the previous three functions for repaired and NA-free clinical sample data and EQAM data to get summary results for the data analysis regarding the commutability evaluation experiment
  - **transform_data()**: Does simple transformations to the numeric columns of clinical sample data or EQAM data such as log and square root.
  
# Future functions

The following functions are required for the package to be complete:

  - **perform_assessment_tests()**: Normality and heteroscedasticity tests regarding the validness of regression models with test strengths estimated by bootstrap
  - **plot_assessment_plots()**: Plots model assessment plots for visiualization to be used in addition to the formal tests from **perform_assessment_tests()**. The plots are constructed by the **ggplot()** package.
  - **plot_commutability_evaluation_plots()** Plot standard commutability evaluation plots using popular **ggplot()** package.

# Installation instructions

The following packages **must** be installed before installing *commutability*:

  - devtools
  ```{r}
  install.packages("devtools")
  ```
  - fasteqa
  - fasteqa may be installed after installing devtools using
  ```{r}
  devtools::install_github("pernille267/fasteqa")  
  ```
  
The remaining required packages will be requested automatically by Rstudio if you do not have them installed already.

# Why not on CRAN

I am planning to get this package (**commutability**) and **fasteqa** on CRAN, but for the moment the packages is not robust enough to do so. In addition, I am not yet skilled enough to do so. But, the goal is to get them on CRAN as soon as possible. Please feel free so suggest enhancements to either package by sending me an email on pernille.fauskanger@noklus.no. 
