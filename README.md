# Outlier and contamination effects on parameter variability

For the MSc Econometrics & Management Science, specializing in Business Analytics & Quantitative Marketing [BAQM], at the Erasmus School of Economics [ESE] in Rotterdam, I have written a short research project for the course "Multivariate Statistics" concerning the effects of outliers or contamination of error terms by heavy-tailed distributions on the variability of parameter estimates. In particular, I have performed a simulation study, in which I applied several robust generalized linear models (Mallows-type M-estimator and MM-estimator) and a benchmark OLS-regression on a data set with either outliers in the terms of bad leverage points, or contamination on errors with the t-distribution. Should outliers and contamination have significant effects on the variances of the estimates, then the values of often used test statistics such as t− and Wald-statistics, the corresponding significance levels of the estimates, and ultimate conclusions drawn based on these outcomes can change drastically. Hence, circumventing such problems is of interest, since it is crucial for actors such as policymakers, organizations and scientists to minimize the probability of drawing incorrect conclusions and/or making undesirable policy decisions.

# Replication files

This folder contains files for as well as a general introduction for the reader:
- Replication results.R: this file produces the boxplots in Figure 1-9 and estimation results in Table 1 for uncontaminated data, contaminated data with the t-distribution, and data with bad leverage outliers.
- Outlier and contamination effects on parameter variability.pdf: This file contains the written assignment.
