#==============================================================================#
# Lab 1: Invariance & Reliability for Cross-Sectional Data
#==============================================================================#

### Packages required for Practical --------------------------------------------
#install.packages("semTools")
library(semTools)

#Trick if something fails:
#detach("package:semTools", unload = TRUE)
#detach("package:lavaan", unload = TRUE)
#library(semTools)


### Data required for Practical-------------------------------------------------
#load the workspace Workspace_IOPS.RData. The dataset is Data_Cross_Panel.
load("Workspace_IOPS.RData")

### The Factor Model for Cross-Sectional Data-----------------------------------
# note: for an explanation of the model syntax (including the different symbols), 
# see https://lavaan.ugent.be/tutorial/cfa.html
# quick summary:
# Regression of Y on X:                Y ~X
# Variance of Y specified as:          Y ~~Y
# Covariance between X and Y:          X ~~ Y
# Intercept given by:                  X ~1
# Loading of item on factor            F1 =~ X

CM_factor_T1 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_01 +lambda1*T1_CM_01 + T1_CM_02 + T1_CM_03 + T1_CM_04 + 
        T1_CM_05 + T1_CM_06 + T1_CM_07 + T1_CM_08

# Intercepts
T1_CM_01 ~ i1*1
T1_CM_02 ~ 1
T1_CM_03 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

# Unique Variances
T1_CM_01 ~~ T1_CM_01
T1_CM_02 ~~ T1_CM_02
T1_CM_03 ~~ T1_CM_03
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

# Latent Variable Means
CM_1 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
'

# Model fitting
fit_CM_T1 <- cfa(CM_factor_T1, data = Data_Cross_Panel, mimic = "mplus")

# Check the model fit and decide if the model fits well:
# Look at CFI (should be > .90), RMSEA (should be < .08), 
# SRMR (should be < .08), and chi-square (should be non-significant)
#
# The CFI (Comparative Fit Index), RMSEA (Root Mean Squared Error of 
# Approximation), and SRMR (Standardized Root Mean Squared Error) are 
# Approximate Fit Indices. They do not correct for sampling variability,
# have no clear cut-offs for good fit, and assume multivariate normal data.
# They are more qualitative measures of fit.
#
# CFI:   Looks at the improvement in fit relative to the independence model 
#        which states that every variable has a mean and variance, but all are 
#        unrelated (correlations are all 0).
#
# RMSEA: Tests for close fit between data (observed (co)variances) 
#        and model implied relations.
#
# SRMR:  Looks at the (mean) covariance residuals.   
#
# The chi-square test is an actual test with corresponding null-hypothesis.
# It tests for the null-hypothesis of perfect fit (in the population), that is,
# the model perfectly recovers the observed (co)variances, which
# might be a little unrealistic. It is (unnecessarily) strict (especially for 
# large sample size), assumes multivariate normality, and assumes a random 
# sample. Moreover, it doesn't work well in case there are high correlations 
# and/or high percentages of unique variance. 
# It also tends to miss misfit in single parameters (e.g., a single 
# large covariance residual) and patterns of smaller misfit (many small 
# residuals), AND a non-significant result does NOT mean a good model. 


summary(fit_CM_T1, fit.measures = TRUE)

### Check for Invariance in 4 steps for Cross-Sectional Data--------------------

#### Configural invariance------------------------------------------------------

# Generate code for configural invariance between groups
mod.configural  <- semTools::measEq.syntax(configural.model= CM_factor_T1, 
                                         data = Data_Cross_Panel,
                                         group ="Gender", mimic = "mplus")

# Inspect the generated code
cat(as.character(mod.configural))

# Fit the configural invariance model
fit.configural <- cfa(as.character(mod.configural), data = Data_Cross_Panel, 
                      group ="Gender", mimic = "mplus")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural, fit.measures = TRUE)


#### Invariance of Loadings-----------------------------------------------------

# Generate code for invariance of loadings
mod.loadings  <- semTools::measEq.syntax(configural.model= CM_factor_T1, 
                                           data = Data_Cross_Panel,
                                           group.equal = c("loadings"),
                                           group ="Gender", mimic = "mplus")

# Inspect the generated code
cat(as.character(mod.loadings))

# Fit the loading invariance model
fit.loadings <- cfa(as.character(mod.loadings), data = Data_Cross_Panel, 
                    group ="Gender", mimic = "mplus")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.loadings, fit.measures = TRUE)

#### Invariance of Intercepts & Loadings----------------------------------------

# Generate code for invariance of intercepts and loadings
mod.int.load <- semTools::measEq.syntax(configural.model= CM_factor_T1, 
                                           data = Data_Cross_Panel,
                                           group.equal = c("intercepts", 
                                                           "loadings"),
                                           group ="Gender", mimic = "mplus")

# Inspect the generated code
cat(as.character(mod.int.load))

# Fit the intercept and loading invariance model
fit.int.load <- cfa(as.character(mod.int.load), data = Data_Cross_Panel, 
                    group ="Gender", mimic = "mplus")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.int.load, fit.measures = TRUE)


# Compare the different models successively, using the guidelines below.
# >20 observations per group:
# -.01 change in CFI, paired with changes in RMSEA of .015 and SRMR of .030 
# (loading invariance) or .015 (intercept invariance). Difference in 
# Chi-square should be non-significant
# 10-20 observations per group:
# CFI -.02 and RMSEA of .03  most appropriate for tests of loading invariance 
# with large group sizes, traditional criteria of -.01 for CFI 
# and .01 for RMSEA were appropriate for intercept invariance tests.
# Difference in Chi-square should be non-significant
# <10 observations per group: invariance testing is not a good idea.

Check.Load <- semTools::compareFit(fit.configural, fit.loadings) 
summary(Check.Load) # Good

Check.Int <- semTools::compareFit(fit.loadings, fit.int.load)
summary(Check.Int) # Not all good, so find intercept with largest diff between
# groups

# Let's check where the largest differences are to improve the model fit
# Difference in groups in intercepts
parameterEstimates(fit.loadings)[9:16,7] - 
  parameterEstimates(fit.loadings)[35:42,7]

which(
  (parameterEstimates(fit.loadings)[9:16,7] - 
     parameterEstimates(fit.loadings)[35:42,7]) 
  == min(parameterEstimates(fit.loadings)[9:16,7] - 
           parameterEstimates(fit.loadings)[35:42,7]))

# Largest difference in intercept of item 5
mod.intercepts.part <- semTools::measEq.syntax(configural.model= CM_factor_T1, 
                                          data = Data_Cross_Panel,
                                          group.equal = c("loadings",
                                                          "intercepts"),
                                          group.partial = c("T1_CM_05 ~ 1"),
                                          group ="Gender", mimic = "mplus")

# Inspect the generated code
cat(as.character(mod.intercepts.part))

# Fit the intercept invariance model
fit.intercepts.part <- cfa(as.character(mod.intercepts.part), data = Data_Cross_Panel, 
                      group ="Gender", mimic = "mplus")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.intercepts.part, fit.measures = TRUE)

Check.Int.Part <- semTools::compareFit(fit.loadings, fit.intercepts.part)
summary(Check.Int.Part) # Good

# Calculate Reliability for the final model
compRelSEM(fit.intercepts.part)

#==============================================================================#
# Lab 2: Reliability & Invariance for Longitudinal Data
#==============================================================================#

### Packages required for Practical --------------------------------------------
#install.packages("semTools")
library(semTools)

### Data required for Practical-------------------------------------------------
#load the workspace Workspace_IOPS.RData. Now you need both Data_Cross_Panel
#and Data_ILD.

### The Factor Model for Panel Data---------------------------------------------

LI_CM <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_01 + 1*T1_CM_01 + T1_CM_02 + T1_CM_03 + T1_CM_04 + T1_CM_05 
+ T1_CM_06 + T1_CM_07 + T1_CM_08

CM_2 =~ NA*T2_CM_01 + 1*T2_CM_01 + T2_CM_02 + T2_CM_03 + T2_CM_04 + T2_CM_05 
+ T2_CM_06 + T2_CM_07 + T2_CM_08

CM_3 =~ NA*T3_CM_01 + 1*T3_CM_01 + T3_CM_02 + T3_CM_03 + T3_CM_04 + T3_CM_05 
+ T3_CM_06 + T3_CM_07 + T3_CM_08

# Intercepts
T1_CM_01 ~ 1
T1_CM_02 ~ 1
T1_CM_03 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_01 ~ 1
T2_CM_02 ~ 1
T2_CM_03 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_06 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_01 ~ 1
T3_CM_02 ~ 1
T3_CM_03 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_06 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_01 ~~ T1_CM_01
T1_CM_02 ~~ T1_CM_02
T1_CM_03 ~~ T1_CM_03
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_01 ~~ T2_CM_01
T2_CM_02 ~~ T2_CM_02
T2_CM_03 ~~ T2_CM_03
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_06 ~~ T2_CM_06
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_01 ~~ T3_CM_01
T3_CM_02 ~~ T3_CM_02
T3_CM_03 ~~ T3_CM_03
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_06 ~~ T3_CM_06
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3
'

#Model fitting
fit_CM_Long <- cfa(LI_CM, data = Data_Cross_Panel, mimic = "mplus")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit_CM_Long, fit.measures = TRUE)


### Check for Invariance in 4 steps for Panel Data------------------------------

#### Configural invariance------------------------------------------------------

# Generate code for configural invariance across time

longFacNames <- list(CM = c("CM_1","CM_2","CM_3"))

mod.configural.long <-semTools::measEq.syntax(LI_CM, 
                                     longFacNames = longFacNames,
                                     data=Data_Cross_Panel, missing="fiml")
# now we use a new argument longFacNames. For details, see ?measEq.syntax

# check model code
cat(as.character(mod.configural.long))


fit.configural.long <- lavaan::cfa(as.character(mod.configural.long), 
                              data = Data_Cross_Panel, 
                              fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long, fit.measures=T) # Fit is not great, this already
# shows that at one of the three time-points the model fits less well.

# Lets figure out where using modification indices
MIs <- modindices(fit.configural.long)
head(MIs[order(MIs$mi, decreasing = TRUE), 1:4], n = 20)
# At T3 items 2 and 5, and 1 and 3 are pretty correlated
# At T2 items 1 and 2, and 3 and 6 are pretty correlated
# At T1 items 4 and 5, and 3 and 6 are pretty correlated


LI_CM2 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_01 + 1*T1_CM_01 + T1_CM_02 + T1_CM_03 + T1_CM_04 + T1_CM_05 + T1_CM_06 + T1_CM_07 + T1_CM_08
CM_2 =~ NA*T2_CM_01 + 1*T2_CM_01 + T2_CM_02 + T2_CM_03 + T2_CM_04 + T2_CM_05 + T2_CM_06 + T2_CM_07 + T2_CM_08
CM_3 =~ NA*T3_CM_01 + 1*T3_CM_01 + T3_CM_02 + T3_CM_03 + T3_CM_04 + T3_CM_05 + T3_CM_06 + T3_CM_07 + T3_CM_08

# Intercepts
T1_CM_01 ~ 1
T1_CM_02 ~ 1
T1_CM_03 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_01 ~ 1
T2_CM_02 ~ 1
T2_CM_03 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_06 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_01 ~ 1
T3_CM_02 ~ 1
T3_CM_03 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_06 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_01 ~~ T1_CM_01
T1_CM_02 ~~ T1_CM_02
T1_CM_03 ~~ T1_CM_03
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_01 ~~ T2_CM_01
T2_CM_02 ~~ T2_CM_02
T2_CM_03 ~~ T2_CM_03
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_06 ~~ T2_CM_06
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_01 ~~ T3_CM_01
T3_CM_02 ~~ T3_CM_02
T3_CM_03 ~~ T3_CM_03
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_06 ~~ T3_CM_06
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3


# Residual Covariances 
T1_CM_03 ~~ T1_CM_06
T2_CM_03 ~~ T2_CM_06
T3_CM_03 ~~ T3_CM_06

'

mod.configural.long2 <-semTools::measEq.syntax(LI_CM2, 
                                              longFacNames = longFacNames,
                                              data=Data_Cross_Panel, missing="fiml")
cat(as.character(mod.configural.long2))


fit.configural.long2 <- lavaan::cfa(as.character(mod.configural.long2), 
                              data = Data_Cross_Panel, 
                              fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long2, fit.measures=T) # Fit still not good

MIs2 <- modindices(fit.configural.long2)
head(MIs2[order(MIs2$mi, decreasing = TRUE), 1:4], n = 20)

# T3: Items 1 and 3, and 3 and 5, and 1 and 2
# T2: Items 1 and 2, and 2 and 6, and 2 and 3
# T1: Items 4 and 5, 

LI_CM3 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_01 + 1*T1_CM_01 + T1_CM_02 + T1_CM_03 + T1_CM_04 + T1_CM_05 
+ T1_CM_06 + T1_CM_07 + T1_CM_08
CM_2 =~ NA*T2_CM_01 + 1*T2_CM_01 + T2_CM_02 + T2_CM_03 + T2_CM_04 + T2_CM_05 
+ T2_CM_06 + T2_CM_07 + T2_CM_08
CM_3 =~ NA*T3_CM_01 + 1*T3_CM_01 + T3_CM_02 + T3_CM_03 + T3_CM_04 + T3_CM_05 
+ T3_CM_06 + T3_CM_07 + T3_CM_08

# Intercepts
T1_CM_01 ~ 1
T1_CM_02 ~ 1
T1_CM_03 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_01 ~ 1
T2_CM_02 ~ 1
T2_CM_03 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_06 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_01 ~ 1
T3_CM_02 ~ 1
T3_CM_03 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_06 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_01 ~~ T1_CM_01
T1_CM_02 ~~ T1_CM_02
T1_CM_03 ~~ T1_CM_03
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_01 ~~ T2_CM_01
T2_CM_02 ~~ T2_CM_02
T2_CM_03 ~~ T2_CM_03
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_06 ~~ T2_CM_06
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_01 ~~ T3_CM_01
T3_CM_02 ~~ T3_CM_02
T3_CM_03 ~~ T3_CM_03
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_06 ~~ T3_CM_06
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3


# Residual Covariances 
T1_CM_03 ~~ T1_CM_06
T2_CM_03 ~~ T2_CM_06
T3_CM_03 ~~ T3_CM_06

T1_CM_01 ~~ T1_CM_02
T2_CM_01 ~~ T2_CM_02
T3_CM_01 ~~ T3_CM_02

'

mod.configural.long3 <-semTools::measEq.syntax(LI_CM3, 
                                               longFacNames = longFacNames,
                                               data=Data_Cross_Panel, missing="fiml")
cat(as.character(mod.configural.long3))


fit.configural.long3 <- lavaan::cfa(as.character(mod.configural.long3), 
                               data = Data_Cross_Panel, 
                               fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long3, fit.measures=T)  # Fit still isn't great, 
#even after adding an additional set of correlations. 
# Always item 3, 1 and 6 that are causing the issue. Let's remove them one at 
#a time.

LI_CM4 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_01 + 1*T1_CM_01 + T1_CM_02 + T1_CM_04 + T1_CM_05 + T1_CM_06 
+ T1_CM_07 + T1_CM_08
CM_2 =~ NA*T2_CM_01 + 1*T2_CM_01 + T2_CM_02 + T2_CM_04 + T2_CM_05 + T2_CM_06 
+ T2_CM_07 + T2_CM_08
CM_3 =~ NA*T3_CM_01 + 1*T3_CM_01 + T3_CM_02 + T3_CM_04 + T3_CM_05 + T3_CM_06 
+ T3_CM_07 + T3_CM_08

# Intercepts
T1_CM_01 ~ 1
T1_CM_02 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_01 ~ 1
T2_CM_02 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_06 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_01 ~ 1
T3_CM_02 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_06 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_01 ~~ T1_CM_01
T1_CM_02 ~~ T1_CM_02
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_01 ~~ T2_CM_01
T2_CM_02 ~~ T2_CM_02
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_06 ~~ T2_CM_06
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_01 ~~ T3_CM_01
T3_CM_02 ~~ T3_CM_02
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_06 ~~ T3_CM_06
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3
'

mod.configural.long4 <-semTools::measEq.syntax(LI_CM4, 
                                               longFacNames = longFacNames,
                                               data=Data_Cross_Panel, missing="fiml")
cat(as.character(mod.configural.long4))


fit.configural.long4 <- lavaan::cfa(as.character(mod.configural.long4), 
                               data = Data_Cross_Panel, 
                               fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long4, fit.measures=T) # Still not good


LI_CM5 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_02 + T1_CM_02 + T1_CM_04 + T1_CM_05 + T1_CM_06 + T1_CM_07 
+ T1_CM_08
CM_2 =~ NA*T2_CM_02 + T2_CM_02 + T2_CM_04 + T2_CM_05 + T2_CM_06 + T2_CM_07 
+ T2_CM_08
CM_3 =~ NA*T3_CM_02 + T3_CM_02 + T3_CM_04 + T3_CM_05 + T3_CM_06 + T3_CM_07 
+ T3_CM_08

# Intercepts
T1_CM_02 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_06 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_02 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_06 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_02 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_06 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_02 ~~ T1_CM_02
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_06 ~~ T1_CM_06
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_02 ~~ T2_CM_02
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_06 ~~ T2_CM_06
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_02 ~~ T3_CM_02
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_06 ~~ T3_CM_06
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3
'

mod.configural.long5 <-semTools::measEq.syntax(LI_CM5, 
                                               longFacNames = longFacNames,
                                               data=Data_Cross_Panel, missing="fiml")
cat(as.character(mod.configural.long5))


fit.configural.long5 <- lavaan::cfa(as.character(mod.configural.long5), 
                               data = Data_Cross_Panel, 
                               fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long5, fit.measures=T) # Almost


LI_CM6 <- '

# Define the latent factors.
CM_1 =~ NA*T1_CM_02 + T1_CM_02 + T1_CM_04 + T1_CM_05 + T1_CM_07 + T1_CM_08
CM_2 =~ NA*T2_CM_02 + T2_CM_02 + T2_CM_04 + T2_CM_05 + T2_CM_07 + T2_CM_08
CM_3 =~ NA*T3_CM_02 + T3_CM_02 + T3_CM_04 + T3_CM_05 + T3_CM_07 + T3_CM_08

# Intercepts
T1_CM_02 ~ 1
T1_CM_04 ~ 1
T1_CM_05 ~ 1
T1_CM_07 ~ 1
T1_CM_08 ~ 1

T2_CM_02 ~ 1
T2_CM_04 ~ 1
T2_CM_05 ~ 1
T2_CM_07 ~ 1
T2_CM_08 ~ 1

T3_CM_02 ~ 1
T3_CM_04 ~ 1
T3_CM_05 ~ 1
T3_CM_07 ~ 1
T3_CM_08 ~ 1

# Unique Variances
T1_CM_02 ~~ T1_CM_02
T1_CM_04 ~~ T1_CM_04
T1_CM_05 ~~ T1_CM_05
T1_CM_07 ~~ T1_CM_07
T1_CM_08 ~~ T1_CM_08

T2_CM_02 ~~ T2_CM_02
T2_CM_04 ~~ T2_CM_04
T2_CM_05 ~~ T2_CM_05
T2_CM_07 ~~ T2_CM_07
T2_CM_08 ~~ T2_CM_08

T3_CM_02 ~~ T3_CM_02
T3_CM_04 ~~ T3_CM_04
T3_CM_05 ~~ T3_CM_05
T3_CM_07 ~~ T3_CM_07
T3_CM_08 ~~ T3_CM_08

# Latent Variable Means
CM_1 ~ 0*1
CM_2 ~ 0*1
CM_3 ~ 0*1

# Latent Variable Variances and Covariance
CM_1 ~~ 1*CM_1
CM_2 ~~ 1*CM_2
CM_3 ~~ 1*CM_3
'

mod.configural.long6 <-semTools::measEq.syntax(LI_CM6, 
                                               longFacNames = longFacNames,
                                               data=Data_Cross_Panel, missing="fiml")
cat(as.character(mod.configural.long6))


fit.configural.long6 <- lavaan::cfa(as.character(mod.configural.long6), 
                               data = Data_Cross_Panel, 
                               fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.configural.long6, fit.measures=T) # Now we have good fit.


#### Invariance of Loadings-----------------------------------------------------

# Generate code for longitudinal invariance of loadings

mod.loadings.long <-semTools::measEq.syntax(LI_CM6, longFacNames = longFacNames, 
                                     long.equal = "loadings", data=Data_Cross_Panel,
                                     missing="fiml")

# Inspect the generated code
cat(as.character(mod.loadings.long))

# Fit the loading invariance model
fit.loading.long <- lavaan::cfa(as.character(mod.loadings.long), 
                               data = Data_Cross_Panel, 
                               fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.loading.long, fit.measures=T) 


#### Invariance of Intercepts & Loadings----------------------------------------

# Generate code for longitudinal invariance of intercepts and loadings
mod.int.load.long <-semTools::measEq.syntax(LI_CM6, 
                                              longFacNames = longFacNames, 
                                              long.equal = c("intercepts", 
                                                             "loadings"), 
                                              data=Data_Cross_Panel, missing="fiml")

# Inspect the generated code
cat(as.character(mod.int.load.long))

# Fit the intercept invariance model
fit.int.load.long <- lavaan::cfa(as.character(mod.int.load.long), 
                                  data = Data_Cross_Panel, 
                                  fixed.x=FALSE)

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.int.load.long, fit.measures=T) 

# Compare the different models successively, using the guidelines below.
# >20 observations per group:
# -.01 change in CFI, paired with changes in RMSEA of .015 and SRMR of .030 
# (loading invariance) or .015 (intercept invariance). Difference in 
# Chi-square should be non-significant
# 10-20 observations per group:
# CFI -.02 and RMSEA of .03  most appropriate for tests of loading invariance 
# with large group sizes, traditional criteria of -.01 for CFI 
# and .01 for RMSEA were appropriate for intercept invariance tests.
# Difference in Chi-square should be non-significant
# <10 observations per group: invariance testing is not a good idea.


Check.Load.Long <- semTools::compareFit(fit.configural.long6, fit.loading.long) 
summary(Check.Load.Long) # Good

Check.Int.Load.Long <- semTools::compareFit(fit.loading.long, 
                                            fit.int.load.long)
summary(Check.Int.Load.Long) # Good enough


### Reliability-----------------------------------------------------------------
compRelSEM(fit.int.load.long) # Omega's for all three timepoints; are a bit low


### Multilevel Factor Model-----------------------------------------------------
Mutlilevel_factor <- '
# Define the latent factors on level 1.
level: 1
  DG_1 =~ NA*anything +lambda1*anything + fail + fault + smart + 
        mistakes + hire + looks

# Intercepts
  anything ~ i1*1
  fail ~ 1
  fault ~ 1
  smart ~ 1
  mistakes ~ 1
  hire ~ 1
  looks ~ 1
  
# Unique Variances
  anything ~~ anything
  fail ~~ fail
  fault ~~ fault
  smart ~~ smart
  mistakes ~~ mistakes
  hire ~~ hire
  looks ~~ looks
  

# Latent Variable Means
  DG_1 ~ 0*1

# Latent Variable Variances and Covariance
  DG_1 ~~ 1*DG_1
  
# Define the latent factors on level 2.
level: 2

  DG_1 =~ NA*anything +lambda1*anything + fail + fault + smart + 
        mistakes + hire + looks
        
# Latent Variable Means
  DG_1 ~ 0*1

# Latent Variable Variances and Covariance
  DG_1 ~~ 1*DG_1
'

fit.multilevel <- cfa(model = Mutlilevel_factor, data = Data_ILD, 
                      cluster = "id")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.multilevel, fit.measures=T) # Fit isn't completely okay

# Can also check the proportion of variance in the indicators that is on the
# between level using the ICC
lavInspect(fit.multilevel, "icc")

# And you can get the within and between covariance matrices
lavInspect(fit.multilevel, "h1")


# Check how we can improve model-fit
MIsML <- modindices(fit.multilevel)
head(MIsML[order(MIsML$mi, decreasing = TRUE), 1:7], n = 20)



Mutlilevel_factor2 <- '
# Define the latent factors on level 1.
level: 1
  DG_1 =~ NA*anything +lambda1*anything + fail + fault + smart + 
        mistakes + hire + looks

# Intercepts
  anything ~ i1*1
  fail ~ 1
  fault ~ 1
  smart ~ 1
  mistakes ~ 1
  hire ~ 1
  looks ~ 1
  
# Unique Variances
  anything ~~ anything
  fail ~~ fail
  fault ~~ fault
  smart ~~ smart
  mistakes ~~ mistakes
  hire ~~ hire
  looks ~~ looks
  

# Latent Variable Means
  DG_1 ~ 0*1

# Latent Variable Variances and Covariance
  DG_1 ~~ 1*DG_1
  
# Residual Covariances

 anything ~~ fail
  
# Define the latent factors on level 2.
level: 2

  DG_1 =~ NA*anything +lambda1*anything + fail + fault + smart + 
        mistakes + hire + looks
        
# Latent Variable Means
  DG_1 ~ 0*1

# Latent Variable Variances and Covariance
  DG_1 ~~ 1*DG_1
'

fit.multilevel2 <- cfa(model = Mutlilevel_factor2, data = Data_ILD, 
                      cluster = "id")

# Check model fit using CFI, RMSEA, SRMR, and Chi-square again
summary(fit.multilevel2, fit.measures=T) # Good. Notice that the fit on the
# within level is different than the fit on the between level. It really
# are two different models looking at different things.

### Reliability per Level-------------------------------------------------------
compRelSEM(fit.multilevel2) # The reliability of the within-factor is .603
# The reliability of the between-factor is .707.

# Instead of looking at the re liabilities of the within and between factor
# we could also look at the reliability of composite (e.g. sum) scores of the
# mean-item scores (level 2 composite) and person-centered item scores (level 1
# composite). See  https://doi.org/10.1037/met0000287



#==============================================================================#
# Lab 3: Exploring Non-Invariance for Intensive Longitudinal Data
#==============================================================================#

### Packages required for Practical --------------------------------------------
#install.packages("devtools")
library(devtools)
#devtools::install_github("leonievm/lmfa")
library(lmfa)

### Data required for Practical-------------------------------------------------

data("ESM")

### Latent Markov Factor Analysis-----------------------------------------------

# After successful installation, you can perform LMFA by means of the three-step 
# estimation. The package consists of three main functions that are shown below. 
# For details about the function arguments, see the function documentations, 
# which can be opened with ?functionname

#### Research Questions we want to answer:
# (1) How many MMs are underlying our ILD? 
# (2) How do the MMs differ?
# (3) How do subjects transition between the MMs over time, and is this related 
#     to time- or subject-specific covariates?
# (4) For which subjects does longitudinal invariance hold, and for
#     which of these subjects does invariance hold across subjects?

#### Step 1---------------------------------------------------------------------

# The step 1 function estimates the state-specific factor analysis models 
# (with or without model selection). For our example data, we estimated models 
# with one to four states and two to three factors per state. The estimation 
# time takes around three hours. Therefore, you can just load the model 
# selection object, which is provided together with the package:

# set.seed(1000)
# modelselection <- step1(data = ESM,
#                         indicators = c(
#                           "Interested","Joyful","Determined","Calm","Lively",
#                           "Enthusiastic","Relaxed","Cheerful","Content",
#                           "Energetic","Upset","Gloomy","Sluggish","Anxious",
#                           "Bored","Irritated","Nervous","Listless"),
#                         modelselection = TRUE,
#                         n_state_range = 1:4, 
#                         n_fact_range = 2:3,
#                         n_starts = 25,
#                         max_iterations = 1000)


data("modelselection")

# We obtain the model-selection results as follows:
summary(modelselection)

# For model selection you can also look at the following two outputs, one
# for the BIC and one for the CHull:
plot(modelselection)
chull_lmfa(modelselection)

# We choose model [323]. We can extract and store it as follows:
measurementmodel323 <- modelselection$`[323]`

# Subsequently, we can display the parameter estimates with:
summary(measurementmodel323)

# We can see differences in all parameters.

# A copy of the dataset with the state-specific factor scores attached can be 
# obtained with
ESM_fs <- factorscores_lmfa(data = ESM, model = measurementmodel323)

#### Step 2---------------------------------------------------------------------

# The step 2 function obtains the posterior state-membership probabilities and 
# the modal state assignments and calculates the classification error:
classification <- step2(data = ESM_fs, model = measurementmodel323)

# The following code prints the results:
summary(classification)

# We don't have to look into it too much. We mainly need the object to obtain 
# correct estimates in step 3. In brief:
# The R-square measure R_entropy indicates how much the measurement models
# differ and thus how well the states are separated (from 0 = bad separation
# to 1= good separation). The R-entropy for our example indicates good
# separation. This explains the small total classification error.


#### Step 3---------------------------------------------------------------------

# The step 3 function estimates the transitions between the states (conditional 
# on covariates) by means of a continuous-time latent Markov model. We include
# two predictors, having had an intervention (yes or no) and unpleasantness of 
# the most unpleasant event (0-100). The estimation for our example data took 
# about 20 minutes. Again, you can just load the object from the data:

# set.seed(1000)
# transitionmodel <- step3(data = ESM_fs,
#                          identifier = "id",
#                          n_state = 3,
#                          postprobs = 
#                            classification$classification_posteriors[,-1],
#                          timeintervals  = "deltaT",
#                          initialCovariates = NULL,
#                          transitionCovariates = 
#                            c("intervention", "negativeEvent"),
#                          n_starts = 25,
#                          max_iterations = 1000)


data("transitionmodel")

# The following code prints the results:
summary(transitionmodel)

# We first look at the Wald tests to see if the inclusion of the covariates
# is significant. If you don't like p-values, you may (additionally) run 
# alternative models (e.g., with only one covariate or no covariate) and 
# compare the BIC values and choose the one with the lowest. For this example
# it didn't make a difference but it may make a difference for other data.
# We keep both covariates in the model.

# The parameters are difficult to interpret. It is better to obtain the 
# probabilities for an interval of interest and different covariate
# values that you want to compare with. For example, keep the negative event
# score equal to it's sample mean 49.65 and obtain the probabilities for
# before having had an intervention and for after having had an intervention;
# both for a unit interval:

probabilities(model = transitionmodel, 
              deltaT = 1,
              initialCovariateScores = NULL,
              transitionCovariateScores = c(0,49.65))

probabilities(model = transitionmodel, 
              deltaT = 1,
              initialCovariateScores = NULL,
              transitionCovariateScores = c(1,49.65))

# Comparing the transition probabilities, we see that having had an intervention
# is related to relatively smaller probabilities of transitioning to and staying 
# in the displeasure state ("S1"). 


# We can now obtain a copy of our dataset with the final state assignments 
# attached as follows:
ESM_fs_cl <- transitionmodel$data

# To obtian an initial idea of how subjects transition, we can inspect
# transition plots (here only for four subjects):
layout(matrix(c(1,3,2,4), nrow = 2, ncol = 2))
for(i in 1:4) plot(transitionmodel, identifier = "id", id = i)

# Finally, we can get some insights into for which subjects longitudinal 
# invariance holds and for which subjects the model is also invariant across 
# subjects. In other words, subjects would have to stay in the same state
# for their entire participation and invariance across subjects holds
# if they are in the same permanent state. We obtian the info with:

invariance(model = transitionmodel, identifier = "id")

# If there were subjects for which longitudinal invariance holds, they would be 
# listed under their permanent state membership. For subjects who are in the
# same permanent state, invariance holds across all observations. However, 
# the NA's indicate that this longitudinal invariance doesn't apply to any
# of the subjects.

#### Summary of the findings----------------------------------------------------

# (1) How many MMs are underlying our ILD? 
#     Three MMs are underlying the example data.
# (2) How do the MMs differ?
#     The number and nature of the factors differ, implying that configural 
#     invariance is violated for our example data. More specifically, we found 
#     three states (a displeasure, a neutral, and a pleasure state) that all 
#     contained a positive affect and a negative affect (or distress) factor, 
#     but the displeasure state was additionally characterized by a drive factor 
#     and the neutral state by a serenity factor.
# (3) How do subjects transition between the MMs over time, and is this related 
#     to time- or subject-specific covariates?
#     Most subjects started in the displeasure state. The probabilities of 
#     staying in a state were generally higher than transitioning to another 
#     state (especially for subjects in the displeasure state). Transitions to 
#     the displeasure state were most likely, especially when experiencing 
#     negative events. After receiving an intervention, the probabilities of 
#     transitioning to and staying in the neutral or pleasure state increased. 
# (4) For which subjects does longitudinal invariance hold, and for
#     which of these subjects does invariance hold across subjects? 
#     Longitudinal invariance does not hold for any of the subjects and, 
#     therefore, neither does invariance across subjects.



