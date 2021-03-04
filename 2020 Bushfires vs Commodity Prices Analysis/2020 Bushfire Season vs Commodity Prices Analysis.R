# Firstly, press Alt + O (the letter O not zero) to collapse all the sections

library(RCurl)
download.file("https://github.com/gsleigh24/Portfolio/raw/main/2020%20Bushfires%20vs%20Commodity%20Prices%20Analysis/Collated%20Dataset%201.xlsx", "temp.xlsx", mode = "wb")
library(readxl)
dataset <- read_excel("temp.xlsx", sheet = 2)

# Data Set Up -------------------------------------------------------------

#Import Dataset (to the right)
#Find file, and change name to "dataset"

#load up these function libraries for later use
library(mice)
library(readxl)
library(rugarch)
library(forecast)
library(fGarch)
library(FinTS)
library(fGarch)
library(forecast)
library(seastests)
library(tseries)
library(Dowd)
library(devtools)
library(mctest)
library(ppcor)

# Important Code for Output -----------------------------------------------

# **Important output lines:
# (can be copied and pasted into console whenever you want to access the data)**

# To show t-distribution of commodities data
  #hist(commodities_rtn, breaks = 100)

# Augmented-Dickey Fuller Test for stationarity of Returns
  #adf.test(commodities_rtn)

#Multicollinearity Measures for variables (previously had removed variables w/ VIF > 4)
  #show(mc_test)
  #show(mc_corr_coef)

# ARIMA Model Selection
  #show(result_auto_arima)
  
# Comparison table of AICs for all the different types of GARCH models
  #show (all_garch_aic_compare)
 
# Same AIC table but comparing different combos of p&q
  #show (all_garch_m_aic_compare)

# Results for Ljung-Box Test can be found in this output
  #show (garch_m32_fit) 

# QQ plot
  #qq_resid = as.numeric(residuals(garch_m32_fit, standardize = TRUE))
  #TQQPlot(qq_resid, n-1)

# GARCH-M(3,2) results for analysis of our theory can be found in this output
  #show(garch_m32_fit)


# Define and Arrange Variables --------------------------------------------

#define variables (assigning certain vectors with a variable name)
n=nrow(dataset) #no. of observations
nvar=ncol(dataset)-1 #no. of variables (excluding the date) = 13
nreg = nvar - 1 # no. of regressors ( = nvar - the y variable)
date = dataset[2:n,1] #1st column of the table "dataset", every row except for the 1st (we are doing returns)

#Dealing with missing values for temp. and smoke level via imputation
imputed_data = mice(dataset[,2:nvar+1], seed = 123) #columns 2:nvar to cut off the date
regressors = mice::complete(imputed_data)
#Showing how many/ what % of values were imputed
values_imputed = sum(is.na(dataset[,3:ncol(dataset)]))
pct_imputed = (values_imputed/((ncol(regressors)-1)*(nrow(dataset)-1)))*100

#Sorting out regressor data (taking top observation off so it matches the return data)
regressors = regressors[2:n,]

#Changing Data Type for ugarchfit function
regressors = as.matrix(regressors)

#Setting variable for commodities return data 
commodities_rtn = as.matrix(dataset[2:n,2])


# Data Assumptions, ADF Test on commodities Data, Multicollinearity Test --------------------------------------------------------

# Outline any assumptions given the data
  # When using GARCH, we assume that our returns series is covariance stationary
  # To test, use ADF Test
adf.test(commodities_rtn)
  # Null hypothesis of a unit root is rejected at the 5% level, therefore stationary 
# Check distribution of commodities data
hist(commodities_rtn, breaks = 100)
# The financial data has fat tails, so we assume a student's T-distribution
# This will adjusted for in the "ugarchspec" model.distribution later on

#Checking for collinearity between the regressors
dataset = cbind(commodities_rtn, regressors)
model_mc = lm(commodities_rtn ~ ., data = as.data.frame(dataset))
mc_test = imcdiag(model_mc)
mc_table = pcor(as.data.frame(regressors))
options(scipen = 999)
mc_corr_coef = round(mc_table$estimate, digits = 3)
mc_pval = round(mc_table$p.value, digits = 3)

# ARIMA Model Selection ---------------------------------------------------

# The weather data is very much dependent on its previous values (i.e. a hot day is likely to be followed by another hot day in summer)
# So we will use an ARIMA model to adjust for this
#   Seasonality is tested and adjusted for in auto.arima, with any seasonal differencing required automatically applied
#   Heteroskedasticity doesn't need to be tested for as we are using GARCH models
# ARMA model selection using AICc
result_auto_arima = auto.arima(commodities_rtn, xreg = regressors[,2:nreg], ic = "aicc")
show(result_auto_arima)
#ARMA parameters will be set to 4,1,1 to follow


# GARCH Model Selection (e.g. eGARCH vs. GARCH-M etc.) -----------------------

# Testing of different types of GARCH (all of order 1,1) from (using information criterion):

#   E-GARCH
e_garch_spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = FALSE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

e_garch_fit = ugarchfit(data = commodities_rtn, spec = e_garch_spec, solver="hybrid")

ic_e_garch = infocriteria(e_garch_fit)
aic_e_garch = ic_e_garch[1,1]

#   GARCH-M

garch_m_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m_fit = ugarchfit(data = commodities_rtn, spec = garch_m_spec, solver="hybrid")

ic_garch_m = infocriteria(garch_m_fit)
aic_garch_m = ic_garch_m[1,1]

#   GJR-GARCH

gjr_garch_spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = FALSE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

gjr_garch_fit = ugarchfit(data = commodities_rtn, spec = gjr_garch_spec, solver="hybrid")

ic_gjr_garch = infocriteria(gjr_garch_fit)
aic_gjr_garch = ic_gjr_garch[1,1]

#   apARCH

ap_arch_spec = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = FALSE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

ap_arch_fit = ugarchfit(data = commodities_rtn, spec = ap_arch_spec, solver="hybrid")

ic_ap_arch = infocriteria(ap_arch_fit)
aic_ap_arch = ic_ap_arch[1,1]

#   iGARCH

i_garch_spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = FALSE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

i_garch_fit = ugarchfit(data = commodities_rtn, spec = i_garch_spec, solver="hybrid")

ic_i_garch = infocriteria(i_garch_fit)
aic_i_garch = ic_i_garch[1,1]

#   csGARCH

cs_garch_spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = FALSE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

cs_garch_fit = ugarchfit(data = commodities_rtn, spec = cs_garch_spec, solver="hybrid")

ic_cs_garch = infocriteria(cs_garch_fit)
aic_cs_garch = ic_cs_garch[1,1]

# Table of all AICs to pick best model

all_garch_aic_compare = cbind(c("eGARCH","GARCH-M", "gjrGARCH", "apARCH", "iGARCH", "csGARCH"), c(aic_e_garch, aic_garch_m, aic_gjr_garch, aic_ap_arch, aic_i_garch, aic_cs_garch))
show (all_garch_aic_compare)

# GARCH-M(1,1) has lowest AIC of all models


# GARCH-M(p,q) Order Selection --------------------------------------------

# Now do the same thing but with different values of p & q for GARCH-M to decide which order is best

# 1, 1

garch_m11_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m11_fit = ugarchfit(data = commodities_rtn, spec = garch_m11_spec, solver = "hybrid")

ic_garch_m11 = infocriteria(garch_m11_fit)
aic_garch_m11 = ic_garch_m11[1,1]

# 1, 2

garch_m12_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m12_fit = ugarchfit(data = commodities_rtn, spec = garch_m12_spec, solver="hybrid")

ic_garch_m12 = infocriteria(garch_m12_fit)
aic_garch_m12 = ic_garch_m12[1,1]

# 2, 1

garch_m21_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m21_fit = ugarchfit(data = commodities_rtn, spec = garch_m21_spec, solver="hybrid")

ic_garch_m21 = infocriteria(garch_m21_fit)
aic_garch_m21 = ic_garch_m21[1,1]


# 2, 2

garch_m22_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m22_fit = ugarchfit(data = commodities_rtn, spec = garch_m22_spec, solver="hybrid")

ic_garch_m22 = infocriteria(garch_m22_fit)
aic_garch_m22 = ic_garch_m22[1,1]

# 2, 3

garch_m23_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,3)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m23_fit = ugarchfit(data = commodities_rtn, spec = garch_m23_spec, solver="hybrid")

ic_garch_m23 = infocriteria(garch_m23_fit)
aic_garch_m23 = ic_garch_m23[1,1]

# 3, 2

garch_m32_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,2)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m32_fit = ugarchfit(data = commodities_rtn, spec = garch_m32_spec, solver="hybrid")

ic_garch_m32 = infocriteria(garch_m32_fit)
aic_garch_m32 = ic_garch_m32[1,1]

# 3, 3

garch_m33_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3,3)), mean.model = list(armaOrder = c(4,1), include.mean = TRUE, external.regressors = regressors,  arfima = TRUE), distribution.model = "std", fixed.pars = list(arfima = 1))

garch_m33_fit = ugarchfit(data = commodities_rtn, spec = garch_m33_spec, solver="hybrid")

ic_garch_m33 = infocriteria(garch_m33_fit)
aic_garch_m33 = ic_garch_m33[1,1]

# Compare different GARCH-M models by AIC

all_garch_m_aic_compare = cbind(c("1,1","1,2", "2,1", "2,2", "2,3", "3,2", "3,3"), c(aic_garch_m11, aic_garch_m12, aic_garch_m21, aic_garch_m22, aic_garch_m23, aic_garch_m32, aic_garch_m33))
show(all_garch_m_aic_compare)

# GARCH-M(1,1) has the lowest AIC of the models tested


# GARCH(1,1) Diagostics ---------------------------------------------------

# Check for:
#   Autocorrelations
#        Ljung-Box Test
show(garch_m11_fit) # Results for Ljung-Box Test can be found in this output

# QQ plot
qq_resid = as.numeric(residuals(garch_m11_fit, standardize = TRUE))
TQQPlot(qq_resid, n-1)

# Ready for Analysis ------------------------------------------------------

# Conduct analysis on our data to see the effect (if any) of bushfires on the economy over the 6 month period between September 2019 and February/March 2020.
show(garch_m11_fit) # Results for GARCH-M(1,1) can be found at the top of this output