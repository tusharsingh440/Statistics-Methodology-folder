### Title:    World Values Survey Data Analysis
### Author:   T: SNR - / ANR - 
###           T: SNR -  / ANR - 
###           A: SNR -  / ANR - 
###           M: SNR -  / ANR - 
### Group nr.:  36

###--------------------------------------------------------------------------###

# Cleanup to start
rm(list = ls(all = TRUE))

# First, set the working directory to current file 
# (Session -> Set Working Directory -> To Source File Location)

# Load packages needed for data cleaning: 
library(mice) # For missing data descriptives & MI
library(MASS) # For robust stats
library(naniar) # For NA values
library(dplyr) # For filtering the dataset
library(mitools) # For MI
library(MLmetrics) # We'll need this for MSEs

## Source the "studentFunctions.R" script to get the cv.lm function:
source("studentFunctions.R")

## Source the "miPredictionRoutines.R" script to get MI-based prediction stuff:
source("miPredictionRoutines.R")

set.seed(221291) # Set the random number seed

# Define the data directory:
dataDir <- "../data/"

## Load the data file ##
wvs_data <- readRDS(paste0(dataDir, "wvs_data.rds"))

## Multivariate outlier list (to save time when running the code) ##
m_out <- readRDS(paste0("mult_outlier.rds"))

###--------------------------------------------------------------------------###
### Filter variables ###

# Get the variables that are relevant for our analysis
wvs_data_filter <- wvs_data %>% select(V2, V8, V45, V47:V48, V52, V53:V54, V96,
                                       V139, V240, V242,
                                       V71, V81, V97, V99, V101,
                                       V181:V182, V229, V237:V239,
                                       V23, V57:V58, V191, V59)

# Changing names of the variables so they are more informative (Gender Politics)
wvs_data_filter <- wvs_data_filter %>% rename(country_code = V2, availableJob_inequality = V45, Wwage_inequality = V47, Wjob_independence = V48, 
                                              education_inequality = V52, business_inequality = V53, Whouse_fulfillment = V54,
                                              equal_rights = V139, Sex = V240, Age = V242)

# Changing names of the variables so they are more informative (Economic Beliefs)
wvs_data_filter <- wvs_data_filter %>% rename(work_importance = V8, importance_wealth = V71, economy_environment = V81, income_equality = V96,
                                              private_state_ownership = V97, view_on_competition = V99, wealth_accumulation = V101,
                                              job_security = V181, employment_status = V229, family_saving = V237, 
                                              social_class = V238, scale_income = V239)

# Changing names of the variables so they are more informative (Financial Satisfaction)
wvs_data_filter <- wvs_data_filter %>% rename(education_availability = V182, life_satisfaction = V23, Marital_status = V57,
                                              nr_children = V58, no_cash = V191, financial_satisfaction = V59) 
###--------------------------------------------------------------------------###
### Missing Data Descriptives ###

# The minus values all represent missing values, therefore we will set minus values to NA.
# This is to let R know what/how many values are missing. 

negative_values <- c(-1:-5)
wvs_data_filter <- replace_with_na_all(wvs_data_filter, ~.x %in% negative_values)
wvs_data_filter

summary(wvs_data_filter)

# Count the missing values in each feature
cm <- colSums(is.na(wvs_data_filter))
cm
pm <- colMeans(is.na(wvs_data_filter))
pm

# Count the observed values in each feature
co <- colSums(!is.na(wvs_data_filter))
co
po <- colMeans(!is.na(wvs_data_filter))
po

# Alternative of observed values
nrow(wvs_data_filter) - cm
1 - pm

# Alternative of missing values
nrow(wvs_data_filter) - co
1 - po

# Some stats on the missing percentages
range(pm)
mean(pm)
median(pm)

# All features that miss at least 10% of their data
pm[pm > 0.1]

# Missing patterns in the data
missPat <- md.pattern(wvs_data_filter)
missPat
dev.off()

missPat[ , ncol(missPat)]
missPat[-nrow(missPat), ncol(missPat)]

as.numeric(rownames(missPat))
as.numeric(rownames(missPat))[-nrow(missPat)]

# Covariance coverage
cc <- md.pairs(wvs_data_filter)$rr / nrow(wvs_data_filter)
cc
range(cc)

eps <- 0.8
all(cc > eps)

pat <- cc <= eps
apply(pat, 1, function(x) names(x)[x])

cc[lower.tri(cc, diag = TRUE)]
###--------------------------------------------------------------------------###
### Univariate Outlier Analysis ###

# Univariate outliers using the boxplot method
boxplot.stats(wvs_data_filter$private_state_ownership)

x <- wvs_data_filter$private_state_ownership

# Define a function to implement the boxplot method:
bpOutliers <- function(x) {
  iFen <- boxplot.stats(x, coef = 1.5)$stats[c(1, 28)]
  oFen <- boxplot.stats(x, coef = 3.0)$stats[c(1, 28)]
  list(possible = which(x < iFen[1] | x > iFen[2]),
       probable = which(x < oFen[1] | x > oFen[2])
  )
}

rm(list = c("x"))

lapply(wvs_data_filter[ , -c(1, 11)], FUN = bpOutliers)

# Some EDA for plotting the outliers
hist(wvs_data_filter$education_inequality)
hist(wvs_data_filter$equal_rights)
hist(wvs_data_filter$social_class)

### Univariate Outlier Treatment ###

# Using Winsorization to treat the Univariate outliers
# For education inequality (3 is for the 1st quantile value)
summary(wvs_data_filter$education_inequality)
lower_threshold1 <- round(3.0 - IQR(wvs_data_filter$education_inequality, na.rm = TRUE) * 1.5)
lower_threshold1

wvs_data_filter$education_inequality <- replace(wvs_data_filter$education_inequality, wvs_data_filter$education_inequality < 2, lower_threshold1)
hist(wvs_data_filter$education_inequality)

# For equal rights (8 is for the 1st quantile value)
summary(wvs_data_filter$equal_rights)
lower_threshold2 <- round(8 - IQR(wvs_data_filter$equal_rights, na.rm = TRUE) * 1.5)
lower_threshold2

wvs_data_filter$equal_rights <- replace(wvs_data_filter$equal_rights, wvs_data_filter$equal_rights < 5, lower_threshold2)
hist(wvs_data_filter$equal_rights)

# For equal rights (3 is for the 1st quantile value)
summary(wvs_data_filter$social_class)
lower_threshold3 <- round(3.0 - IQR(wvs_data_filter$social_class, na.rm = TRUE) * 1.5)
lower_threshold3

wvs_data_filter$social_class <- replace(wvs_data_filter$social_class, wvs_data_filter$social_class < 2, lower_threshold3)
hist(wvs_data_filter$social_class)

###--------------------------------------------------------------------------###
### Treating Missing Data / Multiple Imputation###

# Multiple Imputation

predM <- quickpred(wvs_data_filter, mincor = 0.2, include = "Sex")
miceOut1 <- mice(wvs_data_filter, m = 10, maxit = 5, predictorMatrix = predM, seed = 221291)

MIlist <- complete(miceOut1, "all")
MIlist

plot(miceOut1)
dev.off()

###--------------------------------------------------------------------------###
### Multivariate data analysis###

# Multivariate outliers using the Mahalanodibis distance method
# MCDestimation
data  <- MIlist$`1`[ , -c(1, 11)]
prob  <- 0.99
ratio <- 0.75

mcdMahalanobis <- function(data, prob, ratio = 0.75, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  stats <- cov.mcd(data, quantile.used = floor(ratio * nrow(data)))
  
  md <- mahalanobis(x = data, center = stats$center, cov = stats$cov)
  
  crit <- qchisq(prob, df = ncol(data))
  
  which(md > crit)
}

rm(list = c("data", "prob", "ratio"))

# The code below is used to test multivariate outliers on each imputed dataframe. 
# To save computing time it is commented out and provided in the loaded data "m_out".

#m_out <- lapply(MIlist,
             #function(x) mcdMahalanobis(prob = 0.99, seed = 221291, data = x))

# Count of which observations are outliers in more than 50% of the cases
m_out_list <- unlist(m_out)
outlier.freq <- as.data.frame(table(m_out_list))
m_out_list <- filter(outlier.freq, Freq >= 5)
m_out_list <- as.numeric(as.vector(m_out_list$m_out_list))

miceOut1_updated <- miceOut1$data[-c(m_out_list), ]

# Creating new MI list to remove the multivariate outliers
miceOut2 <- mice(miceOut1_updated, m = 10, maxit = 5, predictorMatrix = predM, seed = 221291)
MIlist_updated <- complete(miceOut2, "all")
MIlist_updated

###--------------------------------------------------------------------------###
### Analysis Step

## Fit some regression models to the MI data (inference):

## Individual simple linear regression to find significant predictors. 
fitsReg1 <- lm.mids(view_on_competition ~ business_inequality, data = miceOut2)
fitsReg2 <- lm.mids(view_on_competition ~ equal_rights, data = miceOut2)
fitsReg3 <- lm.mids(view_on_competition ~ income_equality, data = miceOut2) 
fitsReg4 <- lm.mids(view_on_competition ~ availableJob_inequality, data = miceOut2)
fitsReg5 <- lm.mids(view_on_competition ~ Whouse_fulfillment, data = miceOut2)

##Additive multiple linear regression where more predictors added.

fitsReg6 <- lm.mids(view_on_competition ~ Whouse_fulfillment + equal_rights, data = miceOut2)
fitsReg7 <- lm.mids(view_on_competition ~ Whouse_fulfillment + availableJob_inequality + equal_rights, data = miceOut2)
fitsReg8 <- lm.mids(view_on_competition ~ Whouse_fulfillment + availableJob_inequality + equal_rights + business_inequality, data = miceOut2)

###--------------------------------------------------------------------------###
### Pooling Step

# Pool an arbitrary list of fitted models:
poolFit1 <- pool(fitsReg1)
poolFit2 <- pool(fitsReg2)
poolFit3 <- pool(fitsReg3)
poolFit4 <- pool(fitsReg4)
poolFit5 <- pool(fitsReg5)


poolFit6 <- pool(fitsReg6)
poolFit7 <- pool(fitsReg7)
poolFit8 <- pool(fitsReg8)


# Summarize pooled results:
summary(poolFit1)
summary(poolFit2)
summary(poolFit3)
summary(poolFit4)
summary(poolFit5)

summary(poolFit6)
summary(poolFit7)
summary(poolFit8)

# Check if coefficeints are significant:
summary(poolFit1)$p.value < 0.05
summary(poolFit2)$p.value < 0.05
summary(poolFit3)$p.value < 0.05
summary(poolFit4)$p.value < 0.05
summary(poolFit5)$p.value < 0.05

summary(poolFit6)$p.value < 0.05
summary(poolFit7)$p.value < 0.05
summary(poolFit8)$p.value < 0.05

#Compute the pooled R^2 value. 
pool.r.squared(fitsReg1)
pool.r.squared(fitsReg2)
pool.r.squared(fitsReg3)
pool.r.squared(fitsReg4)
pool.r.squared(fitsReg5)

pool.r.squared(fitsReg6)
pool.r.squared(fitsReg7)
pool.r.squared(fitsReg8)

## Compute increase in R^2:
pool.r.squared(fitsReg6)[1] - pool.r.squared(fitsReg5)[1]
pool.r.squared(fitsReg7)[1] - pool.r.squared(fitsReg6)[1]
pool.r.squared(fitsReg8)[1] - pool.r.squared(fitsReg7)[1]

## Do an F-test for the increase in R^2:
fTest1 <- pool.compare(fitsReg6, fitsReg5)
fTest2 <- pool.compare(fitsReg7, fitsReg6)
fTest3 <- pool.compare(fitsReg8, fitsReg7)

fTest1$Dm     # Test statistic
fTest1$pvalue # P-Value

fTest2$Dm     # Test statistic
fTest2$pvalue # P-Value

fTest3$Dm     # Test statistic
fTest3$pvalue # P-Value
###--------------------------------------------------------------------------###
### MI-Based Prediction ###

# Split the multiply imputed datasets into training, validation and testing sets:
set.seed(221291)
n <- nrow(MIlist_updated[[1]])
index <- sample(
  c(rep("train", 7500), rep("valid", 2000), rep("test", n - 9500))
)

MIlist2 <- splitImps(imps = MIlist_updated, index = index)

# Creating the models to be checked
mods <- c("financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + job_security + education_availability + no_cash + employment_status + family_saving + scale_income + social_class",
          "financial_satisfaction ~ life_satisfaction + importance_wealth + job_security + education_availability + no_cash + employment_status + scale_income",
          "financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + no_cash + family_saving + social_class",
          "financial_satisfaction ~ life_satisfaction + no_cash + family_saving + scale_income + social_class",
          "financial_satisfaction ~ life_satisfaction + Marital_status + importance_wealth + job_security + education_availability + no_cash + family_saving + scale_income + social_class"
          )

# Train models and compute validation set MSE
mse <- c()
for(m in mods) {
  fits     <- lapply(X   = MIlist2$train,
                     FUN = function(x, mod) lm(mod, data = x),
                     mod = m)
  mse[m] <- mseMi(fits = fits, newData = MIlist2$valid)
}

mse

# Merge the MI training and validations sets:
index2   <- gsub(pattern = "valid", replacement = "train", x = index)
MIList3 <- splitImps(MIlist_updated, index2)

## Conduct 10-fold cross-validation in each multiply imputed dataset:
tmp <- sapply(MIList3$train, cv.lm, K = 10, models = mods, seed = 221291)

## Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)
cve

## Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = MIList3$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = mods[which.min(cve)])
mse <- mseMi(fits = fits, newData = MIList3$test)

mse


# Again but this time without the best model so we get the MSE on test data for the second best
##
##

mods <- c(#"financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + job_security + education_availability + no_cash + employment_status + family_saving + scale_income + social_class",
          "financial_satisfaction ~ life_satisfaction + importance_wealth + job_security + education_availability + no_cash + employment_status + scale_income",
          "financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + no_cash + family_saving + social_class",
          "financial_satisfaction ~ life_satisfaction + no_cash + family_saving + scale_income + social_class",
          "financial_satisfaction ~ life_satisfaction + Marital_status + importance_wealth + job_security + education_availability + no_cash + family_saving + scale_income + social_class"
)

# Merge the MI training and validations sets:
index2   <- gsub(pattern = "valid", replacement = "train", x = index)
MIList3 <- splitImps(MIlist_updated, index2)

## Conduct 10-fold cross-validation in each multiply imputed dataset:
tmp <- sapply(MIList3$train, cv.lm, K = 10, models = mods, seed = 221291)

## Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)
cve

## Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = MIList3$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = mods[which.min(cve)])
mse2 <- mseMi(fits = fits, newData = MIList3$test)

mse2

# Last time to get the 3rd best (income variables) MSE on test data
##
##

mods <- c(#"financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + job_security + education_availability + no_cash + employment_status + family_saving + scale_income + social_class",
  "financial_satisfaction ~ life_satisfaction + importance_wealth + job_security + education_availability + no_cash + employment_status + scale_income",
  "financial_satisfaction ~ life_satisfaction + Marital_status + nr_children + importance_wealth + no_cash + family_saving + social_class",
  "financial_satisfaction ~ life_satisfaction + no_cash + family_saving + scale_income + social_class"
  #"financial_satisfaction ~ life_satisfaction + Marital_status + importance_wealth + job_security + education_availability + no_cash + family_saving + scale_income + social_class"
)

# Merge the MI training and validations sets:
index2   <- gsub(pattern = "valid", replacement = "train", x = index)
MIList3 <- splitImps(MIlist_updated, index2)

## Conduct 10-fold cross-validation in each multiply imputed dataset:
tmp <- sapply(MIList3$train, cv.lm, K = 10, models = mods, seed = 221291)

## Aggregate the MI-based CVEs:
cve <- rowMeans(tmp)
cve

## Refit the winning model and compute test-set MSEs:
fits <- lapply(X   = MIList3$train,
               FUN = function(x, mod) lm(mod, data = x),
               mod = mods[which.min(cve)])
mse3 <- mseMi(fits = fits, newData = MIList3$test)

mse3


mse   # MSE for baseline model
mse2  # MSE for fifth model
mse3  # MSE for income model
