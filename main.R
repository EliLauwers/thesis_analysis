####################################
# Script: main.R
# Author: Eli Lauwers
# Student number: 01504786
# Promotor: Prof. Dr. Beatrijs Moerkerke
# Purpose: This script is the starting point for all data anlysis in my thesis
####################################


####################################
# GENERAL SET-up
####################################
rm(list = ls()) # clean up work env to ensure clean runs
set.seed(1234) # Set seed for reproducibility
# libraries
library(stringr)
library(tidyr)
library(dplyr)
library(glue)


#############################################
# PREPROCESSING
#############################################
# The first script makes sure that the data to be analysed is equal to the data
# as found in the paper. This to ensure comparability between this research
# and the original paper. Following steps are done
# - read raw data in .dat format
# - Encode invalid age data as missing
# - filter data on valid observations
# - keep relevant columns
# - scale mediators and outcome
source("preprocess_data.R")

#############################################
# RENAME DATASET
#############################################
# In order for the different models to be straightforwardly interpreted,
# the dataset is renamed to fit naming conventions
mapper = c(
  "ref"     =  "C",
  # Continuous Covariate, not scaled
  "d_cheap" =  "X",
  # binary treatment variable, not scaled
  "target"  =  "M1",
  # First sequential continuous mediator, scaled
  "qual"    =  "M2",
  # Second sequential continuous mediator, scaled
  "intent"  =  "Y"   # Continuous outcome, scaled
)
names(data) <- mapper[names(data)]



#############################################
# BOOTSTRAP INDICES FOR BOOTSTRAP ITERATIONS
#############################################
# In order to do bootstrapping, a list is created.
# Each element in the list is a vector of integers between 1 and <nrow(data)>.
# one list element therefore consists of one bootstrap iteration
BOOTS_NDATASETS = 10     # number of datasets that will be applied
BOOTS_NOBS = nrow(data)  # Number of observations per bootstrap iteration
boot_inds = lapply(1:BOOTS_NDATASETS, function(x) {
  sample.int(n = nrow(data),
             size = BOOTS_NOBS,
             replace = TRUE)
})
# For testing purposes, we do a 'bootstrap' using the original dataset, identically copied for 10 iterations
repeat_nrow = 10
boot_inds = lapply(1:repeat_nrow, function(x)
  1:nrow(data))



#############################################
# NATURAL EFFECT DEFINITIONS
#############################################
# The natural effects all follow the same principle.
# All are built from a contrast of two potential outcomes
# In the script natural_effect_definitions.R, the definitions for natural effects
# are "built" using simple algorithms.
# The result from each of these functions is a list containing elements:
# - label: the name of the effect (e.g.: "NDE-000")
# - left_po: the potential outcome that is left in the contrast (e.g.: c(1, 0, 0, 0))
# - right_po: the potential outcome that is right in the contrast (e.g.: c(0, 0, 0, 0))
source("natural_effect_definitions.R")
effect_defs = c(natural_defs(),
                MS1_defs(),
                MS2_defs())



#############################################
# EFFECT ESTIMATION
#############################################
# In the script effect_estimation_helpers.R, the various effect types are calculated
# - fit_models(): a list of all relevant fitted models
# - traditional_effects(): based on product of coefficients
# - total_causal_effect(): based on G-estimation
# - controlled_direct_effect(): based on linear model coefficients
# - predict_potential_mediators(): used as preprocessing for the natural effects.
#                                  adds columns to the dataset for M1(1), M2(0), ..
# - natural_effect(): takes the effect definitions for the natural effects (see above).
#                     It takes an effect definition and the dataset, and then calculates
#                     the natural contrast based on potential values in the dataset.
# - summary_natural_effect(): summarizes the natural effects (above)
# - interventional effects(): based on product of coeficients
source("effect_estimation_helpers.R")
# Iterate over the bootsrap_indices, and do the same thing per vecotr of indexes
effect_estimation = bind_rows(lapply(boot_inds, function(inds, effect_defs) {
  # Create a temporary dataframe by sampling the bootstrap indices from the dataframe
  tmp = data[inds, ]
  # Fit all relevant models for the effect types
  models = fit_models(tmp)
  # enhance the dataset, adds columns  denoting potential meediator values
  tmp = predict_potential_mediators(tmp, models)
  # Calculate relevant efeffects
  resp = c(
    total_causal_effect(tmp, models),
    controlled_direct_effect(tmp, models),
    traditional_effects(tmp, models),
    interventional_effects(tmp, models)
  )
  
  
  # Calculate every effect in the list of effect definitions
  # - natural effects
  # - MS1 effects
  # - MS2 effects
  for (ef in effect_defs) {
    resp[[ef$label]] = natural_effect(tmp, models, ef$left_po, ef$right_po)
  }
  
  # Add summary effects
  resp = c(resp, summary_natural_effects(resp))
  
  return(resp)
}, effect_defs = effect_defs))


#############################################
# SENSITIVITY ANALYSIS
#############################################
# in the script, one function is present:
# sample_potentials(): does a monte carlo sampling based on
# the sensitivity anlysis algorithm from Daniel (2015)
source("sensitivity_analysis_helpers.R")
# STEP 0: set-up for the algorithm
nat_defs = natural_defs() # all natural effects will be estimated
K_VECTOR = c(0, 0.5, 1) # let k vary from 0 to 1
# STEP 1: estimate regression parameters by OLS / ML
models = fit_models(data)
# Iterate over elements of K
# The sampling algorithm will be done for each k in the K vector
results = lapply(K_VECTOR, function(k, boot_inds, models, data) {
  # the analysis is bootstrapped, so iterate over the bootstrap inds
  results_k = bind_rows(lapply(boot_inds, function(inds) {
    # Create a bootstrapped dataset
    tmp = data[inds, ]
    # Add columns to the dataset with sampled mediators
    tmp = sample_potentials(k, models, tmp)
    # estimate all natural effects based on the dataset with sampled outcomes
    # To do this, use the list of natural effect definitions
    results_boot = lapply(nat_defs, function(def) {
      potential_outcomes = sapply(def[c("left_po", "right_po")], function(po) {
        glue("Y_{po[1]}_M1_{po[2]}_M2_{po[3]}_M1_{po[4]}")
      })
      mean(tmp[[potential_outcomes[1]]] -
             tmp[[potential_outcomes[2]]])
    })
    names(results_boot) = sapply(nat_defs, function(x)
      x$label)
    return(bind_rows(results_boot))
  }))
  # the results dataset now contains all potentential outcomes,
  # add the k level for future reference
  results_k$k = k
  return(results_k)
}, boot_inds = boot_inds, data = data, models = models)
results = bind_rows(results)

res = results %>%
  pivot_longer(-k, names_to = "effect", values_to = "effectsize") %>%
  group_by(k, effect) %>%
  # create means, and confidence intervals based on T distribution
  summarize(
    mean = mean(effectsize),
    sd  = sd(effectsize),
    n = n(),
    error = qt(1.95 / 2, df = n - 1) * sd / sqrt(n),
    lower = mean - error,
    upper = mean + error
  )
print(res)