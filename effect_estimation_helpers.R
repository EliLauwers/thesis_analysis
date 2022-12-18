#' @function fit_models
#' @description This function fits all relevant models in this research.
#' @param data a dataset
#' @return a list of models
fit_models <- function(data) {
  list(
    Y_no_med = lm(Y ~ X + C, data),
    Y = lm(Y ~ X + M1 + M2 + C, data),
    M1 = lm(M1 ~ X + C, data),
    M2_par = lm(M2 ~ X + C, data),
    M2_seq = lm(M2 ~ X + M1 + C, data)
  )
}



traditional_effects <- function(data, models) {
  resp = list()
  resp[["TRAD_DE"]]    = coef(models$Y)[["X"]]
  resp[["TRAD_IND1"]]  = coef(models$M1)[["X"]] * coef(models$Y)[["M1"]]
  resp[["TRAD_IND2"]]  = coef(models$M2_seq)[["X"]] * coef(models$Y)[["M2"]]
  resp[["TRAD_IND12"]] = coef(models$M1)[["X"]] * coef(models$M2_seq)[["M1"]] * coef(models$Y)[["M2"]]
  resp[["TRAD_IND"]]   = resp[["TRAD_IND1"]] + resp[["TRAD_IND2"]] + resp[["TRAD_IND12"]]
  return(resp)
}

total_causal_effect <- function(data, models) {
  resp = list()
  left = predict(models$Y_no_med, mutate(data, X = 1))
  right = predict(models$Y_no_med, mutate(data, X = 0))
  resp[["TCE"]] = mean(left - right)
  return(resp)
}

controlled_direct_effect <- function(data, models) {
  resp = list()
  resp[["CDE"]] = coef(models$mod_Y)[["X"]]
  return(resp)
}


#' @function predict_potential_mediators
#' @description takes a dataset, adds columns for potential mediator values
#' @param data a dataset
#' @param models the models list from @function fit_models()
#' @return the enhanced dataset with potential mediator values
predict_potential_mediators <- function(data, models) {
  data$M1_0 = as.numeric(predict(models$M1, mutate(data, X = 0)))
  data$M1_1 = as.numeric(predict(models$M1, mutate(data, X = 1)))
  data$M2_0_M1_0 = predict(models$M2_seq, mutate(data, X = 0, M1 = M1_0))
  data$M2_1_M1_0 = predict(models$M2_seq, mutate(data, X = 1, M1 = M1_0))
  data$M2_0_M1_1 = predict(models$M2_seq, mutate(data, X = 0, M1 = M1_1))
  data$M2_1_M1_1 = predict(models$M2_seq, mutate(data, X = 1, M1 = M1_1))
  return(data)
}


#' @function natural_effect
#' @description calculates a natural effect based on a dataset and a natural effect definition
#' @param data a dataset which is enhanced by @function predict_potential_mediators
#' @param models the models list from @function fit_models()
#' @param left_po the left potential outcome in the natural effect contrast. 
#' @param right_po the right potential outcome in the natural effect contrast. 
#' the left and right potential outcome comes from the effect definitions script
#' @return the calculated effect
natural_effect <- function(data, models, left_po, right_po) {
  left = predict(models$Y,
                 mutate(
                   data,
                   X = left_po[1],
                   # the estimated models is fitted based on the M columns,
                   # so, the M columns are interchanged with the columns denoting
                   # the potential mediator values
                   M1 = data[[glue::glue("M1_{left_po[2]}")]],
                   M2 = data[[glue::glue("M2_{left_po[3]}_M1_{left_po[4]}")]]
                 ))
  
  
  right = predict(models$Y,
                  mutate(
                    data,
                    X = right_po[1],
                    M1 = data[[glue::glue("M1_{right_po[2]}")]],
                    M2 = data[[glue::glue("M2_{right_po[3]}_M1_{right_po[4]}")]]
                  ))
  return(mean(left - right))
}


#' @function summary_natural_effects
#' @description calculates summary natural effects after natural effects are calculated
#' @param effects the list containing all (natural) effects
#' @return a list with summary effects
summary_natural_effects <- function(effects) {
  # a vector with effect types to be summarized
  naturals = c("NDE", "NIE1", "NIE2", "NIE12")
  # iterate over the natural list, and do the same thing per effect type
  summaries = lapply(naturals, function(natural) {
    # first, get a vector of the indices where the relevant effects in the 
    # effect list reside.
    inds = str_starts(names(effects), glue::glue("{natural}-"))
    # for the relevant indices, get the subset of relevant indices which
    # denote level 1 and 3 effects.
    levels_1_3 = str_ends(names(effects)[inds], c("000", "111"))
    # a vector of all effects for the relevant effect type
    resp = unname(unlist(effects[inds]))
    # return a weighted sum, with weights based on the level of the effect
    # levels 1 and 3 get a weight of (1/4), level 2 gets weight (1/12)
    return(sum(ifelse(levels_1_3, resp / 4, resp / 12)))
  })
  # the summaries is an unnamed list, so we add the names manually
  names(summaries) = paste0("S", naturals)
  return(summaries)
}

interventional_effects <- function(data, models) {
  resp = list()
  resp[["IDE"]] = coef(models$Y)[["X"]]
  resp[["IE1"]] = coef(models$M1)[["X"]] * coef(models$Y)[["M1"]]
  resp[["IE2"]] = coef(models$M2_par)[["X"]] * coef(models$Y)[["M2"]]
  resp[["IND_TOT"]] = resp[["IE1"]] + resp[["IE2"]]
  return(resp)
}
