print_natural_effect <- function(label, left_po, right_po){
  lpo = glue("Y({left_po[1]}, M1({left_po[2]}), M2({left_po[3]}, M1({left_po[4]}))")
  rpo = glue("Y({right_po[1]}, M1({right_po[2]}), M2({right_po[3]}, M1({right_po[4]}))")
  cat(glue("{label}\tE[{lpo} - {rpo}]"), "\n")
}

natural_effects = list()
# first, the combinations dataframe contains all "labels" for the indirect effects
# a set of 3 binary variables implies a combinations dataframe of 8 rows.
combinations = expand.grid(0:1, 0:1, 0:1)
naturals = c("NDE","NIE1","NIE2","NIE12")
for(i in seq_along(naturals)){
  # Per effect type (NDE, NIE, ..), each combination out of the
  # combinations dataframe is calculated
  for(j in 1:nrow(combinations)){
    comb = unname(combinations[j,]) # a vector with 3 elements like c(0, 1, 1)
    label = glue("{naturals[i]}-{paste(comb, collapse = '')}")
    # The definition of arguments for two potential outcomes
    # the potential outcomes are based upon the 3-elements combination
    # Next, based on what natural effect type is calculated, an extra element is added
    # For example: for the NDE-000,
    # - The combination coming from combinations dataframe is c(0, 0, 0)
    # - The left potential outcome is c(1, 0, 0, 0)
    # - The right potential outcome is c(0, 0, 0, 0)
    left_po = unlist(append(comb, 1, after = i - 1))
    right_po = unlist(append(comb, 0, after = i - 1))
    # both potential outcomes
    print_natural_effect(label, left_po, right_po)
    natural_effects[[label]] = list(left_po = left_po, right_po = right_po)
  }
  # When arrived here in the for loop, all effect definitions for a
  # given natural effect type (NDE, NIE_1, ..) are calculated 
  # The summary natural effect is now calculated
  slabel = glue("S{naturals[i]}")
  # inds = indexes of where the natural effect calculations
  # reside in the response object
  inds = which(stringr::str_detect(names(natural_effects), naturals[i]))
  effects = unlist(resp[inds])
  # for the calculation: the first and last effect (labels 000 and 111)
  # get a weight of (1/4), whereas the others get a weight of (1/12)
  natural_effects[[slabel]] = .25 * sum(effects[c(1,8)]) + (1/12) * sum(effects[2:7])
}


# MS effects
combinations = expand.grid(0:1, 0:1)
mspecific = c("NDE","NIE1","NIE2","NIE12")
for(i in seq_along(mspecific)){
  for(j in 1:nrow(combinations)){
    comb = unname(combinations[j,]) # a vector with 2 elements like c(0, 1)
    left_po = unlist(append(comb, 1, after = i - 1))
    right_po = unlist(append(comb, 0, after = i - 1))
    
    # for the MS1, the last argument of th PO is equal to the third argument
    MS1label = glue("MS1-{mspecific[i]}-{paste(comb, collapse = '')}")
    left_po[4] = left_po[3]
    right_po[4] = right_po[3]
    print_natural_effect(MS1label, left_po, right_po)
    natural_effects[[MS1label]] = natural_effect(mod_Y, left_po, right_po, data)
    
    # for the MS2, the last argument of th PO is equal to the third argument
    MS2label = glue("MS2-{mspecific[i]}-{paste(comb, collapse = '')}")
    left_po[4] = left_po[2]
    right_po[4] = right_po[2]
    print_natural_effect(MS2label, left_po, right_po)
    natural_effects[[MS2label]] = natural_effect(mod_Y, left_po, right_po, data)
  }
}