#' @function print_natural_effect 
#' @description This function pretty prints a natural effect
#' @param label how the natural effect is called (e.g: "NDE-000")
#' @param left_po the left potential outcome in the contrast (e.g.: c(1, 0, 0, 0))
#' @param right_po the right potential outcome in the contrast (e.g.: c(0, 0, 0, 0))
#' @return None
print_natural_effect <- function(label, left_po, right_po){
  lpo = glue("Y({left_po[1]}, M1({left_po[2]}), M2({left_po[3]}, M1({left_po[4]}))")
  rpo = glue("Y({right_po[1]}, M1({right_po[2]}), M2({right_po[3]}, M1({right_po[4]}))")
  cat(glue("{label}\tE[{lpo} - {rpo}]"), "\n")
}


#' @function natural_defs 
#' @description this function creates all natural effects
#' @return a list containing:
#' - label: how the natural effect is called (e.g: "NDE-000")
#' - left_po: the left potential outcome in the contrast (e.g.: c(1, 0, 0, 0))
#' - right_po: the right potential outcome in the contrast (e.g.: c(0, 0, 0, 0))
natural_defs<- function(){
  resp = list() # a response container
  # first, the combinations dataframe contains all "labels" for the indirect effects
  # a set of 3 binary variables implies a combinations dataframe of 8 rows.
  combinations = expand.grid(0:1, 0:1, 0:1)
  naturals = c("NDE","NIE1","NIE2","NIE12")
  for(i in seq_along(naturals)){
    # Per effect type in naturals vector (NDE, NIE1, ..), each combination out of the
    # combinations dataframe is calculated
    for(j in 1:nrow(combinations)){
      effect = list()
      comb = unname(combinations[j,]) # a vector with 3 elements like c(0, 1, 1)
      # the label is equal to the elements in the comb vector
      # e.g.: c(0, 0, 0) -> "000"
      effect[["label"]] = glue("{naturals[i]}-{paste(comb, collapse = '')}")
      # Next, based on what natural effect type is calculated, an extra element is added
      # For example: for the NDE-000,
      # - The combination coming from combinations dataframe is c(0, 0, 0)
      # - The left potential outcome is c(1, 0, 0, 0), 
      #   which is the label for the effect, prepended by a 1
      # - The right potential outcome is c(0, 0, 0, 0),
      #   which is the label for the effect, prepended by a 0.
      # Effect definitions for natural indirect effects are built similarly,
      # but with the 1 and 0 at other places
      effect[["left_po"]] = unlist(append(comb, 1, after = i - 1))
      effect[["right_po"]] = unlist(append(comb, 0, after = i - 1))
      # Print the effect
      do.call(print_natural_effect, effect)
      # Add the effect to the response container
      resp = append(resp, list(effect))
    }
  }
  return(resp)
}


#' @function MS1_defs 
#' @description this function creates all MS1 effects. This function
#'              is similar to the function above, although with other effects.
#'              MS1 effects are similar to natural effects, with the addition 
#'              that the last element in both potential outcomes is always similar
#'              to the third one
#' @return a list containing:
#' - label: how the natural effect is called (e.g: "MS1-NDE-00")
#' - left_po: the left potential outcome in the contrast (e.g.: c(1, 0, 0, 0))
#' - right_po: the right potential outcome in the contrast (e.g.: c(0, 0, 0, 0))
MS1_defs <- function(){
  resp = list()
  combinations = expand.grid(0:1, 0:1)
  mediator_specifics=c("NDE","NIE1","NIE2")
  for(i in seq_along(mediator_specifics)[1:3]){
    for(j in 1:nrow(combinations)){
      effect = list()
      comb = unname(combinations[j,]) # a vector with 2 elements like c(0, 1)
      left_po = unlist(append(comb, 1, after = i - 1))
      right_po = unlist(append(comb, 0, after = i - 1))
      # for the MS1, the last argument of th PO is equal to the third argument
      effect[["label"]] = glue("MS1-{mediator_specifics[i]}-{paste(comb, collapse = '')}")
      left_po[4] = left_po[3]
      right_po[4] = right_po[3]
      effect[["left_po"]] = left_po
      effect[["right_po"]] = right_po
      
      do.call(print_natural_effect, effect)
      resp = append(resp, list(effect))
    }
  }
  return(resp)
}


#' @function MS2_defs 
#' @description this function creates all MS2 effects. This function
#'              is similar to the function above, although with other effects
#' @return a list containing:
#' - label: how the natural effect is called (e.g: "MS2-NDE-00")
#' - left_po: the left potential outcome in the contrast (e.g.: c(1, 0, 0, 0))
#' - right_po: the right potential outcome in the contrast (e.g.: c(0, 0, 0, 0))
MS2_defs <- function(){
  resp = list()
  combinations = expand.grid(0:1, 0:1)
  mediator_specifics=c("NDE","NIE1","NIE2")
  for(i in seq_along(mediator_specifics)[1:3]){
    for(j in 1:nrow(combinations)){
      effect = list()
      comb = unname(combinations[j,]) # a vector with 2 elements like c(0, 1)
      left_po = unlist(append(comb, 1, after = i - 1))
      right_po = unlist(append(comb, 0, after = i - 1))
      
      # for the MS1, the last argument of th PO is equal to the third argument
      effect[["label"]] = glue("MS2-{mediator_specifics[i]}-{paste(comb, collapse = '')}")
      left_po[4] = left_po[2]
      right_po[4] = right_po[2]
      effect[["left_po"]] = left_po
      effect[["right_po"]] = right_po
      
      do.call(print_natural_effect, effect)
      resp = append(resp, list(effect))
    }
  }
  return(resp)
}

