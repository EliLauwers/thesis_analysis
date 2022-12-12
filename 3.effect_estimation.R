
tce <- function(data, model) {
  left = predict(model, mutate(data, X = 1))
  right = predict(model, mutate(data, X = 0))
  mean(left - right)
}

cde <- function(data, model) {
  coef(model)[["X"]]
}


natural <- function(data, model, effect) {
  left = predict(model,
                 mutate(
                   data,
                   X = effect$LY,
                   M1 = data[[glue::glue("M1_{effect$LM1}")]],
                   M2 = data[[glue::glue("M2_{effect$LM2}_M1_{effect$LM2M1}")]]
                 ))
  
  
  right = predict(model,
                  mutate(
                    data,
                    X = effect$RY,
                    M1 = data[[glue::glue("M1_{effect$RM1}")]],
                    M2 = data[[glue::glue("M2_{effect$RM2}_M1_{effect$RM2M1}")]]
                  ))
  mean(left - right)
}


start = Sys.time()
results = lapply(seq_along(inds), function(i, inds, data, natural_effects, mediator_specific_effects) {
  print(glue::glue("estimation [{i} of {length(inds)}]"))
  resp = list()
  tmp = data[inds[[i]], ]
  
  mod_out1 = lm(Y ~ X + C, tmp)
  mod_out2 = lm(Y ~ X + M1 + M2 + C, tmp)
  
  mod_M1 = lm(M1 ~ X + C, tmp)
  mod_M2 = lm(M2 ~ X + C, tmp)
  mod_M2_seq = lm(M2 ~ X + M1 + C, tmp)
  
  resp[["TCE"]] = tce(tmp, mod_out1)
  resp[["CDE"]] = cde(tmp, mod_out2)
  
    tmp$M1_0 = as.numeric(predict(mod_M1, mutate(tmp, X = 0)))
  tmp$M1_1 = as.numeric(predict(mod_M1, mutate(tmp, X = 1)))
  
  tmp$M2_0_M1_0 = predict(mod_M2_seq, mutate(tmp, X = 0, M1 = M1_0))
  tmp$M2_1_M1_0 = predict(mod_M2_seq, mutate(tmp, X = 1, M1 = M1_0))
  tmp$M2_0_M1_1 = predict(mod_M2_seq, mutate(tmp, X = 0, M1 = M1_1))
  tmp$M2_1_M1_1 = predict(mod_M2_seq, mutate(tmp, X = 1, M1 = M1_1))
  
  naturals = list()
  for (i in seq(nrow(natural_effects))) {
    effect = natural_effects[i,]
    naturals[[effect$label]] = natural(tmp, mod_out2, effect)
  }
  
  summaries = c("SNDE", "SNIE1", "SNIE2", "SNIE12")
  for(i in 0:3){
    resp[[summaries[i+1]]] = .25 * (naturals[[i*8+1]] + naturals[[i*8+8]]) + 
      (1/12) * sum(unlist(naturals[(i*8+2):(i*8+7)]))
  }
  
  mediator_specifics = list()
  for (i in seq(nrow(mediator_specific_effects))) {
    effect = mediator_specific_effects[i,]
    mediator_specifics[[effect$label]] = natural(tmp, mod_out2, effect)
  }
  
  resp = append(resp, naturals)
  resp = append(resp, mediator_specifics)
  return(resp)
}, inds=inds,data = data, natural_effects = natural_effects, mediator_specific_effects = mediator_specific_effects)

saveRDS(bind_rows(results), out_path)
print(glue::glue("Estimation process took {Sys.time() - start}"))