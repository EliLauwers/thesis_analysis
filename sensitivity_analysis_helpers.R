sample_potentials <- function(k, models, data){
  
  # STEP 0: get sigma_sq_hat for the mediator and outcome models
  sigma_sq_hat = sapply(models[c("M1", "M2_seq", "Y")], function(mod) {
    mean(mod$residuals**2)
  })
  
  # STEP 2: Sample V_i from a normal distribution
  # Draw M_1,i(x)
  V_i = rnorm(nrow(data), 0, k ** 2 * sigma_sq_hat[1])
  for (x in c(0, 1)) {
    nu_1 = predict(models$M1,
                   mutate(data, X = x))
    dataset_label = glue("M1_{x}")
    data[dataset_label] = rnorm(
      n = nrow(data),
      mean = nu_1 + V_i,
      sd = (1 - k ** 2) * sigma_sq_hat[1]
    )
  }
  
  
  
  # STEP 3: draw M_2,i(x, M_1,i(xp))
  for (x in c(0, 1)) {
    for (xp in c(0, 1)) {
      nu_2 = predict(models$M2_seq,
                     mutate(data,
                            X = x,
                            M1 = data[[glue("M1_{xp}")]]))
      dataset_label = glue("M2_{x}_M1_{xp}")
      data[dataset_label] = rnorm(n = nrow(data),
                                  mean = nu_2,
                                  sd = sigma_sq_hat[2])
    }
  }
  
  
  
  # STEP 4: draw Y_i(x, M_1,i(xp), M1_i(xpp, M_1,i(xppp))
  for (x in c(0, 1)) {
    for (xp in c(0, 1)) {
      for (xpp in c(0, 1)) {
        for (xppp in c(0, 1)) {
          nu_3 = predict(models$Y,
                         mutate(
                           data,
                           X = x,
                           M1 = data[[glue("M1_{xp}")]],
                           M2 = data[[glue("M2_{xpp}_M1_{xppp}")]]
                         ))
          
          dataset_label = glue("Y_{x}_M1_{xp}_M2_{xpp}_M1_{xppp}")
          data[dataset_label] = rnorm(n = nrow(data),
                                      mean = nu_3,
                                      sd = sigma_sq_hat[3])
        }
      }
    }
  }
  return(data)
}

