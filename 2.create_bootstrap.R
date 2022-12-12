inds = lapply(1:args$BOOTSTRAP$N_BOOT, function(x) {
  sample.int(
    n = nrow(data),
    size = ifelse(
      is.null(args$BOOTSTRAP$N_DATASET),
      nrow(data),
      args$BOOTSTRAP$N_DATASET
    ),
    replace = TRUE
  )
})
saveRDS(inds, out_path)
