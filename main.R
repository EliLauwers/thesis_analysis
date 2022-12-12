rm(list = ls())

set.seed(1234)
args = yaml::read_yaml("args.yml")

library(dplyr)

source("1.preprocess_data.R")

out_path = file.path("data", "bootstrap_inds.RDS")
if(args$steps$create_bootstrap){
  source("2.create_bootstrap.R")
}
inds = readRDS(out_path)



natural_effects = read.csv("natural_effects.csv")
mediator_specific_effects = read.csv("mediator_specific_effects.csv")

mapper = c(
  "ref" = "C",
  "d_cheap" = "X",
  "target" = "M1",
  "qual" = "M2",
  "intent" = "Y"
)
names(data) <- mapper[names(data)]

out_path = file.path("data", "results.RDS")
if(args$steps$effect_estimation){
  source("3.effect_estimation.R") 
}
results = readRDS(out_path)
