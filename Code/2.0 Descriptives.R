# Code 2: Descriptives ----

rm(list=ls())

## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Input/"
data_out <- "Output/"

# Data TBC ----
tb <- rio::import(paste0(data_inp, "Serie_TBC_2011_2021.rds"))
tb_up <- rio::import(paste0(data_inp, "COHORTES TB 2015-2023.xlsx"), sheet="COMPLETA")

glimpse(tb); summary(tb)
glimpse(tb_up); summary(tb_up)
