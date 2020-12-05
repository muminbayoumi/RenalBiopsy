library(tidyverse)
library(tableone)
library(here)
library(ReporteRs)

source(file = "Data.R")
source(file = "../RFunctions/tableone to doc .R")

CreateTableOne(data=full,strata = "Biopsy_outcome")
