Sys.setlocale(locale = "en_US.UTF-8")
library(conflicted)
library(tidyverse)
library(brms)
library(marginaleffects)
library(foreach)
library(patchwork)

conflicts_prefer(dplyr::filter, dplyr::select)

source("pathnames.R")
source("aggregation.R")
source("cleanup.R")
source("delta.R")
source("identification.R")
source("discrimination.R")
source("distances.R")
source("plotting.R")
source("regression.R")

discr_idpreds_c <- left_join(
  discr_c,
  idpreds,
  by = c(
    "Listener Group",
    "Phone Language (Long)",
    "Phone Language (Code)",
    "Phone Contrast (Language)"
  )
)
