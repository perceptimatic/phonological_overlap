Sys.setlocale(locale = "en_US.UTF-8")
library(conflicted)
library(tidyverse)
library(brms)
library(cmdstanr)
library(marginaleffects)
library(foreach)
library(patchwork)
library(bayesplot)
library(patchwork)
library(ggrepel)

set_cmdstan_path("~/cmdstan-2.36.0")

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

cumord_categories <- c(-3, -2, -1, 1, 2, 3)

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

discr_preds <- left_join(
  discr,
  distances %>% select(filename, `Δ DTW Mel Filterbank`),
  by = "filename"
) %>% left_join(
  discr_idpreds_c %>%
    rename(`Contrast Accuracy and Certainty`=`Accuracy and Certainty`) %>%
    select(`Phonological Overlap`,
           `Phone Language (Long)`, `Phone Language (Code)`,
           `Phone Contrast (Language)`, `Listener Group`,
           Haskins, Dot, Cosine, Euclidean, SSDiff,
           `Same Top Choice`, `Maximum Categorization Threshold`,
           `Contrast Accuracy and Certainty`),
  by = c(
    "Phone Language (Long)",
    "Phone Language (Code)",
    "Phone Contrast (Language)",
    "Listener Group")) %>% left_join(
      rename(discr_f, `Filename Accuracy and Certainty`=`Accuracy and Certainty`,
             `Filename Accuracy`=Accuracy),
      by=c("filename", "Listener Group")
    )
  
