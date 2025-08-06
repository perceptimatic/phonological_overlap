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
library(e1071)
library(scales)

set_cmdstan_path("~/cmdstan-2.36.0")

conflicts_prefer(dplyr::filter, dplyr::select, e1071::kurtosis)

source("pathnames.R")
source("aggregation.R")
source("cleanup.R")
source("delta.R")
source("identification.R")
source("discrimination.R")
source("distances.R")
source("plotting.R")
source("regression.R")

#cumord_categories <- c(-3, -2, -1, 1, 2, 3)

PHONES_SIMILAR <- c("i–ĩ (pt)", "øː–ʏ (de-m)", "œ–ɯ (tr)")
PHONES_FRENCH <-  c("u–ũ (pt)", "ɤː–yː (et)", "e–ɛ (fr)", "øː–uː (de-m)",
                    "uː–yː (de-m)", "u–y (tr)")
PHONES_ENGLISH <-  c("æ–ɑ (en)", "æ–ʌ (en)", "ɑ–ʌ (en)", "õ–ũ (pt)",
                     "õ–ũ (pt)", "aː–æː (et)", "e–i (fr)", "a–aː (de)")


#distances_c <-  distances |>
#    repeated_average(
#      c(
#        "filename",
#        "Context",
#        "Phone Contrast Asymmetrical (Language)",
#        "Phone Contrast (Language)"
#      ),
#      c("Phone Language (Code)", "Phone Language (Long)", "Phone Contrast"),
#      c("Spectral Distinctness", "Log Spectral Distinctness")
#    )

discr_avgfb_c <- left_join(
  discr_c,
  distances_c,
  by=c("Phone Language (Code)", "Phone Language (Long)", "Phone Contrast",
       "Phone Contrast (Language)")
)


discr_idpreds_c <- left_join(
  discr_c,
  idpreds,
  by = c(
    "Listener Group",
    "Phone Language (Long)",
    "Phone Language (Code)",
    "Phone Contrast", 
    "Phone Contrast (Language)"
  )
) |>
  left_join(distances_c, 
  by=c("Phone Language (Code)", "Phone Language (Long)", "Phone Contrast",
       "Phone Contrast (Language)"))


discr_c <- repeated_average(
  discr,
  c(
    "filename",
    "Context",
    "Phone Contrast Asymmetrical (Language)",
    "Phone Contrast (Language)"
  ),
  c("Listener Group", "Phone Language (Code)", "Phone Language (Long)", "Phone Contrast"),
  c("Accuracy", "Accuracy and Certainty")
) 

discr_preds <- left_join(
  discr,
  distances |> select(filename, `Spectral Distinctness`, `Log Spectral Distinctness`),
  by = "filename"
) |> left_join(
  discr_idpreds_c |>
    rename(`Contrast Accuracy and Certainty`=`Accuracy and Certainty`) %>%
    select(`NeSssKL Overlap (0.00000001)`, `NeSssKL Overlap (0.001)`,
           `Phone Language (Long)`, `Phone Language (Code)`, `Phone Contrast`,
           `Phone Contrast (Language)`, `Listener Group`, `Goodness Difference`,
           Haskins, Dot, Cosine, Euclidean, SSDiff,
           `Same Top Choice`, `Minimum Categorization Strength`,
           `Contrast Accuracy and Certainty`),
  by = c(
    "Phone Language (Long)",
    "Phone Language (Code)",
    "Phone Contrast",
    "Phone Contrast (Language)",
    "Listener Group")) |>
  left_join(
      rename(discr_f, `Filename Accuracy and Certainty`=`Accuracy and Certainty`,
             `Filename Accuracy`=Accuracy),
      by=c("filename", "Listener Group")
    ) |>
  left_join(
    distances_c |> rename(
      `Spectral Distinctness (Averaged)`=`Spectral Distinctness`,
      `Log Spectral Distinctness (Averaged)`=`Log Spectral Distinctness`
      ),
    by = c(
      "Phone Language (Long)",
      "Phone Language (Code)",
      "Phone Contrast",
      "Phone Contrast (Language)")
  )
  