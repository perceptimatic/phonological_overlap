source("pathnames.R")
source("aggregation.R")
source("cleanup.R")
source("identification.R")
source("discrimination.R")
source("plotting.R")
source("regression.R")
source("delta.R")

discr_by_contrast_pam_overlap <- left_join(
  discriminability_by_contrast,
  pam_overlap,
  by = c(
    "Listener Group",
    "Phone Language (Long)",
    "Phone Language (Code)",
    "Phone Contrast (Language)"
  )
)