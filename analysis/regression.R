get_filename <- function(model_name) {
  return(paste0("model_", model_name))
}

makenamesize <- function(d) {
  names(d) <- make.names(colnames(d))
  return(d)
}

run_brms_model <- function(f, d, filename, gpuid, dvmode,
                           gdmean=0.39, gdsd=0.31, tnmedian=88, tnmax=179,
                           centre_overlap=FALSE,
                           chains=4) {
  if (dvmode == "ordered") {
    d <- mutate(d,
                `Accuracy and Certainty` = factor(`Accuracy and Certainty`, ordered = TRUE))
    family <- "cumulative"
  } else if (dvmode == "binarized") {
    d <- mutate(d, `Accuracy and Certainty` = `Accuracy and Certainty` / 6 + 0.5)
    family <- gaussian(link = "logit")
  } else if (dvmode == "bernoulli") {
    family <- bernoulli(link = "logit")
  }
  if ("Listener Group" %in% names(d))
    d[["Listener Group"]] <- ifelse(d$`Listener Group` == "English", -1 / 2, 1 / 2)
  if ("Maximum Categorization Threshold" %in% names(d))
#    d[["Maximum Categorization Threshold"]] <- (d$`Maximum Categorization Threshold` - 0.5) / 0.1
    d[["Maximum Categorization Threshold"]] <- qlogis(d$`Maximum Categorization Threshold`)
  if ("Haskins" %in% names(d))
    d[["Haskins"]] <- 2*d$Haskins
  if ("Δ DTW Mel Filterbank" %in% names(d))
    d[["Δ DTW Mel Filterbank"]] <- d[["Δ DTW Mel Filterbank"]]/0.05
  if ("Δ DTW Mel Filterbank (Phone Contrast)" %in% names(d))
    d[["Δ DTW Mel Filterbank (Phone Contrast)"]] <- d[["Δ DTW Mel Filterbank (Phone Contrast)"]]/0.05
  if ("Goodness Difference" %in% names(d))
    d[["Goodness Difference"]] <- (d$`Goodness Difference` - gdmean)/gdsd
  if ("Trial Number" %in% names(d))
    d[["Trial Number"]] <- (d$`Trial Number` - tnmedian)/tnmax
  if (centre_overlap)
    d[["Phonological Overlap"]] = (d$`Phonological Overlap` - mean(d$`Phonological Overlap`))/sd(d$`Phonological Overlap`)
  if (gpuid != "") {
    m <- brm(
      f,
      file = filename,
      data = makenamesize(d),
      family = family,
      save_pars = save_pars(all = TRUE),
      backend = "cmdstanr",
      opencl = eval(parse(text = gpuid)),
      stan_model_args = list(stanc_options = list("O1")),
      chains=chains
    )
  } else {
    m <- brm(
      f,
      file = filename,
      data = makenamesize(d),
      family = family,
      save_pars = save_pars(all = TRUE),
      backend = "cmdstanr",
      stan_model_args = list(stanc_options = list("O1")),
      chains=chains
    )
  }
  return(m)
}
