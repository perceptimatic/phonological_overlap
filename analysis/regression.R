get_filename <- function(model_name) {
  return(paste0("model_", model_name))
}

makenamesize <- function(d) {
  names(d) <- make.names(colnames(d))
  return(d)
}


prep_data <- function(d, dvmode="bernoulli",  gdmean=0.39, gdsd=0.31, tnmedian=88, tnmax=179,
                           centre_overlap=FALSE) {
  # FIXME
  
  if (dvmode == "ordered") {
    d <- mutate(d,
                `Accuracy and Certainty` = factor(`Accuracy and Certainty`, ordered = TRUE))
  } else if (dvmode == "binarized") {
    d <- mutate(d, `Accuracy and Certainty` = `Accuracy and Certainty` / 6 + 0.5)
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
  if ("Spectral Distinctness" %in% names(d))
    d[["Spectral Distinctness"]] <- d[["Spectral Distinctness"]]/0.05
  if ("Spectral Distinctness (Averaged)" %in% names(d))
    d[["Spectral Distinctness (Averaged)"]] <- d[["Spectral Distinctness (Averaged)"]]/0.05
  

  if ("Goodness Difference" %in% names(d))
    d[["Goodness Difference"]] <- (d$`Goodness Difference` - gdmean)/gdsd
  if ("Trial Number" %in% names(d))
    d[["Trial Number"]] <- (d$`Trial Number` - tnmedian)/tnmax
  if (centre_overlap) {
    if ("Phonological Overlap" %in% names(d))
      d[["Phonological Overlap"]] = (d$`Phonological Overlap` - mean(d$`Phonological Overlap`))/sd(d$`Phonological Overlap`)  
    if ("NeSssKL Overlap (0.001)" %in% names(d))
      d[["NeSssKL Overlap (0.001)"]] = (d$`NeSssKL Overlap (0.001)` - mean(d$`NeSssKL Overlap (0.001)`))/sd(d$`NeSssKL Overlap (0.001)`)  
    if ("NeSssKL Overlap (0.0000001)" %in% names(d))
      d[["NeSssKL Overlap (0.00000001)"]] = (d$`NeSssKL Overlap (0.00000001)` - mean(d$`NeSssKL Overlap (0.00000001)`))/sd(d$`NeSssKL Overlap (0.00000001)`)  
  }
  return(makenamesize(d))
}

run_brms_model <- function(f, d, filename, gpuid, dvmode,
                           gdmean=0.39, gdsd=0.31, tnmedian=88, tnmax=179,
                           centre_overlap=FALSE,
                           chains=4, sample_prior="no", iter=2000,
                           warmup=1000,
                           prior=NULL) {
  d <- prep_data(d, dvmode, gdmean, gdsd, tnmedian, tnmax, centre_overlap)

  
  if (dvmode == "ordered") {
    family <- "cumulative"
  } else if (dvmode == "binarized") {
    family <- gaussian(link = "logit")
  } else if (dvmode == "bernoulli") {
    family <- bernoulli(link = "logit")
  }

  if (gpuid != "") {
    if (!is.null(prior)) {
      m <- brm(
        f,
        file = filename,
        data = d,
        family = family,
        save_pars = save_pars(all = TRUE),
        backend = "cmdstanr",
        opencl = eval(parse(text = gpuid)),
        stan_model_args = list(stanc_options = list("O1")),
        chains=chains,
        sample_prior=sample_prior,
        warmup=warmup,
        prior=prior,
        iter=iter
      )
    } else {
      m <- brm(
        f,
        file = filename,
        data = d,
        family = family,
        save_pars = save_pars(all = TRUE),
        backend = "cmdstanr",
        opencl = eval(parse(text = gpuid)),
        stan_model_args = list(stanc_options = list("O1")),
        chains=chains,
        warmup=warmup,
        sample_prior=sample_prior,
        iter=iter
      )      
    }
  } else {
    if (!is.null(prior)) {
      m <- brm(
        f,
        file = filename,
        data = d,
        family = family,
        save_pars = save_pars(all = TRUE),
        backend = "cmdstanr",
        warmup=warmup,
        stan_model_args = list(stanc_options = list("O1")),
        chains=chains,
        sample_prior=sample_prior,
        prior=prior,
        iter=iter
      )
    } else {
      m <- brm(
        f,
        file = filename,
        data = d,
        family = family,
        save_pars = save_pars(all = TRUE),
        backend = "cmdstanr",
        stan_model_args = list(stanc_options = list("O1")),
        chains=chains,
        warmup=warmup,
        sample_prior=sample_prior,
        iter=iter
      )      
    }
  }
  return(m)
}
