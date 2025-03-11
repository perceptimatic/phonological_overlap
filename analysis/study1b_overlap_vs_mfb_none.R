source("setup.R")

options(mc.cores=4)

model_specs <- list(
  null = list(
    formula = formula(
      "Accuracy.and.Certainty ~
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"
    ),
    dvmode = "ordered"
  ),
  overlap = list(
    formula = formula(
      "Accuracy.and.Certainty ~
                    Overlap*Listener.Group + Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"
    ),
    dvmode = "ordered"
  ),
  dfb = list(
    formula = formula(
      "Accuracy.and.Certainty ~
                    Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"
    ),
    dvmode = "ordered"
  ),  
  overlap_dfb = list(
    formula = formula(
      "Accuracy.and.Certainty ~
                    Overlap*Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) +
                    (1 + Listener.Group|filename)"
    ),
    dvmode = "ordered"
  )
)

models <- foreach(
  m = names(model_specs),
  .final = function(x)
    setNames(x, names(model_specs))
) %do% {
  run_brms_model(model_specs[[m]][["formula"]],
                 discr_preds,
                 get_filename(m),
                 -1,
                 "ordered")
}

models <- foreach(
  m = names(model_specs),
  .final = function(x)
    setNames(x, names(model_specs))
) %do% {
  add_criterion(models[[m]], "loo", file = get_filename(m))
}


print(models)

