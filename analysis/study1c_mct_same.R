source("setup.R")

options(mc.cores=4)

discr_by_mct_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
  aes(
    x = `Maximum Categorization Threshold`,
    y = `Accuracy and Certainty`
  )
) +
  geom_point(size=2.5, stroke=0.7) +
  facet_grid(~ `Listener Group`) +
  cp_theme() +
  coord_cartesian(ylim = c(0, 3))

print(discr_by_mct_plot)
ggsave(
  "discr_by_mct_same_plot.png",
  plot = discr_by_mct_plot,
  width = 6.52,
  height = 2.75,
  units = "in",
  dpi = 600
)


model_same_null <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_null"), "", "ordered")
model_same_null <- add_criterion(model_same_null, "loo", file = get_filename("same_null"))


model_same_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_mct"), "", "ordered")
model_same_mct <- add_criterion(model_same_mct, "loo", file = get_filename("same_mct"))

model_same_overlap <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Phonological.Overlap*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap"), "", "ordered", centre_overlap = TRUE)
model_same_overlap <- add_criterion(model_same_overlap, "loo", file = get_filename("same_overlap"))

model_same_max_c <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Listener.Group*Contrast.Accuracy.and.Certainty +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_max_c"), "", "ordered")
model_same_max_c <- add_criterion(model_same_max_c, "loo", file = get_filename("same_max_c"))

print(loo_compare(model_same_mct, model_same_max_c))
print(loo_compare(model_same_mct, model_same_overlap))
print(loo_compare(model_same_mct, model_same_null))

discr_by_overlap_mct_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
  aes(
    x = `Phonological Overlap`,
    y = `Accuracy and Certainty`,
    alpha = `Maximum Categorization Threshold`
  )
) +
  geom_point(size=2.5, stroke=0.7) +
  facet_grid(~ `Listener Group`) +
  cp_theme() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 3))


discr_by_good_mct_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
  aes(
    x = `Goodness Difference`,
    y = `Accuracy and Certainty`,
    alpha = `Maximum Categorization Threshold`
  )
) +
  geom_point(size=2.5, stroke=0.7) +
  facet_grid(~ `Listener Group`) +
  cp_theme() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0, b = 0),
    legend.spacing.y = unit(0, "in")
  ) +
  coord_cartesian(ylim = c(0, 3))


plots_discr_by_overlap_good_mct <- discr_by_overlap_mct_plot /
  discr_by_good_mct_plot + 
  plot_annotation(tag_level="a")

print(plots_discr_by_overlap_good_mct)
ggsave("discr_by_overlap_goodness_mct_same.png",
       plot=plots_discr_by_overlap_good_mct,
       width=6, height=6, units="in", dpi=600)


model_same_overlap_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap_mct"), "", "ordered", centre_overlap = TRUE)
model_same_overlap_mct <- add_criterion(model_same_overlap_mct, "loo")

print(loo_compare(model_same_overlap_mct, model_same_max_c))
print(loo_compare(model_same_overlap_mct, model_same_null))



model_same_overlap_goodness_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap_goodness_mct"), "", "ordered", centre_overlap = TRUE)
model_same_overlap_goodness_mct <- add_criterion(model_same_overlap_goodness_mct, "loo")

print(loo_compare(model_same_overlap_goodness_mct, model_same_max_c))
print(loo_compare(model_same_overlap_goodness_mct, model_same_null))

model_same_goodness_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_goodness_mct"), "", "ordered", centre_overlap = TRUE)
model_same_goodness_mct <- add_criterion(model_same_goodness_mct, "loo")

print(loo_compare(model_same_goodness_mct, model_same_max_c))
print(loo_compare(model_same_goodness_mct, model_same_null))

