source("setup.R")

options(mc.cores=4)

discr_by_mct_diff_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "No"),
  aes(
    x = `Maximum Categorization Threshold`,
    y = `Accuracy and Certainty`
  )
) +
  geom_point(size=2.5, stroke=0.7, shape=21) +
  facet_grid(~ `Listener Group`) +
  cp_theme() +
  coord_cartesian(ylim = c(0, 3))

print(discr_by_mct_diff_plot)
ggsave(
  "discr_by_mct_diff_plot.png",
  plot = discr_by_mct_diff_plot,
  width = 6.52,
  height = 2.75,
  units = "in",
  dpi = 600
)


model_diff_null <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_null"), "", "ordered")
model_diff_null <- add_criterion(model_diff_null, "loo", file = get_filename("diff_null"))

model_diff_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_mct"), "", "ordered")
model_diff_mct <- add_criterion(model_diff_mct, "loo", file = get_filename("diff_mct"))

loo_compare(model_diff_null, model_diff_mct)




discr_by_overlap_mct_diff_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "No"),
  aes(
    x = `Phonological Overlap`,
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
print(discr_by_overlap_mct_diff_plot)

ggsave(
 "discr_by_overlap_mct_diff_plot.png",
  plot = discr_by_overlap_mct_diff_plot,
  width = 6.52,
  height = 2.75,
  units = "in",
  dpi = 600
)

model_diff_overlap <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Phonological.Overlap*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap"), "", "ordered")
model_diff_overlap <- add_criterion(model_diff_overlap, "loo", file = get_filename("diff_overlap"))

model_diff_overlap_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap_mct"), "", "ordered", centre_overlap = TRUE)
model_diff_overlap_mct <- add_criterion(model_diff_overlap_mct, "loo")

loo_compare(model_diff_overlap, model_diff_overlap_mct)
loo_compare(model_diff_mct, model_diff_overlap_mct)
