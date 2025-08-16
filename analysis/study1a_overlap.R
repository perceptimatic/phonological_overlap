source("setup.R")

options(mc.cores=4)

discr_by_overlap_plot <- ggplot(
  discr_idpreds_c,
  aes(
    x = `NeSssKL Overlap (0.001)`,
    y = `Accuracy`,
    fill = `Same Top Choice`
  )
) +
  geom_point(shape = 21, size=2.5, stroke=0.7) +
  facet_grid(~ `Listener Group`, scales = "free_x") +
  cp_theme() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0, b = 0),
    legend.spacing.y = unit(0, "in")
  )  +
  scale_fill_manual(values = c(No = "#ffffff00", Yes = "#111111aa")) +
  coord_cartesian(xlim = c(0, 1))

print(discr_by_overlap_plot)
ggsave(
  "discr_by_overlap_plot.png",
  plot = discr_by_overlap_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)



discr_by_overlap_highlight_plot <- highlight_scatterplot(
  discr_idpreds_c |>
    mutate(
      `Point Type`=ifelse(
        `Phone Contrast (Language)` %in% PHONES_SIMILAR,
        "T0", ifelse(
          `Phone Contrast (Language)` %in%  PHONES_FRENCH, "T1",
          ifelse(
            `Phone Contrast (Language)` %in% PHONES_ENGLISH,  "T2", "D")))) |>
    filter(`Point Type` != "D"),
  "NeSssKL Overlap (0.001)",
  "Accuracy",
  "Phone Contrast",
  "Point Type", seed=2) + 
  cp_theme() +
  theme(legend.position = "none") + 
  geom_smooth(data=discr_idpreds_c,
              aes(y=`Accuracy`, x=`NeSssKL Overlap (0.001)`),
              colour="#00000044", lwd=0.5,
              se=FALSE,
              inherit.aes=FALSE) +
  facet_wrap(~ `Listener Group`) +
  coord_cartesian(xlim=c(0,1), ylim=c(0.5,1))

print(discr_by_overlap_highlight_plot)

ggsave("discr_by_overlap_highlight_plot.png",
       plot=discr_by_overlap_highlight_plot,
       width = 6.52,
       height = 4.5,
       units="in", dpi=600)


model_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("null_acc"), "", "bernoulli")
model_null_acc <- add_criterion(model_null_acc, "loo", file = get_filename("null_acc"))


model_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
model_overlap_acc <- add_criterion(model_overlap_acc, "loo", file = get_filename("overlap_acc"))

plot_overlap_acc_model <- mcmc_areas(model_overlap_acc,
                                     pars=names(model_overlap_acc$fit)[1:6],
                                    prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Intercept",
    "Phonological Overlap",
    "Listener Group\n(French - English)",
    "Experimental Trial\n(per full session)",
    "Phonological Overlap ×\nListener Group",
    "Listener Group ×\nExperimental Trial")),
    limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

print(plot_overlap_acc_model)
ggsave("plot_overlap_coefs.png",
       plot=plot_overlap_acc_model,
       width=5, height=5, units="in", dpi=600)


print(loo_compare(model_overlap_acc, model_null_acc))
print(loo_compare(model_overlap_acc, model_null_acc)[2,1]/
        loo_compare(model_overlap_acc, model_null_acc)[2,2])


###

model_overlapinf_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    NeSssKL.Overlap..0.00000001.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlapinf_acc"), "", "bernoulli")

model_overlapinf_acc <- add_criterion(model_overlapinf_acc, "loo", file = get_filename("overlapinf_acc"))


model_minsum_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    MinSum*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("minsum_acc"), "", "bernoulli")

model_minsum_acc <- add_criterion(model_minsum_acc, "loo", file = get_filename("minsum_acc"))

