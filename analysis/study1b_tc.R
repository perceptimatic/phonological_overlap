source("setup.R")

options(mc.cores=4)

discr_by_mct_diff_overlap_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "No"),
  aes(
    x = `Minimum Categorization Strength`,
    fill = `NeSssKL Overlap (0.001)`,
    y = `Accuracy`,
#    label = `Phone Contrast`
#    label=round(`NeSssKL Overlap (0.001)`, 2)
#    label=round(Accuracy, 2)
  )
) +
  geom_point(size=2.5, stroke=0.7, pch=21) +
#  geom_label() +
  scale_fill_distiller(palette = "RdYlBu",
                       limits=c(0,1),
                       breaks=c(`0`=0,`.25`=.25,`.5`=.5,`.75`=.75, `1`=1)
                       ) +
  scale_x_continuous(labels=percent) +
  facet_grid(~ `Listener Group`) +
  cp_theme() 

print(discr_by_mct_diff_overlap_plot)
ggsave(
  "discr_by_mct_diff_overlap_plot.png",
  plot = discr_by_mct_diff_overlap_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)


model_diff_mcs_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Minimum.Categorization.Strength*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_mcs_acc"), "", "bernoulli")
model_diff_mcs_acc <- add_criterion(model_diff_mcs_acc, "loo", file = get_filename("diff_mcs_acc"))


print(model_diff_mcs_acc)

plot_diff_mcs_acc_model <- mcmc_areas(model_diff_mcs_acc,
                                     pars=names(model_diff_mcs_acc$fit)[1:6],
                                     prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Intercept",
    "Minimum Categorization Strength",
    "Listener Group\n(French - English)",
    "Experimental Trial\n(per full session)",
    "Minimum Categorization Strength ×\nListener Group",
    "Listener Group ×\nExperimental Trial")),
    limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

print(plot_diff_mcs_acc_model)
ggsave("plot_diff_mcs_acc_coefs.png",
       plot=plot_diff_mcs_acc_model,
       width=5, height=5, units="in", dpi=600)

#conditional_effects(model_diff_mcs_acc, effects="Minimum.Categorization.Strength:Listener.Group")


model_diff_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_null_acc"), "", "bernoulli")
model_diff_null_acc <- add_criterion(model_diff_null_acc, "loo", file = get_filename("diff_null_acc"))


print(loo_compare(model_diff_null_acc, model_diff_mcs_acc))
print(loo_compare(model_diff_null_acc, model_diff_mcs_acc)[2,1]/
        loo_compare(model_diff_null_acc, model_diff_mcs_acc)[2,2])


model_diff_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    NeSssKL.Overlap..0.001.*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap_acc"), "", "bernoulli")
model_diff_overlap_acc <- add_criterion(model_diff_overlap_acc, "loo",
                                            file = get_filename("diff_overlap_acc"))

model_diff_mcs_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    NeSssKL.Overlap..0.001.*Minimum.Categorization.Strength*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap_mcs_acc"), "", "bernoulli")
model_diff_mcs_overlap_acc <- add_criterion(model_diff_mcs_overlap_acc, "loo",
                                            file = get_filename("diff_overlap_mcs_acc"))




plot_diff_mcs_overlap_acc_model <- mcmc_areas(model_diff_mcs_overlap_acc,
                                      pars=names(model_diff_mcs_overlap_acc$fit)[1:10],
                                      prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Intercept",
    "NeSssKL Overlap (0.001)",
    "Minimum Categorization Strength",
    "Listener Group\n(French - English)",
    "Experimental Trial\n(per full session)",
    "NeSssKL Overlap (0.001) ×\nMinimum Categorization Strength",
    "NeSssKL Overlap (0.001) ×\nListener Group",
    "Minimum Categorization Strength ×\nListener Group",
    "Listener Group ×\nExperimental Trial",
    "Listener Group ×\nNeSssKL Overlap (0.001) ×\nMinimum Categorization Strength"
    )),
    limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

print(plot_diff_mcs_overlap_acc_model)
ggsave("plot_diff_mcs_overlap_acc_coefs.png",
       plot=plot_diff_mcs_overlap_acc_model,
       width=5, height=8, units="in", dpi=600)


print(loo_compare(model_diff_mcs_overlap_acc, model_diff_overlap_acc))
print(loo_compare(model_diff_mcs_overlap_acc, model_diff_overlap_acc)[2,1]/
        loo_compare(model_diff_mcs_overlap_acc, model_diff_overlap_acc)[2,2])

conditional_effects(model_diff_mcs_overlap_acc, effects="NeSssKL.Overlap..0.001.:Minimum.Categorization.Strength", conditions=data.frame(Listener.Group=c(0.5, -.5)))

#discr_by_mct_diff_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "No"),
#  aes(
#    x = `Minimum Categorization Strength`,
#    y = Accuracy
#  )
#) +
#  geom_point() +
#  scale_x_continuous(labels=percent) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#print(discr_by_mct_diff_plot)

#highlight_scatterplot(
#  discr_idpreds_c |>
#    filter(`Same Top Choice` == "No") |>
#    mutate(
#      `Point Type`=ifelse(
#        `Phone Contrast (Language)` %in% PHONES_SIMILAR,
#        "T0", ifelse(
#          `Phone Contrast (Language)` %in%  PHONES_FRENCH, "T1",
#          ifelse(
#            `Phone Contrast (Language)` %in% PHONES_ENGLISH,  "T2", "D")))) |>
#    filter(`Point Type` != "D"),
#  "Minimum Categorization Strength",
#  "Accuracy",
#  "Phone Contrast",
#  "Point Type", seed=2) +
#  geom_smooth(data=discr_idpreds_c, mapping=aes(x=`Minimum Categorization Strength`,
#                                             y=`Accuracy`), inherit.aes=FALSE,
#              se=FALSE, fullrange=TRUE,
#              lty="dashed", colour="#00000044", lwd=1) +
#  cp_theme() +
#  facet_grid(~ `Listener Group`) +
#  theme(legend.position = "none") 



#
#model_diff_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_mct"), "", "ordered")
#model_diff_mct <- add_criterion(model_diff_mct, "loo", file = get_filename("diff_mct"))
#

#
#
#
#discr_by_overlap_mct_diff_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "No"),
#  aes(
#    x = `Phonological Overlap`,
#    y = `Accuracy and Certainty`,
#    alpha = `Maximum Categorization Threshold`
#  )
#) +
#  geom_point(size=2.5, stroke=0.7) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() +
#  theme(
#    legend.position = "bottom",
#    legend.box = "vertical",
#    legend.margin = margin(t = 0, b = 0),
#    legend.spacing.y = unit(0, "in")
#  ) +
#  coord_cartesian(ylim = c(0, 3))
#print(discr_by_overlap_mct_diff_plot)
#
#ggsave(
# "discr_by_overlap_mct_diff_plot.png",
#  plot = discr_by_overlap_mct_diff_plot,
#  width = 6.52,
#  height = 2.75,
#  units = "in",
#  dpi = 600
#)
#
#model_diff_overlap <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Phonological.Overlap*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap"), "", "ordered")
#model_diff_overlap <- add_criterion(model_diff_overlap, "loo", file = get_filename("diff_overlap"))
#
#model_diff_overlap_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "No"), get_filename("diff_overlap_mct"), "", "ordered", centre_overlap = TRUE)
#model_diff_overlap_mct <- add_criterion(model_diff_overlap_mct, "loo")
#
#loo_compare(model_diff_overlap, model_diff_overlap_mct)
#loo_compare(model_diff_mct, model_diff_overlap_mct)
