source("setup.R")

options(mc.cores=4)


discr_by_mct_same_overlap_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
  aes(
    x = `Minimum Categorization Strength`,
    fill = `NeSssKL Overlap (0.001)`,
    y = `Accuracy` # ,
    #    label = `Phone Contrast`
    #    label=round(`NeSssKL Overlap (0.001)`, 2)
    #label=round(Accuracy, 2)
  )
) +
  geom_point(size=2.5, stroke=0.7, pch=21) +
#  geom_label() +
  scale_fill_distiller(palette = "RdYlBu",
                       limits=c(0,1),
                       breaks=c(`0`=0,`.25`=.25,`.5`=.5,`.75`=.75, `1`=1)
  ) +
  scale_x_continuous(labels=percent) +
  coord_cartesian(xlim=c(.3,1)) + 
  facet_grid(~ `Listener Group`) +
  cp_theme() 

print(discr_by_mct_same_overlap_plot)
ggsave(
  "discr_by_mct_same_overlap_plot.png",
  plot = discr_by_mct_same_overlap_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
  
)


discr_by_mct_same_cg_plot <- ggplot(
  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
  aes(
    x = `Minimum Categorization Strength`,
    fill = `Goodness Difference`,
    y = `Accuracy`  #,
    #label = `Phone Contrast`
    #    label=round(`NeSssKL Overlap (0.001)`, 2)
    #label=round(Accuracy, 2)
  )
) +
  geom_point(size=2.5, stroke=0.7, pch=21) +
#  geom_label() +
  scale_fill_distiller(palette = "PRGn",
                       limits=c(0,1.25),
                       breaks=c(`0`=0,`.25`=.25,`.75`=.75, `1.25`=1.25)
  ) +
  scale_x_continuous(labels=percent) +
  coord_cartesian(xlim=c(.3,1)) + 
  facet_grid(~ `Listener Group`) +
  cp_theme() 

print(discr_by_mct_same_cg_plot)
ggsave(
  "discr_by_mct_same_cg_plot.png",
  plot = discr_by_mct_same_cg_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
  
)


model_same_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_null_acc"), "", "bernoulli")
model_same_null_acc <- add_criterion(model_same_null_acc, "loo", file = get_filename("same_null_acc"))


model_same_mcs_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Minimum.Categorization.Strength*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_mcs_acc"), "", "bernoulli")
model_same_mcs_acc <- add_criterion(model_same_mcs_acc, "loo", file = get_filename("same_mcs_acc"))

model_same_cg_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Goodness.Difference*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_cg_acc"), "", "bernoulli")
model_same_cg_acc <- add_criterion(model_same_cg_acc, "loo", file = get_filename("same_cg_acc"))



model_same_mcs_cg_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Goodness.Difference*Minimum.Categorization.Strength*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_mcs_cg_acc"), "", "bernoulli")
model_same_mcs_cg_acc <- add_criterion(model_same_mcs_cg_acc, "loo", file = get_filename("same_mcs_cg_acc"))





plot_same_mcs_cg_acc_model <- mcmc_areas(model_same_mcs_cg_acc,
                                              pars=names(model_same_mcs_cg_acc$fit)[1:10],
                                              prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Intercept",
    "Goodness Difference",
    "Minimum Categorization Strength",
    "Listener Group\n(French - English)",
    "Experimental Trial\n(per full session)",
    "Goodness Difference ×\nMinimum Categorization Strength",
    "Goodness Difference ×\nListener Group",
    "Minimum Categorization Strength ×\nListener Group",
    "Listener Group ×\nExperimental Trial",
    "Listener Group ×\nGoodness Difference ×\nMinimum Categorization Strength"
  )),
  limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

print(plot_same_mcs_cg_acc_model)
ggsave("plot_same_mcs_cg_acc_model.png",
       plot=plot_same_mcs_cg_acc_model,
       width=5, height=8, units="in", dpi=600)

loo_compare(model_same_mcs_cg_acc, model_same_mcs_acc, model_same_cg_acc, model_same_null_acc) |>
  as.data.frame() |>
  mutate(elpd_ses=elpd_diff/se_diff)

model_same_noise_ceiling_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group*Contrast.Accuracy.and.Certainty +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_noise_ceiling_acc"), "", "bernoulli")
model_same_noise_ceiling_acc <- add_criterion(model_same_noise_ceiling_acc, "loo", file = get_filename("same_noise_ceiling_acc"))


loo_compare(model_same_noise_ceiling_acc, model_same_mcs_cg_acc, model_same_mcs_acc, model_same_cg_acc, model_same_null_acc)



model_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("null_acc"), "", "bernoulli")
loo_model_null_acc <- loo(model_null_acc)
elpd_model_null_acc <- loo_model_null_acc$pointwise[,"elpd_loo"]
elpd_model_null_acc_same <- elpd_model_null_acc[discr_preds$`Same Top Choice` == "Yes"]

model_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
loo_model_overlap_acc <- loo(model_overlap_acc)
elpd_model_overlap_acc <- loo_model_overlap_acc$pointwise[,"elpd_loo"]
elpd_model_overlap_acc_same <- elpd_model_overlap_acc[discr_preds$`Same Top Choice` == "Yes"]

elpd_diff_same <- elpd_model_overlap_acc_same - elpd_model_null_acc_same
elpd_sumdiff_same <- sum(elpd_diff_same)
se_diff_same <- sqrt(length(elpd_diff_same)) * sd(elpd_diff_same)
print(elpd_sumdiff_same)
print(se_diff_same)
print(elpd_sumdiff_same/se_diff_same)

#stop()
#
#ggplot(
#  #filter(discr_preds, `Same Top Choice` == "Yes") |>
#  discr_preds |>
#    summarize(Accuracy=mean(Accuracy),
#              `Δ DTW Mel Filterbank`=mean(`Δ DTW Mel Filterbank`),
#              .by=c(`Phone Contrast (Language)`, `Listener Group`)),
#  aes(
#    x = `Δ DTW Mel Filterbank`,
#    y = `Accuracy`  #,
#    #label = `Phone Contrast`
#    #    label=round(`NeSssKL Overlap (0.001)`, 2)
#    #label=round(Accuracy, 2)
#  )
#) +
#  geom_point(size=2.5, stroke=0.7, pch=21) +
#  #  geom_label() +
##  scale_fill_distiller(palette = "PRGn",
##                       limits=c(0,1.25),
##                       breaks=c(`0`=0,`.25`=.25,`.75`=.75, `1.25`=1.25)
##  ) +
##  scale_x_continuous(labels=percent) +
##  coord_cartesian(xlim=c(.3,1)) + 
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#
#
#
#
#discr_by_mct_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
#  aes(
#    x = `Maximum Categorization Threshold`,
#    y = `Accuracy and Certainty`
#  )
#) +
#  geom_point(size=2.5, stroke=0.7) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() +
#  coord_cartesian(ylim = c(0, 3))
#
#print(discr_by_mct_plot)
#ggsave(
#  "discr_by_mct_same_plot.png",
#  plot = discr_by_mct_plot,
#  width = 6.52,
#  height = 2.75,
#  units = "in",
#  dpi = 600
#)
#
#
#
#model_same_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_mct"), "", "ordered")
#model_same_mct <- add_criterion(model_same_mct, "loo", file = get_filename("same_mct"))
#
#model_same_overlap <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Phonological.Overlap*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap"), "", "ordered", centre_overlap = TRUE)
#model_same_overlap <- add_criterion(model_same_overlap, "loo", file = get_filename("same_overlap"))
#
#model_same_max_c <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Listener.Group*Contrast.Accuracy.and.Certainty +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_max_c"), "", "ordered")
#model_same_max_c <- add_criterion(model_same_max_c, "loo", file = get_filename("same_max_c"))
#
#print(loo_compare(model_same_mct, model_same_max_c))
#print(loo_compare(model_same_mct, model_same_overlap))
#print(loo_compare(model_same_mct, model_same_null))
#
#
#
#overlap_by_goodness_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
#  aes(
#    x = `NeSssKL Overlap (0.001)`,
#    y = `Goodness Difference`
#  )
#) +
#  geom_point(size=2.5, stroke=0.7) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#print(overlap_by_goodness_plot)
#
#
#
#discr_by_overlap_mct_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes")  %>% arrange(Accuracy),
#  aes(
#    y = `NeSssKL Overlap (0.001)`,
#    fill = `Accuracy`,
#    x = `Maximum Categorization Threshold`
#  )
#) +
#  geom_point(size=3, stroke=0.7, pch=21, alpha=0.8) +
#  scale_fill_distiller(palette = "PiYG") +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#print(discr_by_overlap_mct_plot)
#
#
#
#
#discr_by_overlap_same_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes") ,
#  aes(
#    x = `NeSssKL Overlap (0.001)`,
#    y = `Accuracy`
#  )
#) +
#  geom_point(size=3, stroke=0.7, pch=21, alpha=0.8) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#
#print(discr_by_overlap_same_plot)
#
#
#
#discr_by_mct_same_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
#  aes(
#    y = `Accuracy`,
#    x = `Maximum Categorization Threshold`
#  )
#) +
#  geom_point(size=3, stroke=0.7, pch=21, alpha=0.8) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#print(discr_by_mct_same_plot)
#
#
#discr_by_good_mct_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes") %>% arrange(Accuracy),
#  aes(
#    y = `Goodness Difference`,
#    fill = `Accuracy`,
#    x = `Maximum Categorization Threshold`
#  )
#) +
#  geom_point(size=3, stroke=0.7, pch=21, alpha=0.7) +
#  scale_fill_distiller(palette = "PiYG") +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#print(discr_by_good_mct_plot)
#
#
#discr_by_good_same_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
#  aes(
#    x = `Goodness Difference`,
#    y = `Accuracy`
#  )
#) +
#  geom_point(size=3, stroke=0.7, pch=21, alpha=0.8) +
#  facet_grid(~ `Listener Group`) +
#  cp_theme() 
#
#
#print(discr_by_good_same_plot)
#
#
#discr_by_good_mct_plot <- ggplot(
#  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
#  aes(
#    x = `Goodness Difference`,
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
#
#
#plots_discr_by_overlap_good_mct <- discr_by_overlap_mct_plot /
#  discr_by_good_mct_plot + 
#  plot_annotation(tag_level="a")
#
#print(plots_discr_by_overlap_good_mct)
#ggsave("discr_by_overlap_goodness_mct_same.png",
#       plot=plots_discr_by_overlap_good_mct,
#       width=6, height=6, units="in", dpi=600)
#
#
#model_same_overlap_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap_mct"), "", "ordered", centre_overlap = TRUE)
#model_same_overlap_mct <- add_criterion(model_same_overlap_mct, "loo")
#
#print(loo_compare(model_same_overlap_mct, model_same_max_c))
#print(loo_compare(model_same_overlap_mct, model_same_null))
#
#
#
#model_same_overlap_goodness_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
#                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap_goodness_mct"), "", "ordered", centre_overlap = TRUE)
#model_same_overlap_goodness_mct <- add_criterion(model_same_overlap_goodness_mct, "loo")
#
#print(loo_compare(model_same_overlap_goodness_mct, model_same_max_c))
#print(loo_compare(model_same_overlap_goodness_mct, model_same_null))
#
#model_same_goodness_mct <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_goodness_mct"), "", "ordered", centre_overlap = TRUE)
#model_same_goodness_mct <- add_criterion(model_same_goodness_mct, "loo")
#
#print(loo_compare(model_same_goodness_mct, model_same_max_c))
#print(loo_compare(model_same_goodness_mct, model_same_null))
#
#
#
##discr_by_mct_same_plot <- ggplot(
##  filter(discr_idpreds_c, `Same Top Choice` == "Yes"),
##  aes(
##    x = `Minimum Categorization Strength`,
##    y = Accuracy
##  )
##) +
##  geom_point() +
##  scale_x_continuous(labels=percent) +
##  facet_grid(~ `Listener Group`) +
##  cp_theme() 
##
##print(discr_by_mct_same_plot)
#
#
##highlight_scatterplot(
##  discr_idpreds_c |>
##    filter(`Same Top Choice` == "Yes") |>
##    mutate(
##      `Point Type`=ifelse(
##        `Phone Contrast (Language)` %in% PHONES_SIMILAR,
##        "T0", ifelse(
##          `Phone Contrast (Language)` %in%  PHONES_FRENCH, "T1",
##          ifelse(
##            `Phone Contrast (Language)` %in% PHONES_ENGLISH,  "T2", "D")))) |>
##    filter(`Point Type` != "D"),
##  "Minimum Categorization Strength",
##  "Accuracy",
##  "Phone Contrast",
##  "Point Type", seed=2) +
##  geom_smooth(data=discr_idpreds_c, mapping=aes(x=`Minimum Categorization Strength`,
##                                             y=`Accuracy`), inherit.aes=FALSE,
##              se=FALSE, fullrange=TRUE,
##              lty="dashed", colour="#00000044", lwd=1) +
##  cp_theme() +
##  facet_grid(~ `Listener Group`) +
##  theme(legend.position = "none") 
#
#
#