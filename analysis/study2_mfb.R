source("setup.R")

options(mc.cores=4)


model_dfbavg_mcs_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Minimum.Categorization.Strength*Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("dfbavg_mcs_acc"), "", "bernoulli")
model_dfbavg_mcs_acc <- add_criterion(model_dfbavg_mcs_acc, "loo", file = get_filename("dfbavg_mcs_acc"))


model_dfbavg_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("dfbavg_acc"), "", "bernoulli")
model_dfbavg_acc <- add_criterion(model_dfbavg_acc, "loo", file = get_filename("dfbavg_acc"))

stop()



discr_by_mfb_overlap_plot <- ggplot(
  discr_idpreds_c,
  aes(
    x = `Spectral Distinctness`,
    fill = `NeSssKL Overlap (0.001)`,
    y = `Accuracy`
  )
) +
  geom_point(size=2.5, stroke=0.7, pch=21) +
  scale_fill_distiller(palette = "RdYlBu",
                       limits=c(0,1),
                       breaks=c(`0`=0,`.25`=.25,`.5`=.5,`.75`=.75, `1`=1)
  ) +
  facet_grid(~ `Listener Group`) +
  cp_theme() 

print(discr_by_mfb_overlap_plot)
ggsave(
  "discr_by_mfb_overlap_plot.png",
  plot = discr_by_mfb_overlap_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)

model_overlap_dfbavg_acc <- run_brms_model(
  formula(
    "Accuracy~
                    NeSssKL.Overlap..0.001.*Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_dfbavg_acc"), "", "bernoulli")
model_overlap_dfbavg_acc <- add_criterion(model_overlap_dfbavg_acc, "loo", file = get_filename("overlap_dfbavg_acc"))

summary(model_overlap_dfbavg_acc)

#plot_overlap_dfb_coefs <- mcmc_areas(model_overlap_dfbavg_acc,
#                                     pars=names(model_overlap_dfbavg_acc$fit)[1:10],
#                                     prob=0.95, border_size=0.3) + 
#  scale_y_discrete(labels=rev(c(
#    "Intercept",
#    "NeSssKL Overlap (0.001)",
#    "Spectral Distinctness",
#    "Listener Group\n(French - English)",
#    "Experimental Trial\n(per full session)",
#    "NeSssKL Overlap (0.001) ×\nSpectral Distinctness",
#    "NeSssKL Overlap (0.001) ×\nListener Group",
#    "Spectral Distinctness ×\nListener Group",
#    "Experimental Trial ×\nListener Group",
#    "NeSssKL Overlap (0.001) ×\nSpectral Distinctness ×\nListener Group")),
#    limits=rev) +
#  xlab("Coefficient value") +
#  cp_theme()
#print(plot_overlap_dfb_coefs)
#ggsave(
#  "plots_overlap_dfb_coefs.png",
#  plot = plot_overlap_dfb_coefs,
#  width = 5,
#  height = 8,
#  units = "in",
#  dpi = 600
#)


model_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
model_overlap_acc <- add_criterion(model_overlap_acc, "loo", file = get_filename("overlap_acc"))


model_dfbavg_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("dfbavg_acc"), "", "bernoulli")
model_dfbavg_acc <- add_criterion(model_dfbavg_acc, "loo", file = get_filename("dfbavg_acc"))

loo_compare(model_overlap_acc, model_overlap_dfbavg_acc, model_dfbavg_acc) |>
  as.data.frame() |>
  mutate(elpd_ses=elpd_diff/se_diff)



model_overlap_dfb_acc <- run_brms_model(
  formula(
    "Accuracy~
                    Phonological.Overlap*Spectral.Distinctness*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_dfb_acc"), "", "bernoulli")
model_overlap_dfb_acc <- add_criterion(model_overlap_dfb_acc, "loo", file = get_filename("overlap_dfb_acc"))

loo_compare(model_overlap_dfb_acc, model_overlap_dfbavg_acc) |>
  as.data.frame() |>
  mutate(elpd_ses=elpd_diff/se_diff)



model_logdfbavg_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Log.Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("logdfbavg_acc"), "", "bernoulli")
model_logdfbavg_acc <- add_criterion(model_logdfbavg_acc, "loo", file = get_filename("logdfbavg_acc"))

#model_dfb_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("dfb_acc"), "", "bernoulli")
#model_dfb_acc <- add_criterion(model_dfb_acc, "loo", file = get_filename("dfb_acc"))
#



model_dfbavg_mcs_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Minimum.Categorization.Strength*Spectral.Distinctness..Averaged.*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("dfbavg_mcs_acc"), "", "bernoulli")
model_dfbavg_mcs_acc <- add_criterion(model_dfbavg_mcs_acc, "loo", file = get_filename("dfbavg_mcs_acc"))




predictions <- NULL
for (contrast in unique(discr_idpreds_c$`Phone Contrast (Language)`)) {
  preds_overlap_dfbavg <- posterior_epred(
    model_overlap_dfbavg_acc,
    newdata=prep_data(discr_preds |>
                        filter(`Phone Contrast (Language)` == contrast)))
  preds_overlap <- posterior_epred(
    model_overlap_acc,
    newdata=prep_data(discr_preds |>
                        filter(`Phone Contrast (Language)` == contrast) |>
                        rename(`Phonological Overlap`=`NeSssKL Overlap (0.001)`,
                               `Δ DTW Mel Filterbank`=`Spectral Distinctness`)))
  predictions_c <- discr_preds |> filter(`Phone Contrast (Language)` == contrast) |>
    mutate(`Preds Overlap-MFB`=colMeans(preds_overlap_dfbavg),
           `Preds Overlap`=colMeans(preds_overlap)) |>
    repeated_average(c(
      "filename",  "Context", "Phone Contrast Asymmetrical (Language)",
      "Phone Contrast (Language)"
    ),
    c("Listener Group", "Phone Language (Code)", "Phone Language (Long)",
      "Phone Contrast"),
    c("Accuracy",  "NeSssKL Overlap (0.001)", "Preds Overlap-MFB",
      "Preds Overlap")
    )
  predictions <- bind_rows(predictions,predictions_c)
}


pred_change_dfb <- ggplot(left_join(discr_idpreds_c, predictions) |>
         mutate(`Residual Accuracy (Overlap Only)`=Accuracy - `Preds Overlap`,
                `Residual Accuracy (with Spectral Distinctness)`=Accuracy - `Preds Overlap-MFB`,
                `Change in Prediction`=`Preds Overlap-MFB`-`Preds Overlap`,
                `Improvement in Prediction`=
                  abs(`Residual Accuracy (Overlap Only)`)-abs(`Residual Accuracy (with Spectral Distinctness)`)
                  ),
       aes(x=Accuracy,
           label=ifelse(abs(`Improvement in Prediction`) > .02,
                        ifelse(`Phone Contrast` == "e–i", `Phone Contrast (Language)`,
                               `Phone Contrast`),""),
           y=`Change in Prediction`,
          fill=`Improvement in Prediction`)) +
  geom_point(pch=21, size=2) +
  geom_label_repel(seed=2, force=50) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_fill_distiller(palette = "PiYG",
                       limits=c(-0.04,0.04),
                       breaks=c(`-4%`=-.04,`-2%`=-.02,`0`=0,`+2%`=.02, `+4%`=.04),
                       direction=1) +
  facet_grid(~ `Listener Group`) +
  cp_theme()

print(pred_change_dfb)
ggsave(
  "pred_change_dfb.png",
  plot = pred_change_dfb,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)


discr_by_mfb_dfb_plot <- ggplot(
  discr_idpreds_c,
  aes(
    fill = log(
      `Spectral Distinctness` - min(`Spectral Distinctness`)
    ),
    x = `Minimum Categorization Strength`,
    y = `Accuracy`
  )
) +
  geom_point(size=2.5, stroke=0.7, pch=21) +
  scale_y_continuous(labels=percent) +
  scale_fill_distiller(palette = 
                       "RdYlBu"#, RdYlGn, Spectral
                       #limits=c(0,1),
                       #breaks=c(`0`=0,`.25`=.25,`.5`=.5,`.75`=.75, `1`=1)
  ) +
  facet_grid(~ `Listener Group`) +
  cp_theme() 

print(discr_by_mfb_dfb_plot)
ggsave(
  "discr_by_mfb_dfb_plot.png",
  plot = discr_by_mfb_dfb_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)



#speshul_data <- discr_preds |>
#  filter(`Phone Contrast (Language)` %in%
#           c(PHONES_SIMILAR, PHONES_FRENCH, PHONES_ENGLISH)) |>
#  mutate(`Preds Overlap-MFB`=colMeans(preds_overlap_dfb),
#         `Preds Overlap`=colMeans(preds_overlap)) |>
# repeated_average(
#  c(
#    "filename",
#    "Context",
#    "Phone Contrast Asymmetrical (Language)",
#    "Phone Contrast (Language)"
#  ),
#  c("Listener Group", "Phone Language (Code)", "Phone Language (Long)", "Phone Contrast"),
#  c("Accuracy",  "NeSssKL Overlap (0.001)", "Preds Overlap-MFB",
#    "Preds Overlap")
#) 
#
#
#
#
#highlight_scatterplot(
#  speshul_data |>
#    mutate(
#      `Point Type`=ifelse(
#        `Phone Contrast (Language)` %in% PHONES_SIMILAR,
#        "T0", ifelse(
#          `Phone Contrast (Language)` %in%  PHONES_FRENCH, "T1",
#          ifelse(
#            `Phone Contrast (Language)` %in% PHONES_ENGLISH,  "T2", "D")))) |>
#    filter(`Point Type` != "D"),
#  "Preds Overlap-MFB",
#  "Accuracy",
#  "Phone Contrast",
#  "Point Type", seed=2) + 
#  cp_theme() +
#  theme(legend.position = "none") + 
#  geom_abline() +
##  geom_smooth(data=discr_idpreds_c,
##              aes(y=`Accuracy`, x=`NeSssKL Overlap (0.001)`),
##              colour="#00000044", lwd=0.5,
##              se=FALSE,
##              inherit.aes=FALSE) +
#  facet_wrap(~ `Listener Group`)# +
##  coord_cartesian(xlim=c(0,1), ylim=c(0.5,1))
#
#print(discr_by_mfb_highlight_plot)
#
#
#model_null_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("null_acc"), "", "bernoulli")
#model_null_acc <- add_criterion(model_null_acc, "loo", file = get_filename("null_acc"))
#
#model_dfb_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("dfb_acc"), "", "bernoulli")
#model_dfb_acc <- add_criterion(model_dfb_acc, "loo", file = get_filename("dfb_acc"))
#
#
#plot_overlap_acc_model <- mcmc_areas(model_overlap_acc,
#                                     pars=names(model_overlap_acc$fit)[1:6],
#                                    prob=0.95, border_size=0.3) + 
#  scale_y_discrete(labels=rev(c(
#    "Intercept",
#    "Phonological Overlap",
#    "Listener Group\n(French - English)",
#    "Experimental Trial\n(per full session)",
#    "Phonological Overlap ×\nListener Group",
#    "Listener Group ×\nExperimental Trial")),
#    limits=rev) +
#  xlab("Coefficient value") +
#  cp_theme()
#
#print(plot_overlap_acc_model)
#ggsave("plot_overlap_coefs.png",
#       plot=plot_overlap_acc_model,
#       width=5, height=5, units="in", dpi=600)
#
#
#print(loo_compare(model_overlap_acc, model_null_acc))
#print(loo_compare(model_overlap_acc, model_null_acc)[2,1]/
#        loo_compare(model_overlap_acc, model_null_acc)[2,2])
#
#

#plot_overlap_dfb_model_cepts <- mcmc_areas(model_overlap_dfb, pars=names(model_overlap_dfb$fit)[1:5], prob=0.95, border_size=0.3) + 
#  scale_y_discrete(labels=rev(c("-3/-2", "-2/-1", "-1/1", "1/2", "2/3")), limits=rev) +
#  xlab("Coefficient value") +
#  cp_theme()
#plot_overlap_dfb_model_fixef <- mcmc_areas(model_overlap_dfb, pars=names(model_overlap_dfb$fit)[6:14], prob=0.95, border_size=0.3) + 
#  scale_y_discrete(labels=rev(c(
#    "Phonological Overlap",
#    "Δ DTW Mel Filterbank\n(per 0.05 increase)",
#    "Listener Group\n(French - English)",
#    "Experimental Trial\n(per full session)",
#    "Phonological Overlap ×\nΔ DTW Mel Filterbank",
#    "Phonological Overlap ×\nListener Group",
#    "Δ DTW Mel Filterbank ×\nListener Group",
#    "Listener Group ×\nExperimental Trial",
#    "Phonological Overlap ×\nΔ DTW Mel Filterbank ×\nListener Group")),
#    limits=rev) +
#  xlab("Coefficient value") +
#  cp_theme()
#
#plots_overlap_dfb_model_coefs <- plot_overlap_dfb_model_cepts +
#  plot_overlap_dfb_model_fixef +
#  plot_layout(design="
#A
#A
#A
#B
#B
#B
#B
#B") + 
#  plot_annotation(tag_level="a")
#
#ggsave("plots_overlap_dfb_coefs.png",
#       plot=plots_overlap_dfb_model_coefs,
#       width=5, height=12, units="in", dpi=600)
#
#
#pred_cutpoints_0 <- fixef(model_overlap_dfb)[1:5,1]
#pred_cutpoints_1 <- fixef(model_overlap_dfb)[1:5,1] -
#  fixef(model_overlap_dfb)["Phonological.Overlap",1]
#plot_overlap_dfb_model_preds_0 <- cumord_norm_plot(-6, 6, pred_cutpoints_0,
#                                                    cumord_categories)
#plot_overlap_dfb_model_preds_simple_0 <- cumord_prob_plot(pred_cutpoints_0,
#                                                    cumord_categories)
#plot_overlap_dfb_model_preds_1 <- cumord_norm_plot(-6, 6, pred_cutpoints_1,
#                                                    cumord_categories)
#plot_overlap_dfb_model_preds_simple_1 <- cumord_prob_plot(pred_cutpoints_1,
#                                                    cumord_categories)
#
#plots_overlap_dfb_model_preds <- plot_overlap_dfb_model_preds_0 + 
#  plot_overlap_dfb_model_preds_1 +
#  plot_overlap_dfb_model_preds_simple_0 +
#  plot_overlap_dfb_model_preds_simple_1 + 
#  plot_layout(design="
#AB
#AB
#AB
#AB
#AB
#AB
#AB
#AB
#AB
#AB
#AB
#CD
#CD") +
#     plot_annotation(tag_level="a")
#
#ggsave("plots_overlap_dfb_preds.png",
#       plot=plots_overlap_dfb_model_preds,
#       width=12, height=4.5, units="in", dpi=600)
#
#loo_compare(model_null_acc, model_dfb_acc, model_overlap_acc, model_overlap_dfb_acc)
#
