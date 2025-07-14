source("setup.R")

options(mc.cores=4)


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

# Could be useful for something but
#
#model_same_max <- run_brms_model(
#  formula(
#    "Accuracy.and.Certainty ~
#                    Listener.Group*Filename.Accuracy.and.Certainty +
#                    Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_max"), "", "ordered")
#model_same_max <- add_criterion(model_same_max, "loo", file = get_filename("same_max"))

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


plot_overlap_mct_model_fixef <- mcmc_areas(model_same_overlap_mct,
                                           pars=names(model_same_overlap_mct$fit)[c(6:8, 10:12, 14)],
                                           prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Lowest Possible Assimilation Threshold",
    "Phonological Overlap",
    "Listener Group\n(French - English)",
    "LPAT ×\nPhonological Overlap",
    "LPAT ×\nListener Group",
    "Phonological Overlap ×\nListener Group",
    "LPAT ×\nPhonological Overlap ×\nListener Group")),
    limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

ggsave("plots_same_overlap_mct_coefs.png",
       plot=plot_overlap_mct_model_fixef,
       width=5, height=5.5, units="in", dpi=600)


print(summary(qlogis(filter(discr_preds, `Same Top Choice` == "Yes")$`Maximum Categorization Threshold`)))
print(mean(filter(discr_preds, `Same Top Choice` == "Yes")$`Phonological Overlap`))
print(sd(filter(discr_preds, `Same Top Choice` == "Yes")$`Phonological Overlap`))


eng_group <- -0.5
fre_group <- 0.5
mct_lo <- qlogis(0.4) # Value on the low end for the same-top group
mct_hi <- qlogis(0.8) # Value on the low end for the same-top group
# Overlap is in SDs from the centre in the same-choice dataset 
overlap_hi <- 1.5
overlap_vhi <- 3.5

pred_cutpoints_lomct_mdov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        mct_lo*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1]
pred_cutpoints_lomct_hiov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_lo*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_lo*eng_group*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_lomct_vhiov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_vhi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_lo*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_lo*eng_group*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_himct_mdov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        mct_lo*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1]
pred_cutpoints_himct_hiov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_hi*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_hi*eng_group*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_himct_vhiov_eng <- fixef(model_same_overlap_mct)[1:5,1] -
                        eng_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_vhi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_hi*eng_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_hi*eng_group*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
cumord_prob_plot(pred_cutpoints_lomct_mdov_eng, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_hiov_eng, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_vhiov_eng, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_mdov_eng, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_hiov_eng, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_vhiov_eng, cumord_categories)


pred_cutpoints_lomct_mdov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        mct_lo*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1]
pred_cutpoints_lomct_hiov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_lo*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_lo*fre_group*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_lomct_vhiov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_lo*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_lo*fre_group*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_himct_mdov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        mct_lo*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1]
pred_cutpoints_himct_hiov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_hi*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_hi*fre_group*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
pred_cutpoints_himct_vhiov_fre <- fixef(model_same_overlap_mct)[1:5,1] -
                        fre_group*fixef(model_same_overlap_mct)["Listener.Group",1] - 
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_vhi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1] -
                        mct_hi*fre_group*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Listener.Group",1] -
                        mct_hi*fre_group*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap:Listener.Group",1]
cumord_prob_plot(pred_cutpoints_lomct_mdov_fre, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_hiov_fre, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_vhiov_fre, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_mdov_fre, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_hiov_fre, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_vhiov_fre, cumord_categories)



pred_cutpoints_lomct_mdov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1]
pred_cutpoints_lomct_hiov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1]
pred_cutpoints_lomct_vhiov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_vhi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_lo*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1]
pred_cutpoints_himct_mdov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_lo*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1]
pred_cutpoints_himct_hiov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_hi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1]
pred_cutpoints_himct_vhiov_no <- fixef(model_same_overlap_mct)[1:5,1] -
                        mct_hi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold",1] -
                        overlap_vhi*fixef(model_same_overlap_mct)["Phonological.Overlap",1] -
                        mct_hi*overlap_vhi*fixef(model_same_overlap_mct)["Maximum.Categorization.Threshold:Phonological.Overlap",1]
cumord_prob_plot(pred_cutpoints_lomct_mdov_no, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_hiov_no, cumord_categories)
cumord_prob_plot(pred_cutpoints_lomct_vhiov_no, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_mdov_no, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_hiov_no, cumord_categories)
cumord_prob_plot(pred_cutpoints_himct_vhiov_no, cumord_categories)











model_same_overlap_goodness_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Phonological.Overlap*Listener.Group +
                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_overlap_goodness_mct"), "", "ordered", centre_overlap = TRUE)
model_same_overlap_goodness_mct <- add_criterion(model_same_overlap_goodness_mct, "loo")

model_same_goodness_mct <- run_brms_model(
  formula(
    "Accuracy.and.Certainty ~
                    Maximum.Categorization.Threshold*Goodness.Difference*Listener.Group +
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  filter(discr_preds, `Same Top Choice` == "Yes"), get_filename("same_goodness_mct"), "", "ordered", centre_overlap = TRUE)
model_same_goodness_mct <- add_criterion(model_same_goodness_mct, "loo")



print(discr_by_mct_plot)
ggsave(
  "discr_by_mct_same_plot.png",
  plot = discr_by_mct_plot,
  width = 6.52,
  height = 4.5,
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


