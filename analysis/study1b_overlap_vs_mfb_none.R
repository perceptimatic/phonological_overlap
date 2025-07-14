source("setup.R")

options(mc.cores=4)

model_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("null_acc"), "", "bernoulli")
model_null_acc <- add_criterion(model_null_acc, "loo", file = get_filename("null_acc"))

model_dfb_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("dfb_acc"), "", "bernoulli")
model_dfb_acc <- add_criterion(model_dfb_acc, "loo", file = get_filename("dfb_acc"))

model_overlap_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
model_overlap_acc <- add_criterion(model_overlap_acc, "loo", file = get_filename("overlap_acc"))

model_overlap_dfb_acc <- run_brms_model(
  formula(
    "Accuracy~
                    Phonological.Overlap*Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_dfb_acc"), "", "bernoulli")
model_overlap_dfb_acc <- add_criterion(model_overlap_dfb_acc, "loo", file = get_filename("overlap_dfb_acc"))

plot_overlap_dfb_model_cepts <- mcmc_areas(model_overlap_dfb, pars=names(model_overlap_dfb$fit)[1:5], prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c("-3/-2", "-2/-1", "-1/1", "1/2", "2/3")), limits=rev) +
  xlab("Coefficient value") +
  cp_theme()
plot_overlap_dfb_model_fixef <- mcmc_areas(model_overlap_dfb, pars=names(model_overlap_dfb$fit)[6:14], prob=0.95, border_size=0.3) + 
  scale_y_discrete(labels=rev(c(
    "Phonological Overlap",
    "Δ DTW Mel Filterbank\n(per 0.05 increase)",
    "Listener Group\n(French - English)",
    "Experimental Trial\n(per full session)",
    "Phonological Overlap ×\nΔ DTW Mel Filterbank",
    "Phonological Overlap ×\nListener Group",
    "Δ DTW Mel Filterbank ×\nListener Group",
    "Listener Group ×\nExperimental Trial",
    "Phonological Overlap ×\nΔ DTW Mel Filterbank ×\nListener Group")),
    limits=rev) +
  xlab("Coefficient value") +
  cp_theme()

plots_overlap_dfb_model_coefs <- plot_overlap_dfb_model_cepts +
  plot_overlap_dfb_model_fixef +
  plot_layout(design="
A
A
A
B
B
B
B
B") + 
  plot_annotation(tag_level="a")

ggsave("plots_overlap_dfb_coefs.png",
       plot=plots_overlap_dfb_model_coefs,
       width=5, height=12, units="in", dpi=600)


pred_cutpoints_0 <- fixef(model_overlap_dfb)[1:5,1]
pred_cutpoints_1 <- fixef(model_overlap_dfb)[1:5,1] -
  fixef(model_overlap_dfb)["Phonological.Overlap",1]
plot_overlap_dfb_model_preds_0 <- cumord_norm_plot(-6, 6, pred_cutpoints_0,
                                                    cumord_categories)
plot_overlap_dfb_model_preds_simple_0 <- cumord_prob_plot(pred_cutpoints_0,
                                                    cumord_categories)
plot_overlap_dfb_model_preds_1 <- cumord_norm_plot(-6, 6, pred_cutpoints_1,
                                                    cumord_categories)
plot_overlap_dfb_model_preds_simple_1 <- cumord_prob_plot(pred_cutpoints_1,
                                                    cumord_categories)

plots_overlap_dfb_model_preds <- plot_overlap_dfb_model_preds_0 + 
  plot_overlap_dfb_model_preds_1 +
  plot_overlap_dfb_model_preds_simple_0 +
  plot_overlap_dfb_model_preds_simple_1 + 
  plot_layout(design="
AB
AB
AB
AB
AB
AB
AB
AB
AB
AB
AB
CD
CD") +
     plot_annotation(tag_level="a")

ggsave("plots_overlap_dfb_preds.png",
       plot=plots_overlap_dfb_model_preds,
       width=12, height=4.5, units="in", dpi=600)

loo_compare(model_null_acc, model_dfb_acc, model_overlap_acc, model_overlap_dfb_acc)

