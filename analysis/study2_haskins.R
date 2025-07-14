source("setup.R")
options(mc.cores=4)

discracc_by_haskins_plot <- ggplot(
  mutate(discr_idpreds_c,
         label=ifelse(
    `Listener Group` == "French" & `Phone Contrast (Language)` %in% c("iː–yː (et)", "ɪ–ʊ (en)", "o–œ (fr)",  "a–ɐ̃ (pt)"),
    `Phone Contrast`, ""
  )),
  aes(
    x = Haskins,
    y = Accuracy
  )
) +
  geom_point(aes(alpha=ifelse(label == "", 0, 1)), pch=1, size=5, stroke=0.7) +
  scale_alpha_continuous(range=c(0,1), guide="none") +
  geom_point(aes(fill=`Phonological Overlap`), size=2.5, stroke=0.7, pch=21) +
  scale_fill_distiller(type = "seq", palette = "YlGnBu", direction=1) +
  geom_text_repel(aes(label=label),box.padding=10, max.overlaps=50) +
  facet_grid(~ `Listener Group`, scales = "free_x") +
  cp_theme() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0, b = 0),
    legend.spacing.y = unit(0, "in")
  )  +
  coord_cartesian(xlim = c(0, 0.52), ylim = c(0.48, 1))


print(discracc_by_haskins_plot)
ggsave(
  "discracc_by_haskins_plot.png",
  plot = discracc_by_haskins_plot,
  width = 6.52,
  height = 4,
  units = "in",
  dpi = 600
)

model_null_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("null_acc"), "", "bernoulli")
model_null_acc <- add_criterion(model_null_acc, "loo", file = get_filename("null_acc"))

model_overlap_dfb_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_dfb_acc"), "", "bernoulli")
model_overlap_dfb_acc <- add_criterion(model_overlap_dfb_acc, "loo", file = get_filename("overlap_dfb_acc"))

model_haskins_dfb_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Haskins*Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("haskins_dfb_acc"), "", "bernoulli")
model_haskins_dfb_acc <- add_criterion(model_haskins_dfb_acc, "loo", file = get_filename("haskins_dfb_acc"))

model_overlap_haskins_dfb_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Haskins*Δ.DTW.Mel.Filterbank*Listener.Group +Listener.Group*Trial.Number +
                    (1|Participant) + (1 + Listener.Group|filename)"),
  discr_preds, get_filename("overlap_haskins_dfb_acc"), "", "bernoulli")
model_overlap_haskins_dfb_acc <- add_criterion(model_overlap_haskins_dfb_acc, "loo", file = get_filename("overlap_haskins_dfb_acc"))

loo_compare( model_haskins_dfb_acc, model_overlap_dfb_acc, model_overlap_haskins_dfb_acc)


# We had to move to the minimal model to see what we were looking for -
# it is a little suspect, but it is genuinely the case that the other
# models are harder to interpret
model_dot_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Dot*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("dot_min_acc"), "", "bernoulli")

model_cosine_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Cosine*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("cosine_min_acc"), "", "bernoulli")

model_euclid_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Euclidean*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("euclid_min_acc"), "", "bernoulli")

model_ssdiff_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    SSDiff*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("ssdiff_min_acc"), "", "bernoulli")

model_haskins_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Haskins*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("haskins_min_acc"), "", "bernoulli")

model_overlap_min_acc <- run_brms_model(
  formula(
    "Accuracy ~
                    Phonological.Overlap*Listener.Group +
                    (1|Participant)"),
  discr_preds, get_filename("overlap_min_acc"), "", "bernoulli")

model_dot_min_acc <- add_criterion(model_dot_min_acc, "loo", file = get_filename("dot_min_acc"))
model_cosine_min_acc <- add_criterion(model_cosine_min_acc, "loo", file = get_filename("cosine_min_acc"))
model_euclid_min_acc <- add_criterion(model_euclid_min_acc, "loo", file = get_filename("euclid_min_acc"))
model_ssdiff_min_acc <- add_criterion(model_ssdiff_min_acc, "loo", file = get_filename("ssdiff_min_acc"))
model_haskins_min_acc <- add_criterion(model_haskins_min_acc, "loo", file = get_filename("haskins_min_acc"))
model_overlap_min_acc <- add_criterion(model_overlap_min_acc, "loo", file = get_filename("overlap_min_acc"))

loo_compare(model_haskins_min_acc, model_ssdiff_min_acc, model_euclid_min_acc, model_dot_min_acc, model_cosine_min_acc)
loo_compare(model_overlap_min_acc, model_cosine_min_acc)








#discracc_by_overlap_plot <- ggplot(
#  discr_idpreds_c,
#  aes(
#    x = `Phonological Overlap`,
#    y = `Accuracy`,
#    alpha = `Haskins`
#  )
#) +
#  geom_point(size=2.5, stroke=0.7) +
#  facet_grid(~ `Listener Group`, scales = "free_x") +
#  cp_theme() +
#  theme(
#    legend.position = "bottom",
#    legend.box = "vertical",
#    legend.margin = margin(t = 0, b = 0),
#    legend.spacing.y = unit(0, "in")
#  )  +
#  coord_cartesian(xlim = c(0, 1), ylim = c(0.48, 1))

#plots_discracc_by_overlap_haskins <- discracc_by_haskins_plot /
#  discracc_by_overlap_plot + 
#  plot_annotation(tag_level="a")



#
#
#model_overlap_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
#model_overlap_acc <- add_criterion(model_overlap_acc, "loo", file = get_filename("overlap_acc"))
#
#loo_compare(model_haskins_acc, model_null_acc)
#loo_compare(model_haskins_acc, model_overlap_acc)
#
#
#model_haskins_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Haskins*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("haskins_acc"), "", "bernoulli")
#model_haskins_acc <- add_criterion(model_haskins_acc, "loo", file = get_filename("haskins_acc"))
#
#
#model_overlap_haskins_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Phonological.Overlap*Haskins*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("overlap_haskins_acc"), "", "bernoulli")
#model_overlap_haskins_acc <- add_criterion(model_overlap_haskins_acc, "loo", file = get_filename("overlap_haskins_acc"))





#model_dot_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Dot*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("dot_acc"), "", "bernoulli")
#model_dot_acc <- add_criterion(model_dot_acc, "loo", file = get_filename("dot_acc"))
#
#model_cosine_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Cosine*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("cosine_acc"), "", "bernoulli")
#model_cosine_acc <- add_criterion(model_cosine_acc, "loo", file = get_filename("cosine_acc"))
#
#model_euclid_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Euclidean*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("euclid_acc"), "", "bernoulli")
#model_euclid_acc <- add_criterion(model_euclid_acc, "loo", file = get_filename("euclid_acc"))
#
#model_ssdiff_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    SSDiff*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("ssdiff_acc"), "", "bernoulli")
#model_ssdiff_acc <- add_criterion(model_ssdiff_acc, "loo", file = get_filename("ssdiff_acc"))
#
#loo_compare( model_haskins_acc, model_cosine_acc, model_dot_acc)
#loo_compare( model_overlap_acc, model_cosine_acc)
#loo_compare( model_euclid_acc, model_haskins_acc, model_cosine_acc, model_dot_acc, model_overlap_acc, model_ssdiff_acc)
#loo_compare( model_euclid_acc, model_haskins_acc, model_cosine_acc, model_dot_acc, model_ssdiff_acc)
#loo_compare( model_euclid_acc, model_haskins_acc,  model_dot_acc, model_ssdiff_acc)
#
#ggplot(
#  discr_idpreds_c,
#  aes(
#    x = Euclidean,
#    y = Accuracy,
#    label = `Phone Contrast (Language)`
#  )
#) +
#  geom_label(aes(fill=SSDiff), size=2.5) +
#  scale_fill_distiller(type = "seq", palette = "YlGnBu", direction=1) +
#  facet_grid(~ `Listener Group`, scales = "free_x") +
#  cp_theme() +
#  theme(
#    legend.position = "bottom",
#    legend.box = "vertical",
#    legend.margin = margin(t = 0, b = 0),
#    legend.spacing.y = unit(0, "in")
#  )  
#
#
#
#model_overlap_min_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Phonological.Overlap*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("overlap_acc"), "", "bernoulli")
#model_overlap_acc <- add_criterion(model_overlap_acc, "loo", file = get_filename("overlap_acc"))
#
#
#model_haskins_acc <- run_brms_model(
#  formula(
#    "Accuracy ~
#                    Haskins*Listener.Group +Listener.Group*Trial.Number +
#                    (1|Participant) + (1 + Listener.Group|filename)"),
#  discr_preds, get_filename("haskins_acc"), "", "bernoulli")
#model_haskins_acc <- add_criterion(model_haskins_acc, "loo", file = get_filename("haskins_acc"))
#
