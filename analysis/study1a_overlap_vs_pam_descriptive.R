source("setup.R")

discr_by_overlap_plot <- ggplot(
  discr_idpreds_c,
  aes(
    x = `Phonological Overlap`,
    y = `Accuracy and Certainty`,
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
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 3))

print(discr_by_overlap_plot)
ggsave(
  "discr_by_overlap_plot.png",
  plot = discr_by_overlap_plot,
  width = 6.52,
  height = 4.5,
  units = "in",
  dpi = 600
)
