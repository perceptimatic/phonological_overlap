source("setup.R")




discr_english_by_french_plot <- ggplot(discr_c_wide,
                                       aes(x=`Accuracy English`,
                                           y=`Accuracy French`)) +
  geom_point() +
  geom_abline(colour = "#bbbbbb") +
  scale_x_continuous(limits = c(0.5,1)) +
  scale_y_continuous(limits = c(0.5,1)) +
  cp_theme()
print(discr_english_by_french_plot)
  

  point_types <- rep("D", nrow(d))
  point_types[d[[variable_label]] %in% type_0] <- "T0"
  point_types[d[[variable_label]] %in% type_1] <- "T1"
  point_types[d[[variable_label]] %in% type_2] <- "T2"
 

discr_english_by_french_with_highlighting_plot <- highlight_scatterplot(
  discr_c_wide |>
    mutate(
      `Point Type`=ifelse(
        `Phone Contrast (Language)` %in% PHONES_SIMILAR,
        "T0", ifelse(
          `Phone Contrast (Language)` %in%  PHONES_FRENCH, "T1",
          ifelse(
          `Phone Contrast (Language)` %in% PHONES_ENGLISH,  "T2", "D")))) |>
    filter(`Point Type` != "D"),
  "Accuracy English",
  "Accuracy French",
  "Phone Contrast",
  "Point Type", seed=2) +
    geom_smooth(data=discr_c_wide, mapping=aes(x=`Accuracy English`,
                          y=`Accuracy French`), method="lm", inherit.aes=FALSE,
                se=FALSE, fullrange=TRUE,
                lty="dashed", colour="#00000044", lwd=1) +
    scale_x_continuous(limits = c(0.5,1)) +
    scale_y_continuous(limits = c(0.5,1)) +
    cp_theme() +
    theme(legend.position = "none") 

print(discr_english_by_french_with_highlighting_plot)

ggsave("discr_english_by_french_plots_600.png",
       plot=discr_english_by_french_plot |
         discr_english_by_french_with_highlighting_plot,
       width = 6.52,
       height=4.5, units="in", dpi=600)

