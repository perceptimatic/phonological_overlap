source("setup.R")

assimilation_vectors_example <- ggplot(
  assimilation_vectors |> filter(`Listener Group` == "French",
                                 `Phone Language (Long)` == "American English"),
  aes(x=`Phone`, y=Response, fill=`Proportion of Responses`)
) +
  geom_tile(colour=NA, linewidth=NA) +
  geom_vline(xintercept=seq(1, 60) + 0.5, colour="#555555", linewidth=0.3) +
  scale_fill_viridis_c(limits=c(0,1), option="B") +
  scale_y_discrete(limits=rev) +
  cp_theme() +
  theme(#axis.text=element_text(size=10, colour="black"),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  guides(fill=guide_colourbar(title.position="top",
                              title.vjust=1,
                              title.hjust=0.5,
                              barwidth=unit(2.5, "inches"),
                              barheight=unit(0.15, "inches"),
                              frame.colour="black",
                              ticks.colour="black"))
ggsave("assimilation_vectors_example_plot_600.png",
       plot=assimilation_vectors_example,
       width=4, height=4.5, units="in", dpi=600)

  


assimilation_vectors_plot <- ggplot(assimilation_vectors,
       aes(x=`Phone`, y=Response, fill=`Proportion of Responses`)) +
  geom_tile(colour=NA, linewidth=NA) +
  geom_vline(xintercept=seq(1, 60) + 0.5, colour="#555555", linewidth=0.3) +
  facet_grid(`Listener Group` ~ `Phone Language (Long)`, scales="free",
             space="free", labeller=label_wrap_gen(width=13)) +
  scale_fill_viridis_c(limits=c(0,1), option="B",
                       breaks= c(0,0.25,0.5,0.75,1),
                       labels=as.character(c(0,0.25,0.5,0.75,1))) +
  scale_y_discrete(limits=rev) +
  cp_theme() +
  theme(axis.text=element_text(size=8, colour="black"),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  guides(fill=guide_colourbar(title.position="left",
                              title.vjust=1,
                              barwidth=unit(1.5, "inches"),
                              barheight=unit(0.15, "inches"),
                              frame.colour="black",
                              ticks.colour="black"))




assimilation_vectors_bycontext_french <- 
  read_id(IDENT_DATA) %>%
#  filter(`Phone Language (Long)` == "Hexagonal French") %>%
  group_by(`Listener Group`) %>%
  do(get_assimilation_vectors(., c("Phone", "Phone (Language)", 
                                   "Triphone (Language)",
                                    "Triphone", "Phone Language (Long)",
                                    "Phone Language (Code)"),
                              "Response", "Goodness") %>% 
       repeated_average("Triphone (Language)",
                        c("Phone", "Phone Language (Long)",
                          "Phone Language (Code)", "Response"),
                        c("Proportion of Responses", "Goodness"),
                        na.rm=TRUE)) %>%
  ungroup()

assimilation_vectors_without_sucre_french <- 
  read_id(IDENT_DATA) %>%
  filter(`Phone Language (Long)` == "Hexagonal French",
         Triphone != "søk", Phone == "ø",
         `Listener Group` == "French") %>%
  group_by(`Listener Group`) %>%
  do(get_assimilation_vectors(., c("Phone", "Phone (Language)", 
                                   "Context", "Phone Language (Long)",
                                    "Phone Language (Code)"),
                              "Response", "Goodness") %>% 
       repeated_average(c("Context", "Phone (Language)"),
                        c("Phone", "Phone Language (Long)",
                          "Phone Language (Code)", "Response"),
                        c("Proportion of Responses", "Goodness"),
                        na.rm=TRUE)) 

assimilation_vectors_bycontext_french_plot <- ggplot(assimilation_vectors_bycontext_french,
                                    aes(x=`Triphone (Language)`, y=Response, fill=`Proportion of Responses`)) +
  geom_tile(colour=NA, linewidth=NA) +
  geom_vline(xintercept=seq(1, 60) + 0.5, colour="#555555", linewidth=0.3) +
  facet_grid(`Listener Group` ~ Phone, scales="free",
             space="free", labeller=label_wrap_gen(width=13)) +
  scale_fill_viridis_c(limits=c(0,1), option="B") +
  scale_y_discrete(limits=rev) +
  cp_theme() +
  theme(axis.text=element_text(size=10, colour="black"),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=7)) +
  guides(fill=guide_colourbar(title.position="left",
                              title.vjust=1,
                              barwidth=unit(1.5, "inches"),
                              barheight=unit(0.15, "inches"),
                              frame.colour="black",
                              ticks.colour="black"))


 print(assimilation_vectors_plot)
 print(assimilation_vectors_bycontext_french_plot)
 ggsave("assimilation_vectors_plot_600.png",
        plot=assimilation_vectors_plot,
         width=12, height=5.5, units="in", dpi=600)


 assimilation_vectors_plot <- ggplot(assimilation_vectors,
                                     aes(x=`Phone`, y=Response, fill=`Proportion of Responses`)) +
   geom_tile(colour=NA, linewidth=NA) +
   geom_vline(xintercept=seq(1, 60) + 0.5, colour="#555555", linewidth=0.3) +
   facet_grid(`Listener Group` ~ `Phone Language (Long)`, scales="free",
              space="free", labeller=label_wrap_gen(width=13)) +
   scale_fill_viridis_c(limits=c(0,1), option="B",
                        breaks= c(0,0.25,0.5,0.75,1),
                        labels=as.character(c(0,0.25,0.5,0.75,1))) +
   scale_y_discrete(limits=rev) +
   cp_theme() +
   theme(axis.text=element_text(size=8, colour="black"),
         axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
   guides(fill=guide_colourbar(title.position="left",
                               title.vjust=1,
                               barwidth=unit(1.5, "inches"),
                               barheight=unit(0.15, "inches"),
                               frame.colour="black",
                               ticks.colour="black"))
 
 
goodness_vs_prob_plot <- ggplot(assimilation_vectors |> filter(!is.na(Goodness)),
       aes(x=`Proportion of Responses`, y=Goodness)) +
  geom_point(pch=1) +
  facet_grid(~ `Listener Group`) +
  cp_theme() 
print(goodness_vs_prob_plot)

ggsave("goodness_vs_prob_plot.png",
       plot=goodness_vs_prob_plot,
       width = 6.52,
       height = 4.5,
       units="in", dpi=600)

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "English"),
           cor(`Goodness`, `Proportion of Responses`)))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French"),
           cor(`Goodness`, `Proportion of Responses`)))


print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "English",
                    `Proportion of Responses` >= 0.1),
           cor(`Goodness`, `Proportion of Responses`)))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French",
                    `Proportion of Responses` >= 0.1),
           cor(`Goodness`, `Proportion of Responses`)))

print(coef(lm(Goodness ~ `Proportion of Responses`,
                 data=assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "English",
                    `Proportion of Responses` >= 0.1))))

print(coef(lm(Goodness ~ `Proportion of Responses`,
                 data=assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French",
                    `Proportion of Responses` >= 0.1))))

print(with(assimilation_vectors |>
                filter(!is.na(Goodness), `Listener Group` == "English",
                       `Proportion of Responses` < 0.1),
           mean(Goodness)))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French",
                    `Proportion of Responses` < 0.1),
           mean(Goodness)))


print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "English",
                    `Proportion of Responses` < 0.1),
           kurtosis(Goodness)))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French",
                    `Proportion of Responses` < 0.1),
           kurtosis(Goodness)))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "English",
                    `Proportion of Responses` >= 0.1),
           kurtosis(resid(lm(Goodness ~ `Proportion of Responses`)))))

print(with(assimilation_vectors |>
             filter(!is.na(Goodness), `Listener Group` == "French",
                    `Proportion of Responses` >= 0.1),
           kurtosis(resid(lm(Goodness ~ `Proportion of Responses`)))))


 
 assimilation_vectors_bycontext  <- 
   read_id(IDENT_DATA) %>%
   group_by(`Listener Group`) %>%
   do(get_assimilation_vectors(., c("Phone", "Phone (Language)", 
                                    "Triphone (Language)",
                                    "Triphone", "Phone Language (Long)",
                                    "Phone Language (Code)"),
                               "Response", "Goodness") %>% 
        repeated_average("Triphone (Language)",
                         c("Phone", "Phone Language (Long)",
                           "Phone Language (Code)", "Response"),
                         c("Proportion of Responses", "Goodness"),
                         na.rm=TRUE)) %>%
   ungroup()
 