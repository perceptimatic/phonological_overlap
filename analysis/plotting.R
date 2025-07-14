color_scheme_set(c("#7ed1e6", "black", "black", "black", "black", "black"))

cp_theme <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          text=element_text(family="Arial", size=14),
          axis.text=element_text(colour="black"),
          axis.title=element_text(size=12),
          legend.text=element_text(size=14),
          legend.box.spacing = unit(0, "inches"),
          strip.background = element_rect(fill="white"))
}

cumord_norm_plot <- function(from, to, cutpoints, categories, x_precision=2) {
  if (length(cutpoints) + 1 != length(categories))
    stop("Number of cutpoints must be exactly one less than number of categories")
  cutpoints <- round(cutpoints, x_precision)
  z <- seq(from = from, to = to, by = 10**(-x_precision))
  d <- data.frame(z=z, density=dnorm(z), prob=pnorm(z))
  d_categories <- data.frame(z=c(-Inf, cutpoints), 
                            prob=c(pnorm(cutpoints), 1) - c(0, pnorm(cutpoints)),
                            categories=categories,
                            plot_x=c(from, cutpoints) + (c(cutpoints, to) - c(from, cutpoints))/2)
  ggplot(d, aes(x=z, y=density)) +
    geom_area(fill="lightgrey") +
    geom_text(data=d_categories,
              mapping=aes(x=plot_x, y=0.47, label=categories)) +
    geom_text(data=d_categories,
              mapping=aes(x=plot_x, y=0.43,
                          label=sub("^\\.00$", "0", sub("^0\\.", ".", sprintf("%0.2f", prob))))) +
    geom_vline(data=data.frame(z=cutpoints), aes(xintercept=z), lwd=0.2) +
    scale_x_continuous(breaks=from:to) +
    scale_y_continuous(breaks=NULL) +
    cp_theme() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("") + xlab("z")
}

cumord_prob_plot <- function(cutpoints, categories, print_precision=2, dontprint_thresh=0.025) {
  if (length(cutpoints) + 1 != length(categories))
    stop("Number of cutpoints must be exactly one less than number of categories")
  d_categories <- tibble(prob=c(pnorm(cutpoints), 1) - c(0, pnorm(cutpoints)),
                             categories=categories,
                             midpoints=0.5*(cumsum(prob)+cumsum(c(0, prob[1:(length(prob)-1)]))))
  d_printing_categories <- filter(d_categories, prob > dontprint_thresh)
  ggplot(d_categories) +
    geom_bar(aes(fill=factor(categories, levels=rev(categories)), y=prob, x=0),
             colour="black", stat="identity", alpha=0.5) + 
    geom_text(data=d_printing_categories, aes(label=categories, x=.2, y=midpoints))+
    geom_text(data=d_printing_categories,
              aes(label=
                   sub("^\\.00$", "0", sub("^0\\.", ".", sprintf("%0.2f", round(prob, print_precision)))),
                                              x=-0.2, y=midpoints))+
   cp_theme() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.title=element_blank(),
         axis.ticks=element_blank(),
         axis.text=element_blank(),
         legend.position="none")+
    scale_fill_brewer(palette="RdYlBu", direction=-1) +
    coord_flip(expand=FALSE)
}
