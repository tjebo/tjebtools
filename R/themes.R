#' theme_tjebo
#' @name theme_tjebo
#' @description typical theme used by Tjebo F.C. Heeren
#' @author Tjebo F.C. Heeren
#' @param base_size	 base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @import ggplot2
#' @export
theme_tjebo <- function (base_size = 9, base_family = "",
                         base_line_size = base_size/30,
                         base_rect_size = base_size/30) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(1)),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(margin = margin(t = base_size)),
          axis.title.y = element_text(margin = margin(r = base_size),
                                     angle = 0, vjust = .5),
          axis.text = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.8)),
          legend.key.size = unit(0.8*base_size,'pt'),
          complete = TRUE)
}
