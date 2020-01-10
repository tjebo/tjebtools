#' plot_bar
#' @author SO::AF7, modified by Tjebo
#' @description Creates a "fake legend" - Plotting a discrete color scale bar for continuous data, to combine with main plot using plot combining packages such as patchwork

#' @param breaks Required! Integer or numeric vector of breaks (should have unique values). If +-Inf are used, triangles will be added to the sides of the color bar
#' @param palette Default = "Greys", RColorBrewer palette to use
#' @param colors Default: RColorBrewer::brewer.pal(length(breaks) - 1, palette). Alternatively, set colors manually by providing vector of colors
#' @param direction Flip colors? Either 1 or -1
#' @param spacing Spacing between labels. String, either "natural" or "constant"
#' @param border_color default = no border color
#' @param legend_title legend title
#' @param legend_direction string. Either "horizontal" or "vertical"
#' @param font_size font size
#' @param expand_size  Controls spacing around legend plot
#' @param spacing_scaling  Multiplicative factor for label and legend title spacing
#' @param width Thickness of color bar
#' @param triangle_size Relative width of +-Inf triangles
#'
#' @import ggplot2 RColorBrewer
#'
#' @export
#'

plot_bar <- function(breaks, palette = "Greys", colors = NULL, label_position = "break",
                     direction = 1, spacing = "natural", border_color = NA, legend_title = NULL,
                     legend_direction = "horizontal", font_size = 10, breaksize = .1,
                     expand_size = 1, spacing_scaling = 1, width = 0.1, triangle_size = 0.1, ...) {

  if(!is.null(colors)) {
    warning("Ignoring RColorBrewer palette [", palette, "], since colors were passed manually")
    palette <- 0
    if (label_position == "break") {
      if (length(colors) != length(breaks) - 1) {
        stop("Number of colors (", length(colors), ") must be equal to number of breaks minus 1 (", length(breaks) - 1, ")")
      }
      if (label_position == "centered") {
        if (length(colors) != length(breaks)) {
          stop(
            "Number of colors (", length(colors), ") must be equal to number of breaks (",
            length(breaks), ")"
          )
        }
      }
    }
  }

  if (!(label_position %in% c("break", "centered"))) {
    stop("label_position must be either 'break' or 'centered'")
  }

  if (!(spacing %in% c("natural", "constant"))) {
    stop("spacing must be either 'natural' or 'constant'")
  }

  if (!(direction %in% c(1, -1))) {
    stop("direction must be either 1 or -1")
  }

  if (!(legend_direction %in% c("horizontal", "vertical"))) {
    stop("legend_direction must be either 'horizontal' or 'vertical'")
  }

  breaks <- as.numeric(breaks)
  new_breaks <- sort(unique(breaks))

  if (any(new_breaks != breaks)) {
    warning("Wrong order or duplicated breaks")
  }

  breaks <- new_breaks

  if (label_position == "break") {
    if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      n_max_palette <- RColorBrewer::brewer.pal.info$maxcolors[which(rownames(RColorBrewer::brewer.pal.info) == palette)]
      palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      min_palette <- palette[1]
      max_palette <- palette[length(palette)]
      colfunc <- colorRampPalette(c(min_palette, max_palette))
      colors <- colfunc(length(breaks) - 1)
    }

    if (direction == -1) {
      colors <- rev(colors)
    }
    inf_breaks <- which(is.infinite(breaks))
    if (length(inf_breaks) != 0) {
      breaks <- breaks[-inf_breaks]
    }
    plotcolors <- colors
    n_breaks <- length(breaks)
    labels <- breaks
    if (spacing == "constant") {
      breaks <- 1:n_breaks
    }
    r_breaks <- range(breaks)
    cbar_df <- data.frame(
      stringsAsFactors = FALSE, y = breaks,
      yend = c(breaks[-1], NA), color = as.character(1:n_breaks)
    )[-n_breaks, ]
    xmin <- 1 - width / 2
    xmax <- 1 + width / 2
    cbar_plot <- ggplot2::ggplot(
      cbar_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))
    ) +
      ggplot2::geom_rect(show.legend = FALSE, color = border_color)

    if (any(inf_breaks == 1)) {
      firstv <- breaks[1]
      polystart <- data.frame(x = c(xmin, xmax, 1), y = c(rep(
        firstv,
        2
      ), firstv - diff(r_breaks) * triangle_size))
      plotcolors <- plotcolors[-1]
      cbar_plot <- cbar_plot + ggplot2::geom_polygon(
        data = polystart,
        ggplot2::aes(x = x, y = y), show.legend = FALSE, inherit.aes = FALSE,
        fill = colors[1], color = border_color
      )
    }
    if (any(inf_breaks > 1)) {
      lastv <- breaks[n_breaks]
      polyend <- data.frame(x = c(xmin, xmax, 1), y = c(rep(
        lastv,
        2
      ), lastv + diff(r_breaks) * triangle_size))
      plotcolors <- plotcolors[-length(plotcolors)]
      cbar_plot <- cbar_plot + ggplot2::geom_polygon(
        data = polyend,
        ggplot2::aes(x = x, y = y), show.legend = FALSE, inherit.aes = FALSE,
        fill = colors[length(colors)], color = border_color
      )
    }
    if (legend_direction == "horizontal") {
      mul <- 1
      x <- xmin
      xend <- xmax
      cbar_plot <- cbar_plot + ggplot2::coord_flip()
      angle <- 0
      legend_position <- xmax + 0.1 * spacing_scaling
    }
    else {
      mul <- -1
      x <- xmax
      xend <- xmin
      angle <- -90
      legend_position <- xmax + 0.2 * spacing_scaling
    }

    cbar_plot <- cbar_plot +
      ggplot2::geom_segment(
        data = data.frame(y = breaks, yend = breaks),
        ggplot2::aes(y = y, yend = yend),
        x = x - 0.05 * mul * spacing_scaling,
        xend = xend, inherit.aes = FALSE
      ) +
      ggplot2::annotate(
        geom = "text",
        x = x - 0.1 * mul * spacing_scaling, y = breaks,
        label = breaks,
        size = font_size
      ) +
      ggplot2::scale_x_continuous(expand = c(expand_size, expand_size)) +
      ggplot2::scale_fill_manual(values = plotcolors) +
      ggplot2::theme_void()

    if (label_position == "centered") {
      cbar_plot <- cbar_plot +
        ggplot2::geom_segment(
          data = data.frame(y = breaks, yend = breaks),
          ggplot2::aes(y = y, yend = yend),
          x = x - 0.05 * mul * spacing_scaling,
          xend = xend, inherit.aes = FALSE
        ) +
        ggplot2::annotate(
          geom = "text",
          x = x - 0.1 * mul * spacing_scaling, y = breaks[-length(breaks)] + diff(breaks) / 2,
          label = breaks[1:length(breaks) - 1],
          size = font_size
        ) +
        ggplot2::scale_x_continuous(expand = c(expand_size, expand_size)) +
        ggplot2::scale_fill_manual(values = plotcolors) +
        ggplot2::theme_void()
    }

    if (!is.null(legend_title)) {
      cbar_plot <- cbar_plot + ggplot2::annotate(
        geom = "text", x = legend_position,
        y = mean(r_breaks), label = legend_title, angle = angle,
        size = font_size
      )
    }
    cbar_plot
  } else if (label_position == "centered") {
    if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      n_max_palette <- RColorBrewer::brewer.pal.info$maxcolors[which(rownames(RColorBrewer::brewer.pal.info) == palette)]
      palette <- brewer.pal(n_max_palette, palette)
      min_palette <- palette[1]
      max_palette <- palette[length(palette)]
      colfunc <- colorRampPalette(c(min_palette, max_palette))
      colors <- colfunc(length(breaks))
    }


    if (direction == -1) {
      colors <- rev(colors)
    }
    inf_breaks <- which(is.infinite(breaks))
    if (length(inf_breaks) != 0) {
      breaks <- breaks[-inf_breaks]
    }
    plotcolors <- colors
    n_breaks <- length(breaks)
    labels <- breaks
    if (spacing == "constant") {
      breaks <- 1:n_breaks
    }

    diff_breaks <- breaks[-length(breaks)] + diff(breaks) / 2
    val_br <- c(breaks[1] - (diff_breaks[1] - breaks[1]), diff_breaks, breaks[length(breaks)] + breaks[length(breaks)] - diff_breaks[length(diff_breaks)])
    r_breaks <- range(breaks)

    cbar_df <- data.frame(
      stringsAsFactors = FALSE, y = val_br[-length(val_br)],
      yend = val_br[-1], color = c(as.character(1:n_breaks))
    )



    xmin <- 1 - width / 2
    xmax <- 1 + width / 2
    cbar_plot <- ggplot2::ggplot(
      cbar_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))
    ) +
      ggplot2::geom_rect(show.legend = FALSE, color = border_color)

    if (any(inf_breaks == 1)) {
      firstv <- breaks[1]
      polystart <- data.frame(x = c(xmin, xmax, 1), y = c(rep(
        firstv,
        2
      ), firstv - diff(r_breaks) * triangle_size))
      plotcolors <- plotcolors
      cbar_plot <- cbar_plot + ggplot2::geom_polygon(
        data = polystart,
        ggplot2::aes(x = x, y = y), show.legend = FALSE, inherit.aes = FALSE,
        fill = colors[1], color = border_color
      )
    }
    if (any(inf_breaks > 1)) {
      lastv <- breaks[n_breaks]
      polyend <- data.frame(x = c(xmin, xmax, 1), y = c(rep(
        lastv,
        2
      ), lastv + diff(r_breaks) * triangle_size))
      plotcolors <- plotcolors
      cbar_plot <- cbar_plot + ggplot2::geom_polygon(
        data = polyend,
        ggplot2::aes(x = x, y = y), show.legend = FALSE, inherit.aes = FALSE,
        fill = colors[length(colors)], color = border_color
      )
    }
    if (legend_direction == "horizontal") {
      mul <- 1
      x <- xmin
      xend <- xmax
      cbar_plot <- cbar_plot + ggplot2::coord_flip()
      angle <- 0
      legend_position <- xmax + 0.1 * spacing_scaling
    }
    else {
      mul <- -1
      x <- xmax
      xend <- xmin
      angle <- -90
      legend_position <- xmax + 0.2 * spacing_scaling
    }

    y_seg <- c(cbar_df$y, cbar_df$yend[length(cbar_df$yend)])


    cbar_plot <- cbar_plot +
      ggplot2::geom_segment(
        data = data.frame(y = y_seg, yend = y_seg),
        ggplot2::aes(y = y, yend = yend),
        size = breaksize,
        x = x - 0.05 * mul * spacing_scaling,
        xend = xend, inherit.aes = FALSE
      ) +
      ggplot2::annotate(
        geom = "text",
        x = x - 0.1 * mul * spacing_scaling, y = val_br[-length(val_br)] + diff(val_br) / 2,
        label = breaks,
        size = font_size * 5 / 14
      ) +
      ggplot2::scale_x_continuous(expand = c(expand_size, expand_size)) +
      ggplot2::scale_fill_manual(values = plotcolors) +
      ggplot2::theme_void()
  }

  if (!is.null(legend_title)) {
    cbar_plot <- cbar_plot + ggplot2::annotate(
      geom = "text", x = legend_position,
      y = mean(r_breaks), label = legend_title, angle = angle,
      size = font_size
    )
  }
  cbar_plot
}
