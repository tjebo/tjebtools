#' geom_pointpath
#' @author SO::teunbrand
#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @import grid
#' @export

GeomPointPath <- ggplot2::ggproto(
  "GeomPointPath", ggplot2::GeomPoint,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE){

    # Default geom point behaviour
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    coords <- coord$transform(data, panel_params)
    my_points <- pointsGrob(
      coords$x,
      coords$y,
      pch = coords$shape,
      gp = gpar(col = alpha(coords$colour, coords$alpha),
                fill = alpha(coords$fill, coords$alpha),
                fontsize = coords$size * .pt + coords$stroke * .stroke/2,
                lwd = coords$stroke * .stroke/2))

    # New behaviour
    ## Convert x and y to units
    x <- unit(coords$x, "npc")
    y <- unit(coords$y, "npc")

    ## Make custom grob class
    my_path <- grob(
      x = x,
      y = y,
      mult = (coords$size * .pt + coords$stroke * .stroke/2) * coords$mult,
      name = "pointpath",
      gp = grid::gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$colour, coords$alpha),
        lwd = (coords$linesize * .pt),
        lty = coords$linetype,
        lineend = "butt",
        linejoin = "round", linemitre = 10
      ),
      vp = NULL,
      ### Now this is the important bit:
      cl = 'pointpath'
    )

    ## Combine grobs
    ggplot2:::ggname(
      "geom_pointpath",
      grid::grobTree(my_path, my_points)
    )
  },
  # Adding some defaults for lines and mult
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA, stroke = 0.5,
    linesize = 0.5, linetype = 1, mult = 0.5
  )
)

#' makeContent.pointpath
#' @description underlying function for geom_type_b
#' @author SO::teunbrand
#'
#' @import ggplot2
#' @export

makeContent.pointpath <- function(x){
  # Make hook for drawing
  # Convert npcs to absolute units
  x_new <- convertX(x$x, "mm", TRUE)
  y_new <- convertY(x$y, "mm", TRUE)

  # Do trigonometry stuff
  hyp <- sqrt(diff(x_new)^2 + diff(y_new)^2)
  sin_plot <- diff(y_new) / hyp
  cos_plot <- diff(x_new) / hyp

  diff_x0_seg <- head(x$mult, -1) * cos_plot
  diff_x1_seg <- (hyp - head(x$mult, -1)) * cos_plot
  diff_y0_seg <- head(x$mult, -1) * sin_plot
  diff_y1_seg <- (hyp - head(x$mult, -1)) * sin_plot

  x0 = head(x_new, -1) + diff_x0_seg
  x1 = head(x_new, -1) + diff_x1_seg
  y0 = head(y_new, -1) + diff_y0_seg
  y1 = head(y_new, -1) + diff_y1_seg
  keep <- unclass(x0) < unclass(x1)

  # Remove old xy coordinates
  x$x <- NULL
  x$y <- NULL

  # Supply new xy coordinates
  x$x0 <- unit(x0, "mm")[keep]
  x$x1 <- unit(x1, "mm")[keep]
  x$y0 <- unit(y0, "mm")[keep]
  x$y1 <- unit(y1, "mm")[keep]

  # Set to segments class
  class(x)[1] <- 'segments'
  x
}

#' geom_pointpath
#' @description makes point and line plot just as plot type = b
#' @import ggplot2
#' @export

geom_pointpath <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPointPath, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
}

