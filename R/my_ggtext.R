geom_richtext <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          nudge_x = 0,
                          nudge_y = 0,
                          label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                          label.margin = unit(c(0, 0, 0, 0), "lines"),
                          label.r = unit(0.15, "lines"),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y` but not both.", call. = FALSE)
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRichText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRichText <- ggplot2::ggproto("GeomRichText", ggplot2::Geom,
                        required_aes = c("x", "y", "label"),

                        default_aes = aes(
                          colour = "black", fill = "white", size = 3.88, angle = 0, hjust = 0.5,
                          vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
                          text.colour = NULL, label.colour = NULL, label.size = 0.25
                        ),

                        draw_panel = function(data, panel_params, coord,
                                              label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
                                              label.margin = unit(c(0, 0, 0, 0), "lines"),
                                              label.r = unit(0.15, "lines"),
                                              na.rm = FALSE) {
                          data <- coord$transform(data, panel_params)
                          if (is.character(data$vjust)) {
                            data$vjust <- compute_just(data$vjust, data$y)
                          }
                          if (is.character(data$hjust)) {
                            data$hjust <- compute_just(data$hjust, data$x)
                          }

                          gridtext::richtext_grob(
                            data$label,
                            data$x, data$y, default.units = "native",
                            hjust = data$hjust, vjust = data$vjust,
                            rot = data$angle,
                            padding = label.padding,
                            margin = label.margin,
                            gp = grid::gpar(
                              col = scales::alpha(data$text.colour %||% data$colour, data$alpha),
                              fontsize = data$size * ggplot2::.pt,
                              fontfamily = data$family,
                              fontface = data$fontface,
                              lineheight = data$lineheight
                            ),
                            box_gp = grid::gpar(
                              col = scales::alpha(data$label.colour %||% data$colour, data$alpha),
                              fill = scales::alpha(data$fill, data$alpha),
                              lwd = data$label.size * ggplot2::.pt
                            ),
                            r = label.r
                          )
                        },

                        draw_key = ggplot2::draw_key_rect
)

GeomRichtext <- GeomRichText # for automated geom discovery

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
