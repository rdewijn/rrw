#  ggbiplot.r
#
#  Copyright 2011 Vincent Q. Vu.
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

#' Biplot for Principal Components using ggplot2
#'
#' @param pcobj           an object returned by prcomp() or princomp()
#' @param choices         which PCs to plot
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param obs.scale       scale factor to apply to observations
#' @param var.scale       scale factor to apply to variables
#' @param pc.biplot       for compatibility with biplot.princomp()
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
#' @param ellipse         draw a normal data ellipse for each group?
#' @param ellipse.prob    size of the ellipse in Normal probability
#' @param labels          optional vector of labels for the observations
#' @param labels.size     size of the text used for the labels
#' @param alpha           alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
#' @param var.axes        draw arrows for the variables?
#' @param varname.size    size of the text for variable names
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param, obspoint.size   size of the points for the observations
#' @param varname.abbrev  whether or not to abbreviate the variable names
#'
#' @return                a ggplot2 plot
#' @export
#' @import ggplot2 plyr scales grid
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
#'
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE,
                     obs.scale = 1 - scale, var.scale = scale,
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68,
                     labels = NULL, labels.size = 3, alpha = 1,
                     var.axes = TRUE,
                     circle = FALSE, circle.prob = 0.69,
                     varname.size = 3, varname.adjust = 1.5,
                     obspoint.size = 2,
                     varname.abbrev = FALSE, ...)
{

  stopifnot(length(choices) == 2)

  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }

  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)

  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))

  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs,
                       sprintf('(%0.1f%% explained var.)',
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))

  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }


  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) +
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()

  if(var.axes) {
    # Draw circle
    if(circle)
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'),
                         size = 1/2, alpha = 1/3)
    }

    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')),
                   color = muted('red'))
  }

  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups),
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha, size = obspoint.size)
    } else {
      g <- g + geom_point(alpha = alpha)
    }
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))

    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'),
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }

  # Label the variable axes
  if(var.axes) {
    g <- g +
      geom_text(data = df.v,
                aes(label = varname, x = xvar, y = yvar,
                    angle = angle, hjust = hjust),
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)),
  #                               palette = 'Dark2')
  # }

  # TODO: Add a second set of axes

  return(g)
}

#' Biplot for Principal Components using ggplot2
#'
#' @param df_long       a long data frame
#' @param variables     string indicating which column to use for the variable names
#' @param group         string indicating which column to use for the group coloring
#' @param value         string indicating which column to use for the values (default: "value")
#'
#' @return                a ggplot2 plot
#' @export
#' @import ggplot2 dplyr tidyr

ggradar <- function(df_long, variables, group, value = "value") {

  library(ggplot2)
  library(dplyr)
  library(tidyr)

  vars <- unique(df_long[[variables]])
  n_vars <- length(vars)

  # Compute angles for each variable
  angles <- tibble(
    variable = vars,
    angle = seq(pi/2, pi/2 + 2*pi * (n_vars - 1) / n_vars, length.out = n_vars)
  )

  # Rename columns to standard names for internal use
  df_internal <- df_long %>%
    select(variable = all_of(variables), group = all_of(group), value = all_of(value))

  # Join angles and convert polar → Cartesian
  df_cart <- df_internal %>%
    left_join(angles, by = "variable") %>%
    mutate(
      x = value * cos(angle),
      y = value * sin(angle)
    )

  # Close each polygon by repeating the first point per group
  df_cart <- df_cart %>%
    group_by(group) %>%
    group_modify(~ bind_rows(.x, slice(.x, 1))) %>%
    ungroup()

  # Axis label positions (slightly outside max value)
  max_val <- max(df_internal$value, na.rm = TRUE)
  label_df <- angles %>%
    mutate(
      x = max_val * 1.15 * cos(angle),
      y = max_val * 1.15 * sin(angle)
    )

  # Grid circles at 25%, 50%, 75%, 100% of max
  grid_vals <- max_val * c(0.25, 0.5, 0.75, 1)
  grid_df <- expand.grid(r = grid_vals, theta = seq(0, 2*pi, length.out = 200)) %>%
    mutate(x = r * cos(theta), y = r * sin(theta))

  # Build plot
  ggplot() +
    geom_path(data = grid_df, aes(x, y, group = r), color = "grey80", linewidth = 0.3) +
    geom_segment(data = angles, aes(x = 0, y = 0,
                                    xend = max_val * cos(angle),
                                    yend = max_val * sin(angle)),
                 color = "grey70", linewidth = 0.3) +
    geom_polygon(data = df_cart, aes(x, y, group = group, fill = group), alpha = 0.2) +
    geom_path(data = df_cart, aes(x, y, group = group, color = group), linewidth = 1) +
    geom_point(data = df_cart, aes(x, y, color = group), size = 3) +
    geom_text(data = label_df, aes(x, y, label = variable), size = 4) +
    coord_equal() +
    theme_void() +
    theme(legend.title = element_blank()) +
    labs(title = "Radar Chart")
}

#' Save a ggplot as publication-ready PDF and TIFF
#'
#' Writes the plot twice: a vector PDF (via `cairo_pdf`, which embeds fonts)
#' and a 1000 DPI LZW-compressed TIFF suitable for line art submissions.
#'
#' Objects that are not ggplots or grobs but draw when printed, such as the
#' output of `survminer::ggsurvplot()`, are rendered by opening the device
#' directly and calling `print()`.
#'
#' @param plot      a ggplot object, grob, or printable plot object (e.g. a `ggsurvplot`)
#' @param filename  output path without extension; `.pdf` and `.tiff` are appended
#' @param width     figure width in inches (default: 6)
#' @param height    figure height in inches (default: 6)
#'
#' @return          called for its side effect of writing files; invisibly returns
#'                  the paths of the PDF and TIFF files
#' @export
#' @import ggplot2
#' @importFrom grDevices cairo_pdf tiff dev.off
#' @examples
#' \dontrun{
#'   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'   save_ggfigure(p, "figures/mtcars_scatter", width = 5, height = 4)
#' }
save_ggfigure <- function(plot, filename, width = 6, height = 6) {
  pdf_file <- paste0(filename, ".pdf")
  tiff_file <- paste0(filename, ".tiff")

  if (inherits(plot, c("gg", "grob", "gtable"))) {
    # vector PDF (cairo embeds fonts properly)
    ggsave(pdf_file, plot,
           width = width, height = height, units = "in",
           device = cairo_pdf)
    # raster TIFF, minimum 1000 DPI for line art
    ggsave(tiff_file, plot,
           width = width, height = height, units = "in",
           dpi = 1000, device = "tiff", compression = "lzw")
  } else {
    # objects that are not ggplots/grobs but have a print method that draws
    # (e.g. survminer's ggsurvplot, a list of plot + risk table):
    # open the device ourselves and print into it
    draw_to <- function(open_device) {
      open_device()
      on.exit(dev.off(), add = TRUE)
      print(plot, newpage = FALSE)
    }
    draw_to(\() cairo_pdf(pdf_file, width = width, height = height))
    draw_to(\() tiff(tiff_file, width = width, height = height, units = "in",
                     res = 1000, compression = "lzw"))
  }

  invisible(c(pdf_file, tiff_file))
}