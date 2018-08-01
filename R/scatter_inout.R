#' Create scatterplot of number of species, cover or height inside vs outside exclosures
#'
#' @inheritParams multiscatter
#' @param vble Character. Either "nspp", "cover", or "height".
#' @param jitter Logical.
#'
#' @return Prints (and optionally saves) a ggplot.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr spread
#'
scatter_inout <- function(dataset, vble, shrub.tree = "shrub",
                               xlab = "Outside", ylab = "Inside",
                               fontsize = 12, psize = 6, jitter = TRUE,
                               filename = NULL, ...) {

  ## prepare dataset

  df <- dataset %>%
    filter(shrub_tree == shrub.tree) %>%
    mutate(inside = ifelse(inside == "in", "inside", "outside"))

  if (vble == "cover") {
    df <- df %>%
      filter(acronym == "Bare") %>%
      mutate(y = 2*(50 - cover))
  }

  if (vble == "nspp") {
    df <- df %>%
      filter(acronym != "Bare", presence == 1) %>%
      group_by(site, inside) %>%
      summarise(y = sum(presence))
  }

  if (vble == "height") {
    df <- df %>%
      distinct(site, inside, shrub_height) %>%
      rename(y = shrub_height)
  }

  df <- df %>%
    dplyr::select(site, inside, y) %>%
    tidyr::spread(inside, y)




  ## plot

  if (jitter) pos = "jitter" else pos = "identity"

  g <- ggplot(df, aes(x = outside, y = inside)) +
    geom_point(size = psize, position = pos) +
    geom_abline(colour = "grey30") +
    xlab(xlab) +
    ylab(ylab) +
    theme_cowplot(fontsize) +
    background_grid(major = "xy")

  if (vble == "cover") {
    g <- g + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
  }

  if (vble == "nspp" | vble == "height") {
    g <- g + coord_cartesian(xlim = c(0, max(df$inside, df$outside)),
                             ylim = c(0, max(df$inside, df$outside)))
  }


  print(g)

  if (!is.null(filename)) ggsave(filename, ...)

  invisible(g)

}
