#' Produce multipanel scatterplot of cover/damage within and outside exclosures
#'
#' @param dataset Name of the dataframe.
#' @param vble character. Either "cover" or "damage".
#' @param shrub.tree character. Either "shrub" (default) or "tree".
#' @param remove.zeros Logical. Show dots with cero cover both inside and outside? Default is TRUE.
#' @param show.bare Logical. Show bare ground cover as if another species (the default), or omit from the plot?
#' @param min.n Numeric. Omit species occurring at a number of sites lower than this. Default is 1, which implies plotting all species.
#' @param psize numeric. Point size (for geom_point).
#' @param n.row,n.col Numeric. Number of rows/columns of the final plot. Default is NULL, which leaves ggplot choose the best display.
#' @param xlab character. Horizontal axis title.
#' @param ylab character. Vertical axis title.
#' @param filename character (optional). If provided, will save the plot with this filename.
#' @param ... further arguments for \code{\link[ggplot2]{ggsave}}, such as width, height, paper, etc.
#'
#' @return Prints (and optionally saves) a ggplot.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr spread

multiscatter <- function(dataset, vble, shrub.tree = "shrub",
                         remove.zeros = TRUE,
                         show.bare = TRUE, min.n = 1,
                         xlab = "Outside", ylab = "Inside",
                         fontsize = 12, psize = 4,
                         n.row = NULL, n.col = NULL,
                         filename = NULL, ...) {

  ## format dataset
  df <- dplyr::select_(dataset, .dots = list("site", "inside", "species", "acronym",
                                             "shrub_tree", vble))
  df <- filter(df, shrub_tree == shrub.tree)
  df <- mutate(df, inside = ifelse(inside == "in", "inside", "outside"))
  df <- tidyr::spread_(df, "inside", vble)
  df <- filter(df, !is.na(inside) & !is.na(outside))

  if (vble == "cover") {
    df <- mutate(df, inside = 2*inside, outside = 2*outside)
  } # because transect length is 50 m, to transform in %

  if (remove.zeros) {
    df <- filter(df, !all.equal.vectorised(inside, 0) |
                   !all.equal.vectorised(outside, 0))  # omit spp missing both out and inside
  }


  if (!all(is.na(df$inside))) {   # e.g. no data for damage in trees

    ## if show.bare, reorder species so that bare appears first
    if (vble == "cover") {
      if (show.bare) {
        spp <- sort(unique(subset(df$species, df$species != "bare")))
        df$species <- factor(df$species, levels = c("bare", spp))
      } else {
        df <- filter(df, species != "bare")
      }
    }


    ## is min.n is provided, select only those species being present in as many transects
    if (!is.null(min.n)) {
      nsites <- count(df, species)
      rare.spp <- nsites$species[nsites$n < min.n]
      df <- filter(df, !species %in% rare.spp)
    }


    # plot
    g <- ggplot(df, aes(x = outside, y = inside)) +
      facet_wrap(~species, nrow = n.row, ncol = n.col) +
      geom_point(size = psize) +
      geom_abline(intercept = 0, colour = "grey50") +
      theme_minimal(base_size = fontsize) +
      theme(panel.grid.major = element_line(colour = "#ECECEC", size = 0.3, linetype = 1)) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title = element_text(size = rel(1.9))) +
      theme(axis.title.y = element_text(vjust = 1, margin = margin(r=20))) +
      theme(axis.title.x = element_text(margin = margin(t=20))) +
      theme(axis.text.x = element_text(size = rel(1.7)),
            axis.text.y = element_text(size = rel(1.3))) +
      theme(strip.text = element_text(size = rel(0.85))) +
      labs(x = xlab, y = ylab)

    if (vble == "damage") {
      g <- g + coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
        scale_x_continuous(breaks = c(0:5)) +
        scale_y_continuous(breaks = c(0:5),
                           labels = c("0", "1", "2", "3", "4", "5"))
    }

    print(g)


    if (!is.null(filename)) ggsave(filename, ...)

    invisible(g)

  } else message("Nothing to plot.")

}



