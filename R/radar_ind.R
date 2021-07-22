# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties

radarplot_ind <- function(data, x, y, width) {
  df <- data
  lab <- df$indicators
  xlabel <- as.character(substitute(x))
  ylabel <- as.character(substitute(y))
  x <- as.character(eval(substitute(x), df))
  y <- eval(substitute(y), df)
  width <- eval(substitute(width), df)
  pos <- positions(width)
  natest <- data$natest
  
  p <- suppressWarnings(
    ggplot2::ggplot() +
      geom_hline(yintercept = seq(0, 10, by = 2), colour = "grey90", size = 0.2) +
      ggplot2::geom_bar(ggplot2::aes(x = pos, width = width, y = y, fill = x),
                        stat = "identity", colour = "black", size =.1) +
      geom_bar(aes(x = pos, width = width, y = natest), fill = "white",
               stat = "identity", colour = "white", size =.1, alpha = 1) +  # NA
      ggplot2::scale_x_continuous(label = lab, breaks = pos) +
      ggplot2::xlab(xlabel) +
      ggplot2::ylab(ylabel) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = xlabel))
  )

}

positions <- function(width) {
  0.5 * (cumsum(width) + cumsum(c(0, width[-length(width)])))
}