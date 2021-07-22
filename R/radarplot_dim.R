#radar_dim

radarplot_dim <- function(data, x, y, width, values = FALSE) {
  df <- data
  lab <- df$dimension
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
      #ggplot2::geom_text(aes(label = lab, x = pos, y = y), size = 2) +
      ggplot2::xlab(xlabel) +
      ggplot2::ylab(ylabel) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = xlabel))
  )
  if(values) {
    p + ggplot2::geom_text(ggplot2::aes(x = pos, y = 0, label = y, vjust = -0.5))
  } else {
    p
  }
}

positions <- function(width) {
  0.5 * (cumsum(width) + cumsum(c(0, width[-length(width)])))
}