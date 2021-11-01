#' Calculate distance between RGB
#' #' @export
colorSim <- function(rgbColor = c(0, 0, 0),
                     compareColor = c(0, 0, 0)) {
  sqrt(sum((rgbColor - compareColor)^2))
}

#' Calculate distance between RGB
#' #' @export
similarColor <- function(actualColor, palette = palette_list) {
  palette[[which.min(sapply(palette, \(x)colorSim(actualColor, x)))]]
}

#' Check the color type
#'
#' @param colors charactor vector or list of numeric vector
#' @return Character string, type of color
#'
#' @export
color_type <- function(colors) {
  if(!is.list(colors)) {
    if(
      unique(substr(colors, 1, 1)) == "#" &&
      unique(nchar(colors)) %in% c(7,9)
    ) {
      return("hex")
    }

    if(length(colors) == 3) {
      return("rgb")
    }

  } else {
    return("rgb")


  }
}

#' @export
preview <- function(img, ...){
  hw <- grDevices::dev.size("px")
  img %>%
    magick::image_scale(paste0(hw, collapse="x")) %>%
    print( info = FALSE, ...)
  print(image_info(img))
  invisible(img)
}

#' @export
preview_color <- function(colors, name = NULL){
  if(is.list(colors)){
    tag <- sapply(colors, \(x) do.call(\(...) rgb(..., maxColorValue = 255), as.list(x)))
  } else {
    tag <- colors
  }
  if(!is.null(name)) name <- paste0(name,"\n", tag)
  else name <- tag
  x <- min(5, length(tag))

  y <- rep(seq_len(ceiling(length(tag)/x)),each = x)[1:length(tag)]
  x <- rep(seq_len(x), length = length(tag))
  p_data <- tibble(x, y, tag, name)
  p_data %>%
    ggplot(aes(x=x, y=y, fill = tag, label = name))+
    geom_raster() +
    geom_text() +
    scale_fill_manual(values = `names<-`(p_data$tag, p_data$tag)) +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank())

}
