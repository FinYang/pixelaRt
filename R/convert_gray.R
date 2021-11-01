
#' Convert image into gray scale
#'
#' @param img An \code{magick-image} object.
#' @return An \code{magick-image} object.
#'
#' @export
convert_gray <- function(img) {
  image_convert(img, colorspace = "gray")
}



