#' @export
pixelate <- function(img,
                     scale = 8,
                     pixel_width = NULL,
                     pixel_height = NULL,
                     keep_resolution = TRUE,
                     output_width = NULL,
                     output_height = NULL) {
  img_info <- image_info(img)
  img_height <- img_info$height
  img_width <- img_info$width
  aspect_ratio <- img_width/img_height
  pixel_size_re <- list(pixel_width,
                        pixel_height)
  if(all(sapply(pixel_size_re, is.null))) {

    scale <- ifelse(
      scale && scale>0 && scale <=50,
      scale*0.01,
      8*0.01)

    if(img_width > 800 || img_height > 800) {
      scale <- scale * 0.25
      # scale <- 0.08
    }
    scaledW <- img_width *scale
    scaledH <- img_height *scale

  } else {
    if(sum(sapply(pixel_size_re, is.null))==1) {
      if(!is.null(pixel_size_re[[1]])) {
        pixel_width <- pixel_size_re[[1]]
        pixel_height <- pixel_width/aspect_ratio
      } else {
        pixel_height <- pixel_size_re[[2]]
        pixel_width <- pixel_height * aspect_ratio
      }
    }
    scaledW <- pixel_width
    scaledH <- pixel_height


  }


  temp_img <- image_resize(
    img,
    pixel_size <- geometry_size_pixels(
      width = scaledW,
      height = scaledH))
  message("pixel size: ", pixel_size)

  if(sum(is.null(output_width), is.null(output_height)) == 1) {

    if(!is.null(output_width)) {
      img_width <- output_width
      img_height <- img_width/aspect_ratio
    }
    if(!is.null(output_height)) {
      img_height <- output_height
      img_width <- img_height * aspect_ratio
    }
  } else if(all(!is.null(output_width), !is.null(output_height))) {
    img_width <- output_width
    img_height <- output_height
  } else if(!keep_resolution) {
    img_width <- pixel_width
    img_height <- pixel_height
  }
  temp_img %>%
    image_resize(
      geometry_size_pixels(
        width = img_width,
        height = img_height),
      filter = "point")
}








