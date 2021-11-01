
#' Convert the color of image to the color in the palette
#'
#' @param img An \code{magick-image} object.
#' @param palette A palette.
#'
#' @return An \code{magick-image} object.
#'
#' @export
convert_palette <- function(img, palette = pixelit_16) {

  if(color_type(palette) == "hex") {
    palette <- col2rgb(palette) %>%
      t() %>%
      split(seq_len(nrow(.))) %>%
      unname()
  }

  raster_df <- img %>%
    # image_flip()%>%
    image_raster() %>%
    as_tibble()
  raster_unique <- distinct(raster_df, col)
  raster_joined <- raster_unique %>%
    mutate(new_col = sapply(
      col,
      \(x) {
        x %>%
          col2rgb() %>%
          similarColor(palette) %>%
          as.list() %>%
          do.call(\(...) rgb(..., maxColorValue = 255), .)
      }) ) %>%
    right_join(raster_df, by = "col") %>%
    arrange( y, x)
  raster_joined %>%
    .$new_col %>%
    # raster_df %>%
    #   .$col %>%
    matrix(byrow = TRUE, ncol = max(raster_df$x)) %>%
    image_read()
}
