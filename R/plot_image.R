#' Display image explanations as superpixel areas
#'
#' When classifying images one is often interested in seeing the areas that
#' supports and/or contradicts a classification. `plot_image_explanation()` will
#' take the result of an image explanation and highlight the areas found
#' relevant to each label in the explanation. The highlighting can either be
#' done by blocking the parts of the image not related to the classification, or
#' by encircling and colouring the areas that influence the explanation.
#'
#' @param explanation The explanation created with an `image_explainer`
#' @param which The case in `explanation` to illustrate. `plot_image_explanation`
#' only supports showing one case at a time.
#' @param threshold The lowest absolute weighted superpixels to include
#' @param show_negative Should areas that contradicts the prediction also be
#' shown
#' @param display How should the areas be shown? Either `outline` or `block`
#' @param fill_alpha In case of `display = 'outline'` how opaque should the area
#' colour be?
#' @param outline_col A vector of length 2 giving the colour for supporting and
#' contradicting areas respectively if `display = 'outline'`
#' @param block_col The colour to use for the unimportant areas if
#' `display = 'block'`
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' # load precalculated explanation as it takes a long time to create
#' explanation <- .load_image_example()
#'
#' # Default
#' plot_image_explanation(explanation)
#'
#' # Block out background instead
#' plot_image_explanation(explanation, display = 'block')
#'
#' # Show negatively correlated areas as well
#' plot_image_explanation(explanation, show_negative = TRUE)
#'
plot_image_explanation <- function(explanation, which = 1, threshold = 0.01,
                                   show_negative = FALSE,
                                   display = 'outline', fill_alpha = 0.3,
                                   outline_col = c('lightgreen', 'red'),
                                   block_col = 'grey') {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package magick needed for this function to work. Please install it.", call. = FALSE)
  }
  display <- match.arg(display, c('outline', 'block'))
  explanation <- explanation[explanation$case == unique(explanation$case)[1], , drop = FALSE]
  explanation$label <- factor(explanation$label, unique(explanation$label[order(explanation$label_prob, decreasing = TRUE)]))
  explanation$fit <- format(explanation$model_r2, digits = 2)
  im <- magick::image_read(explanation$data[[1]])
  raster <- do.call('rbind', lapply(split(explanation, explanation$label), function(exp) {
    pos <- exp[exp$feature_weight > threshold, , drop = FALSE]
    im_hl <- hightlight_segments(im, unlist(pos$feature_value), display, fill_alpha, outline_col[1], block_col)
    im_hl <- tidy_raster(im_hl)
    im_hl$type <- 'Supports'
    if (show_negative) {
      neg <- exp[exp$feature_weight <= -threshold, , drop = FALSE]
      if (nrow(neg) > 0) {
        im_neg <- hightlight_segments(im, unlist(neg$feature_value), display, fill_alpha, rep(outline_col, length.out = 2)[2], block_col)
      } else {
        im_neg <- if (display == 'outline') {
          im
        } else {
          magick::image_blank(magick::image_info(im)$width, magick::image_info(im)$height, color = block_col)
        }
      }
      im_neg <- tidy_raster(im_neg)
      im_neg$type <- 'Contradicts'
      im_hl <- rbind(im_hl, im_neg)
    }
    im_hl$label <- exp$label[1]
    im_hl$probability <- format(exp$label_prob[1], digits = 2)
    im_hl$`Explanation fit` <- exp$fit[1]
    im_hl
  }))
  raster$type <- factor(raster$type, levels = c('Supports', 'Contradicts'))

  p <- ggplot(raster) +
    geom_raster(aes_(~x, ~y, fill = ~colour)) +
    coord_fixed(expand = FALSE) +
    scale_y_reverse() +
    scale_fill_identity() +
    theme_lime() +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())

  if (show_negative) {
    p <- p + facet_grid(type ~ label + probability + `Explanation fit`, labeller = label_both_upper, switch = 'y')
  } else {
    p <- p + facet_wrap(~ label + probability + `Explanation fit`, labeller = label_both_upper)
  }
  p
}

#' Test super pixel segmentation
#'
#' The segmentation of an image into superpixels are an important step in
#' generating explanations for image models. It is both important that the
#' segmentation is correct and follows meaningful patterns in the picture, but
#' also that the size/number of superpixels are appropriate. If the important
#' features in the image are chopped into too many segments the permutations
#' will probably damage the picture beyond recognition in almost all cases
#' leading to a poor or failing explanation model. As the size of the object of
#' interest is varying it is impossible to set up hard rules for the number of
#' superpixels to segment into - the larger the object is relative to the size
#' of the image, the fewer superpixels should be generated. Using
#' `plot_superpixels` it is possible to evaluate the superpixel parameters
#' before starting the time consuming explanation function.
#'
#' @param path The path to the image. Must be readable by [magick::image_read()]
#' @param n_superpixels The number of superpixels to segment into
#' @param weight How high should locality be weighted compared to colour. High
#' values leads to more compact superpixels, while low values follow the image
#' structure more
#' @param n_iter How many iterations should the segmentation run for
#' @param colour What line colour should be used to show the segment boundaries
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' image <- system.file('extdata', 'produce.png', package = 'lime')
#'
#' # plot with default settings
#' plot_superpixels(image)
#'
#' # Test different settings
#' plot_superpixels(image, n_superpixels = 100, colour = 'white')
#'
plot_superpixels <- function(path, n_superpixels = 400, weight = 20, n_iter = 10,
                             colour = 'black') {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package magick needed for this function to work. Please install it.", call. = FALSE)
  }
  im <- magick::image_read(path)
  im_lab <- magick::image_convert(im, colorspace = 'LAB')
  super_pixels <- slic(
    magick::image_channel(im_lab, 'R')[[1]][1,,],
    magick::image_channel(im_lab, 'G')[[1]][1,,],
    magick::image_channel(im_lab, 'B')[[1]][1,,],
    n_sp = n_superpixels,
    weight = weight,
    n_iter = n_iter
  ) + 1
  contour <- magick::image_read(array(t(super_pixels)/max(super_pixels),
                              dim = c(rev(dim(super_pixels)), 1)),
                        depth = 16)
  contour <- magick::image_convolve(contour, 'Laplacian')[[1]]
  contour[contour != as.raw(0)] <- as.raw(255)
  lines <- magick::image_blank(magick::image_info(im)$width, magick::image_info(im)$height, color = colour)
  lines <- magick::image_convert(lines, type = 'TrueColorAlpha')[[1]]
  lines[4,,] <- contour[1,,]
  raster <- tidy_raster(magick::image_composite(im, magick::image_read(lines)))

  ggplot(raster) +
    geom_raster(aes_(~x, ~y, fill = ~colour)) +
    coord_fixed(expand = FALSE) +
    scale_y_reverse() +
    scale_fill_identity() +
    theme_void()
}

#' @importFrom grDevices rgb
hightlight_segments <- function(im, pixels, display, fill_alpha, outline_col,
                                block_col) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package magick needed for this function to work. Please install it.", call. = FALSE)
  }
  area <- matrix(as.raw(0), ncol = magick::image_info(im)$width, nrow = magick::image_info(im)$height)
  area[pixels] <- as.raw(255)
  area <- magick::image_read(array(area, dim = c(1, rev(dim(area)))))
  if (display == 'outline') {
    lines <- magick::image_convolve(area, 'Laplacian')
    area <- magick::image_composite(
      magick::image_blank(magick::image_info(im)$width, magick::image_info(im)$height, color = rgb(fill_alpha, fill_alpha, fill_alpha)),
      area,
      'Darken'
    )
    lines <- magick::image_composite(area, lines, 'lighten')
    hl <- magick::image_blank(magick::image_info(im)$width, magick::image_info(im)$height, color = outline_col)
    hl <- magick::image_convert(hl, type = 'TrueColorAlpha')[[1]]
    hl[4,,] <- lines[[1]][1,,]
  } else {
    area <- magick::image_negate(area)
    hl <- magick::image_blank(magick::image_info(im)$width, magick::image_info(im)$height, color = block_col)
    hl <- magick::image_convert(hl, type = 'TrueColorAlpha')[[1]]
    hl[4,,] <- area[[1]][1,,]
  }
  magick::image_composite(im, magick::image_read(hl))
}
#' @importFrom grDevices as.raster
tidy_raster <- function(im) {
  raster <- as.raster(im)
  data.frame(x = rep(seq_len(ncol(raster)), nrow(raster)),
             y = rep(seq_len(nrow(raster)), each = ncol(raster)),
             colour = as.vector(raster),
             stringsAsFactors = FALSE)
}
