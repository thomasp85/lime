#' @rdname lime
#' @export
#'
#' @examples
#' \dontrun{
#' library(keras)
#' library(abind)
#' # get some image
#' img_path <- system.file('extdata', 'produce.png', package = 'lime')
#' # load a predefined image classifier
#' model <- application_vgg16(
#'   weights = "imagenet",
#'   include_top = TRUE
#' )
#'
#' # create a function that prepare images for the model
#' img_preprocess <- function(x) {
#'   arrays <- lapply(x, function(path) {
#'     img <- image_load(path, target_size = c(224,224))
#'     x <- image_to_array(img)
#'     x <- array_reshape(x, c(1, dim(x)))
#'     x <- imagenet_preprocess_input(x)
#'   })
#'   do.call(abind, c(arrays, list(along = 1)))
#' }
#'
#' # Create an explainer (lime recognise the path as an image)
#' explainer <- lime(img_path, as_classifier(model, unlist(labels)), img_preprocess)
#'
#' # Explain the model (can take a long time depending on your system)
#' explanation <- explain(img_path, explainer, n_labels = 2, n_features = 10, n_superpixels = 70)
#' }
lime.imagefile <- function(x, model, preprocess, ...) {
  assert_that(is.function(preprocess))
  assert_that(!is.null(model))

  explainer <- c(as.list(environment()), list(...))
  explainer$x <- NULL

  structure(explainer, class = c('image_explainer', 'explainer', 'list'))
}
#' @rdname explain
#' @param n_superpixels The number of segments an image should be split into
#' @param weight How high should locality be weighted compared to colour. High
#' values leads to more compact superpixels, while low values follow the image
#' structure more
#' @param n_iter How many iterations should the segmentation run for
#' @param p_remove The probability that a superpixel will be removed in each
#' permutation
#' @param batch_size The number of explanations to handle at a time
#' @param background The colour to use for blocked out superpixels
#'
#' @importFrom methods as
#' @export
explain.imagefile <- function(x, explainer, labels = NULL, n_labels = NULL,
                              n_features, n_permutations = 1000,
                              feature_select = 'auto', n_superpixels = 400,
                              weight = 20, n_iter = 10, p_remove = 0.5,
                              batch_size = 10, background = 'grey', ...) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package magick needed for this function to work. Please install it.", call. = FALSE)
  }

  assert_that(is.image_explainer(explainer))
  m_type <- model_type(explainer)
  o_type <- output_type(explainer)
  if (m_type == 'regression') {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning('"labels" and "n_labels" arguments are ignored when explaining regression models')
    }
    n_labels <- 1
    labels <- NULL
  }
  assert_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  assert_that(is.count(n_features))
  assert_that(is.count(n_permutations))
  assert_that(is.count(n_superpixels))
  assert_that(is.count(batch_size))

  res <- lapply(x, function(ind) {
    im <- magick::image_read(ind)
    im_lab <- magick::image_convert(im, colorspace = 'LAB')
    super_pixels <- slic(
      magick::image_channel(im_lab, 'R')[[1]][1,,],
      magick::image_channel(im_lab, 'G')[[1]][1,,],
      magick::image_channel(im_lab, 'B')[[1]][1,,],
      n_sp = n_superpixels,
      weight = weight,
      n_iter = n_iter
    ) + 1
    im_raw <- magick::image_convert(im, type = 'TrueColorAlpha')[[1]]
    perms <- matrix(sample(c(TRUE, FALSE), n_permutations * max(super_pixels), TRUE, c(p_remove, 1-p_remove)), nrow = n_permutations)
    perms[1, ] <- FALSE
    batches <- rep(seq_len(n_permutations), each = batch_size, length.out = n_permutations)
    batches <- split(seq_along(batches), batches)
    case_res <- do.call(rbind, lapply(batches, function(b) {
      perm_files <- vapply(b, function(i) {
        tmp <- tempfile()
        im_perm <- im_raw
        im_perm[4,,][super_pixels %in% which(perms[i,])] <- as.raw(0)
        im_perm <- magick::image_read(im_perm)
        im_perm <- magick::image_background(im_perm, background)
        magick::image_write(im_perm, path = tmp, format = 'png')
        tmp
      }, character(1))
      batch_res <- predict_model(explainer$model, newdata = explainer$preprocess(perm_files), type = o_type)
      unlink(perm_files)
      batch_res
    }))
    case_res <- set_labels(case_res, explainer$model)
    perms_sparse <- as(!perms, 'dgCMatrix')
    case_dist <- cosine_distance_vector_to_matrix_rows(perms_sparse[1,], perms_sparse)
    colnames(perms_sparse) <- as.character(seq_len(ncol(perms)))
    res <- model_permutations(perms_sparse, case_res, case_dist, labels, n_labels, n_features, feature_select)
    res$feature_value <- lapply(as.integer(res$feature), function(i) which(super_pixels == i))
    res$feature_desc <- describe_superpixel(as.integer(res$feature), super_pixels)
    res$case <- basename(ind)
    res$label_prob <- unname(as.matrix(case_res[1, ]))[match(res$label, colnames(case_res))]
    res$data <- list(im_raw)
    res$prediction <- list(as.list(case_res[1, ]))
    res$model_type <- m_type
    res
  })
  res <- do.call(rbind, res)
  class(res$data) <- 'bitmap_list'
  class(res$feature_value) <- 'superpixel_list'
  res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'model_prediction', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
  if (m_type == 'regression') {
    res$label <- NULL
    res$label_prob <- NULL
    res$prediction <- unlist(res$prediction)
  }
  res
}
is.image_explainer <- function(x) inherits(x, 'image_explainer')

#' Load an example image explanation
#'
#' This function is needed to cut down on package size. It reassembles the
#' explanation data.frame by attaching the image data to the saved data.frame
#'
#' @return A data.frame containing an example of a image explanation
#'
#' @keywords internal
#' @export
.load_image_example <- function() {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package magick needed for this function to work. Please install it.", call. = FALSE)
  }
  exp <- readRDS(system.file('extdata', 'image_explanation.rds', package = 'lime'))
  img <- magick::image_read(system.file('extdata', 'produce.png', package = 'lime'))
  exp$data <- list(magick::image_convert(img, type = 'TrueColorAlpha')[[1]])
  class(exp$data) <- 'bitmap_list'
  exp
}

describe_superpixel <- function(i, superpixels) {
  vapply(i, function(ii) {
    which_sp <- superpixels == ii
    rows <- range(which(apply(which_sp, 1, any)))
    cols <- range(which(apply(which_sp, 2, any)))
    paste0('[', cols[1], '-', cols[2],'], [', rows[1], '-', rows[2], ']')
  }, character(1))
}
#' @export
format.bitmap <- function(x, ...) {
  dims <- dim(x)
  sprintf("%d channel %dx%d bitmap", dims[1], dims[2], dims[3])
}
#' @export
format.bitmap_list <- function(x, ...) {
  vapply(x, format, character(1))
}
#' @export
format.superpixel_list <- function(x, ...) {
  vapply(x, function(el) {paste0(length(el), 'px superpixel')}, character(1))
}
#' @importFrom tools file_ext
is.image_file <- function(x) {
  all(file.exists(x) && all(tolower(file_ext(x)) %in% image_ext))
}
image_ext <- c(
  'jpg', 'jpeg',
  'bmp',
  'png',
  'tiff', 'tif',
  'gif', # only with hard g
  'bpg'
)
