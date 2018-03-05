#' @rdname lime
#' @export
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
#' @param batch_size The number of explanations to handle at a time
#' @param background The colour to use for blocked out superpixels
#'
#' @importFrom magick image_read image_convert image_channel image_background image_write
#' @importFrom methods as
#' @export
explain.imagefile <- function(x, explainer, labels = NULL, n_labels = NULL,
                              n_features, n_permutations = 1000,
                              feature_select = 'auto', n_superpixels = 400,
                              weight = 20, n_iter = 10, p_remove = 0.5,
                              batch_size = 100, background = 'grey', ...) {
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
    im <- image_read(ind)
    im_lab <- image_convert(im, colorspace = 'LAB')
    super_pixels <- slic(
      image_channel(im_lab, 'R')[[1]][1,,],
      image_channel(im_lab, 'G')[[1]][1,,],
      image_channel(im_lab, 'B')[[1]][1,,],
      n_sp = n_superpixels,
      weight = weight,
      n_iter = n_iter
    ) + 1
    im_raw <- im[[1]]
    perms <- matrix(sample(c(TRUE, FALSE), n_permutations * max(super_pixels), TRUE), nrow = n_permutations)
    perms[1, ] <- FALSE
    batches <- rep(seq_len(n_permutations), each = batch_size, length.out = n_permutations)
    case_res <- do.call(rbind, lapply(batches, function(b) {
      perm_files <- vapply(b, function(i) {
        tmp <- tempfile()
        im_perm <- im_raw
        im_perm[4,,][super_pixels %in% which(perms[i,])] <- as.raw(0)
        im_perm <- image_read(im_perm)
        im_perm <- image_background(im_perm, background)
        image_write(im_perm, path = tmp, format = 'png')
        tmp
      }, character(1))
      batch_res <- predict_model(explainer$model, newdata = explainer$preprocess(perm_files), type = o_type)
      unlink(perm_files)
      batch_res
    }))
    perms_sparse <- as(perms, 'dgCMatrix')
    case_dist <- cosine_distance_vector_to_matrix_rows(perms_sparse[1,], perms_sparse)
    colnames(perms) <- as.character(seq_len(nrow(perms)))
    res <- model_permutations(perms, case_res, case_dist, labels, n_labels, n_features, feature_select)
    res$feature_value <- lapply(as.integer(res$feature), function(i) which(super_pixels == i))
    res$feature_desc <- describe_superpixel(as.integer(res$feature), super_pixels)
    res$case <- basename(ind)
    res$label_prob <- unname(as.matrix(case_res[1, ]))[match(res$label, colnames(case_res))]
    res$data <- list(im_raw)
    class(res$data) <- 'bitmap_list'
    res$prediction <- list(as.list(case_res[1, ]))
    res$model_type <- m_type
    res
  })
  res <- do.call(rbind, res)
  res <- res[, c('model_type', 'case', 'label', 'label_prob', 'model_r2', 'model_intercept', 'model_prediction', 'feature', 'feature_value', 'feature_weight', 'feature_desc', 'data', 'prediction')]
  if (m_type == 'regression') {
    res$label <- NULL
    res$label_prob <- NULL
    res$prediction <- unlist(res$prediction)
  }
  res
}
is.image_explainer <- function(x) inherits(x, 'image_explainer')

describe_superpixel <- function(i, superpixels) {
  vapply(i, function(ii) {
    which_sp <- superpixels
    which_sp[] <- superpixels == ii
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
