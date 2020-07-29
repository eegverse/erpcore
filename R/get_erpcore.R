#' Download ERP Core N170
#'
#' @param dest_path destination file path
#' @param file_name destination file name
#' @export
get_N170 <- function(dest_path = NULL,
                     file_name = NULL) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1CIw8tlnRUNURYdEBhTEvhtBycyKfGuvV")
  file_out <- meta_dat$name

  if (!is.null(dest_path)) {
    zip_file <- paste0(dest_path, file_out)
  }
  googledrive::drive_download(googledrive::as_id(meta_dat$id),
                              path = dest_path)

  utils::unzip(file.path(paste0(dest_path,
                                file_out)))
}

#' Download ERP Core MMN
#'
#' @param dest_path destination file path
#' @param file_name destination file name
#' @param overwrite overwrite existing files (defaults to FALSE)
#' @export
get_mmn <- function(dest_path = NULL,
                    file_name = NULL,
                    overwrite = FALSE) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1_QGoOUmPO4xPre_Q2ujSgatRS7kf5O5u")
  file_out <- tools::file_path_sans_ext(meta_dat$name)

  if (!is.null(dest_path)) {
    zip_file <- paste0(file.path(dest_path,
                                 file_out),
                       ".zip")
  } else {
    zip_file <- file_out
  }

  googledrive::drive_download(file = googledrive::as_id(meta_dat$id),
                              path = zip_file,
                              overwrite = overwrite)

  utils::unzip(file.path(paste0(zip_file)))
}

#' Download ERP Core n2pc
#'
#' @param dest_path destination file path
#' @param file_name destination file name
#' @param overwrite overwrite existing files (defaults to FALSE)
#' @export
get_n2pc <- function(dest_path = NULL,
                    file_name = NULL,
                    overwrite = FALSE) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1AaW5mps1KxhMoI3U1Kw3wtJj_DXbrU1C")
  file_out <- tools::file_path_sans_ext(meta_dat$name)

  if (!is.null(dest_path)) {
    zip_file <- paste0(file.path(dest_path,
                                 file_out),
                       ".zip")
  } else {
    zip_file <- file_out
  }

  googledrive::drive_download(file = googledrive::as_id(meta_dat$id),
                              path = zip_file,
                              overwrite = overwrite)

  utils::unzip(file.path(paste0(zip_file)))
}

#' Download ERP Core N400
#'
#' @param dest_path destination file path
#' @param file_name destination file name
#' @param overwrite overwrite existing files (defaults to FALSE)
#' @export
get_n400 <- function(dest_path = NULL,
                    file_name = NULL,
                    overwrite = FALSE) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1dJ4SpPlm31rajstXZoEhUe9Sy5PGyJXf")
  file_out <- tools::file_path_sans_ext(meta_dat$name)

  if (!is.null(dest_path)) {
    zip_file <- paste0(file.path(dest_path,
                                 file_out),
                       ".zip")
  } else {
    zip_file <- file_out
  }

  googledrive::drive_download(file = googledrive::as_id(meta_dat$id),
                              path = zip_file,
                              overwrite = overwrite)

  utils::unzip(file.path(paste0(zip_file)))
}
