
get_N170 <- function(dest_path = NULL,
                     file_name = NULL) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1CIw8tlnRUNURYdEBhTEvhtBycyKfGuvV")
  file_out <- meta_dat$name

  if (!is.null(dest_path)) {
    zip_file <- paste0(dest_path, file_out)
  }
  googledrive::drive_download(googledrive::as_id(meta_dat$id),
                              path = dest_path)

  unzip(file.path(paste0(dest_path,
                         file_out)))
}

get_mmn <- function(dest_path = NULL,
                    file_name = NULL,
                    overwrite = FALSE) {

  meta_dat <- googledrive::drive_get("https://drive.google.com/open?id=1_QGoOUmPO4xPre_Q2ujSgatRS7kf5O5u")
  file_out <- tools::file_path_sans_ext(meta_dat$name)

  if (!is.null(dest_path)) {
    zip_file <- paste0(file.path(dest_path, file_out), ".zip")
  } else {
    zip_file <- file_out
  }

  googledrive::drive_download(file = googledrive::as_id("18ZeEYC7mfUclgQjcAbHhaNuI-v4acAmi"),
                              path = zip_file,
                              overwrite = overwrite)

  #unzip(file.path(paste0(zip_file)))
}
