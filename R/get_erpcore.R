#' Download ERP Core Components
#'
#' @param component Options are:
#' * "n170": download data for the face-specific N170 component.
#' * "mmn": download data for the Mismatch Negativity component.
#' * "n2pc": download data for the N2pc spatial attention component.
#' * "n400": download data for the N400 semantic mismatch component.
#' @param dest_path destination file path
#' @param conflicts This determines what happens when a file with the same name exists at the specified destination. Can be one of the following:
#' * "error" (the default): throw an error and abort the file transfer operation.
#' * "skip": skip the conflicting file(s) and continue transferring the remaining files.
#' * "overwrite": replace the existing file with the transferred copy.
#' @param type The data is in three different formats:
#' * "raw": download the raw data and scripts.
#' * "bids": download the raw data in scripts in a BIDS compatible format.
#' * "all": download the raw and processed data and scripts.
#' @export
get_erpcore <- function(component = c("n170",
                                      "mmn",
                                      "n2pc",
                                      "n400"),
                        dest_path = NULL,
                        conflicts = "error",
                        type = c("raw",
                                 "bids",
                                 "all")) {
  component <- match.arg(component,
                         c("n170",
                           "mmn",
                           "n2pc",
                           "n400"))
  out_dir <- choose.dir()
  switch(component,
         n170 = get_n170(dest_path = out_dir),
         mmn = get_mmn(dest_path = out_dir),
         n2pc = get_n2pc(dest_path = out_dir),
         n400 = get_n400(dest_path = out_dir))
}

#' @param dest_path destination file path
#' @param conflicts This determines what happens when a file with the same name exists at the specified destination. Can be one of the following:
#' * "error" (the default): throw an error and abort the file transfer operation.
#' * "skip": skip the conflicting file(s) and continue transferring the remaining files.
#' * "overwrite": replace the existing file with the transferred copy.
#' @param type The data is in three different formats:
#' * "raw": download the raw data and scripts.
#' * "bids": download the raw data in scripts in a BIDS compatible format.
#' * "all": download the raw and processed data and scripts.
#' @describeIn get_erpcore Retrieve N170 data
#' @export
get_n170 <- function(dest_path = NULL,
                     conflicts = "error",
                     type = c("raw",
                              "bids",
                              "all")) {

  osf_meta <- osfr::osf_retrieve_node("https://osf.io/pfde9/")
  download_data(osf_meta,
                type = type,
                conflicts = conflicts,
                dest_path = dest_path)
}

#' @describeIn get_erpcore Retrieve MMN data
#' @export
get_mmn <- function(dest_path = NULL,
                    conflicts = "error",
                    type = c("raw",
                             "bids",
                             "all")) {

  osf_meta <- osfr::osf_retrieve_node("https://osf.io/5q4xs/")
  download_data(osf_meta,
                type = type,
                conflicts = conflicts,
                dest_path = dest_path)
}

#' @describeIn get_erpcore Retrieve N2pc data
#' @export
get_n2pc <- function(dest_path = NULL,
                     conflicts = "error",
                     type = c("raw",
                              "bids",
                              "all")) {

  osf_meta <- osfr::osf_retrieve_node("https://osf.io/thsqg/")
  download_data(osf_meta,
                type = type,
                conflicts = conflicts,
                dest_path = dest_path)
}

#' @describeIn get_erpcore Retrieve N400 data
#' @export
get_n400 <- function(dest_path = NULL,
                     conflicts = "error",
                     type = c("raw",
                              "bids",
                              "all")) {

  osf_meta <- osfr::osf_retrieve_node("https://osf.io/29xpq/")
  download_data(osf_meta,
                type = type,
                conflicts = conflicts,
                dest_path = dest_path)
}

#' Download the data from OSF
#'
#' @keywords internal
download_data <- function(osf_meta,
                          type,
                          conflicts,
                          dest_path) {
  osf_files <- osfr::osf_ls_files(osf_meta)

  type <- match.arg(type,
                    c("raw",
                      "bids",
                      "all"))
  type <- switch(type,
                 raw = osf_files[2, ],
                 bids = osf_files[3, ],
                 all = osf_files[1, ])
  osfr::osf_download(
    type,
    path = dest_path,
    conflicts = conflicts,
    recurse = TRUE,
    verbose = TRUE,
    progress = TRUE
  )
}
