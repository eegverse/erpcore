#' Download ERP CORE Components
#'
#' Download individual components of the ERP CORE dataset.
#'
#' @param component Options are:
#' * "n170": download data for the face-specific
#'   N170 component.
#' * "mmn": download data for the Mismatch Negativity
#'   component.
#' * "n2pc": download data for the N2pc spatial attention
#'   component.
#' * "n400": download data for the N400 semantic mismatch
#'   component.
#' * "p3": download the P3 data.
#' * "lrp": download the Lateralized Readiness Potential data.
#' * "ern": download the Error-Related Negativity dataset.
#' @param dest_path destination file path
#' @param conflicts This determines what happens when a file with the same name
#'   exists at the specified destination. Can be one of the following: * "error"
#'   (the default): throw an error and abort the file transfer operation. *
#'   "skip": skip the conflicting file(s) and continue transferring the
#'   remaining files. * "overwrite": replace the existing file with the
#'   newly transferred copy.
#' @param type The data is in three different formats: * "raw": download the raw
#'   data and scripts. * "bids": download the raw data and scripts in a BIDS
#'   compatible format. * "all": download the raw and processed data and
#'   scripts.
#' @param subjects Optionally subsets one more of the 40 subjects in ERP CORE.
#'   Subject IDs depend on the "type" argument: * c("1", "2", "3", ...) if
#'   "type" is "raw" or "all". * c("sub-001", "sub-002", "sub-003", ...) if
#'   "type" is "bids".
#' @references Kappenman, E.S., Farrens, J.L., Zhang, W., Stewart, A.X., & Luck,
#'   S.J. (2020). ERP CORE: An Open Resource for Human Event-Related Potential
#'   Research. NeuroImage. https://doi.org/10.1016/j.neuroimage.2020.117465
#' @author Matt Craddock \email{matt@@mattcraddock.com}
#' @export
get_erpcore <- function(component = c("n170",
                                      "mmn",
                                      "n2pc",
                                      "n400",
                                      "p3",
                                      "lrp",
                                      "ern"),
                        dest_path = NULL,
                        conflicts = "error",
                        type = c("raw",
                                 "bids",
                                 "all"),
                        subjects = NULL) {
  component <- match.arg(component)

  if (is.null(dest_path)) {
    out_dir <- choose.dir()
  } else {
    out_dir <- dest_path
  }

  get_comp <-
    switch(component,
           n170 = get_n170, #(dest_path = out_dir),
           mmn = get_mmn, #(dest_path = out_dir),
           n2pc = get_n2pc, #(dest_path = out_dir),
           n400 = get_n400, #(dest_path = out_dir),
           p3 = get_p3, #(dest_path = out_dir),
           lrp = get_lrp, #(dest_path = out_dir)),
           ern = get_ern) #(dest_path = out_dir))

  get_comp(dest_path = out_dir,
           conflicts = conflicts,
           type = type,
           subjects = subjects)
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
                              "all"),
                     subjects = NULL) {
  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/pfde9/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "n170")
}

#' @describeIn get_erpcore Retrieve MMN data
#' @export
get_mmn <- function(dest_path = NULL,
                    conflicts = "error",
                    type = c("raw",
                             "bids",
                             "all"),
                    subjects = NULL) {

  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/5q4xs/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "mmn")
}

#' @describeIn get_erpcore Retrieve N2pc data
#' @export
get_n2pc <- function(dest_path = NULL,
                     conflicts = "error",
                     type = c("raw",
                              "bids",
                              "all"),
                     subjects = NULL) {
  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/thsqg/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "n2pc")
}

#' @describeIn get_erpcore Retrieve N400 data
#' @export
get_n400 <- function(dest_path = NULL,
                     conflicts = "error",
                     type = c("raw",
                              "bids",
                              "all"),
                     subjects = NULL) {

  osf_meta <- osfr::osf_retrieve_node("https://osf.io/29xpq/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "n400")
}


#' @describeIn get_erpcore Retrieve P3 data
#' @export
get_p3 <- function(dest_path = NULL,
                   conflicts = "error",
                   type = c("raw",
                            "bids",
                            "all"),
                   subjects = NULL) {
  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/etdkz/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "p3")
}

#' @describeIn get_erpcore Retrieve LRP data
#' @export
get_lrp <- function(dest_path = NULL,
                    conflicts = "error",
                    type = c("raw",
                             "bids",
                             "all"),
                    subjects = NULL) {
  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/28e6c/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "lrp")
}

#' @describeIn get_erpcore Retrieve LRP data
#' @export
get_ern <- function(dest_path = NULL,
                    conflicts = "error",
                    type = c("raw",
                             "bids",
                             "all"),
                    subjects = NULL) {
  type <- match.arg(type)
  osf_meta <- osfr::osf_retrieve_node("https://osf.io/q6gwp/")
  download_data(osf_meta,
                type = type,
                subjects = subjects,
                conflicts = conflicts,
                dest_path = dest_path,
                component = "ern")
}


#' Download the data from OSF
#'
#' @keywords internal
download_data <- function(osf_meta,
                          type,
                          conflicts,
                          dest_path,
                          component,
                          subjects) {

  files <- find_files(osf_meta, type, subjects)

  osfr::osf_download(
    files,
    path = dest_path,
    conflicts = conflicts,
    recurse = TRUE,
    verbose = TRUE,
    progress = TRUE
  )
}

#' Find relevant files on OSF
#'
#' @keywords internal
find_files <- function(osf_meta, type, subjects) {

  osf_files <- osfr::osf_ls_files(osf_meta)

  type <- match.arg(type,
                    c("raw",
                      "bids",
                      "all"))

  folder <- switch(type,
                   raw = osf_files[2, ],
                   bids = osf_files[3, ],
                   all = osf_files[1, ])

  files <- osfr::osf_ls_files(folder, n_max = Inf)

  if (!is.null(subjects)) {

  all_subjects <- switch(type,
                         raw = as.character(1:40),
                         bids = sprintf("sub-%03d", 1:40),
                         all = as.character(1:40))

  invalid_subjects <- setdiff(subjects, all_subjects)
  if (length(invalid_subjects) != 0) {
    stop("Invalid subject(s): ",
         paste(shQuote(invalid_subjects), collapse = ", ")
    )
  }

  ignore_subjects <- setdiff(all_subjects, subjects)
    files <- files[!files$name %in% ignore_subjects, ]
  }

  return(files)
}

