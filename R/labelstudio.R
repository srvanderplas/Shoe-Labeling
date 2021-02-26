#' Export results from labelstudio API
#' 
#' @param filename Export filename + format
#' @param address Address of labelstudio server (api endpoint will be added to this URL; should end in /)
#' @param ... additional arguments to copy or download.file
#' @return TRUE if completed successfully, FALSE otherwise
#' @export
#' @importFrom curl curl_download
#' @examples 
#' api_export(filename = "results.json", quiet = F, overwrite = T)
api_export <- function(filename = "results.json", address = "https://srvanderplas.com/shoelabels/", ...) {
  addr <- paste0(address, "api/export?format=JSON")
  tmp <- tempfile(fileext = ".zip")
  tmpdir <- tempdir()
  # Debugging
  # cat(addr)
  # cat(tmpdir)
  # cat("\n")
  # cat(filename)
  
  # Download the file (zipped json)
  file_failed <- download.file(addr, destfile = tmp)
  # dlparms <- match.arg(..., download.file)
  # file_failed <- do.call(c(dlparms, url = addr, destfile = tmp), download.file)
  if (file_failed) {
    warning("Download failed")
    return(FALSE)
  }
  # Unzip
  unzip(tmp, exdir = tmpdir)
  # Copy to requested file
  copy <- file.copy(file.path(tmpdir, "result.json"), to = filename)
  # cpparms <- match.arg(..., file.copy)
  # copy <- do.call(c(cpparms, from = file.path(tmpdir, "result.json"), to = filename), file.copy)
  # If successful, clean up
  if (copy) {
    unlink(tmpdir)
    unlink(tmp)
  }
  return(copy)
}