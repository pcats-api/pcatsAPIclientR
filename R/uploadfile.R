#' @title Upload a file
#' @description Upload a file
#' @param filename Filename of a file to upload
#' @return Backend filename reference
#' @export
#' @import httr
#' @import utils
#'
uploadfile <- function(filename) {

  res <- POST(url=paste0('https://pcats.research.cchmc.org/api/uploadfile'),
              encode='multipart',
              body=list(data=upload_file(filename)
              ))
  cont <- content(res)
  jobid <- cont$jobid[[1]]
  jobid
}

