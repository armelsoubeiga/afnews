#' Function to check if URL exists
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
#' @return return TRUE or FALSE
#'
#' @import httr
#'
#' @examples
#' urls <- c('http://www.amazon.com', 'http://this.isafakelink.biz', 'https://stackoverflow.com')
#' base::sapply(urls,url_exists)
#' @export
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...){

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, ...)

    if (is.null(res$result)) return(FALSE) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}




#' function for merging r objects (chars, int, data.frame, ...) of different sizes
#'
#' @description function for merging r objects (chars, int, data.frame, ...) of different sizes
#' @param ... R objects
#' @param first ysorting na values
#' @return a data frame
#'
#' @examples
#' library(afnews)
#' cbind_na(a=1:2, b=letters[1:5], c=c('a',1,3))
#' @export
cbind_na <- function(..., first = TRUE) {

  ## check lsit of entry
  padNA <- function (mydata, rowsneeded, first = TRUE){
    temp1 = colnames(mydata)
    rowsneeded = rowsneeded - nrow(mydata)
    temp2 = setNames(
      data.frame(matrix(rep(NA, length(temp1) * rowsneeded),
                        ncol = length(temp1))), temp1)
    if (isTRUE(first)) rbind(mydata, temp2)
    else rbind(temp2, mydata)
  }

  ## create cols names
  dotnames <- function(...) {
    vnames <- names(as.list(substitute(list(...)))[-1L])
    vnames <- unlist(vnames, FALSE, FALSE)
    vnames
  }

  ##main of function
  Names <- dotnames(...)
  datalist <- setNames(list(...), Names)
  nrows <- max(sapply(datalist, function(x)
    ifelse(is.null(dim(x)), length(x), nrow(x))))
  datalist <- lapply(seq_along(datalist), function(x) {
    z <- datalist[[x]]
    if (is.null(dim(z))) {
      z <- setNames(data.frame(z), Names[x])
    } else {
      if (is.null(colnames(z))) {
        colnames(z) <- paste(Names[x], sequence(ncol(z)), sep = "_")
      } else {
        colnames(z) <- paste(Names[x], colnames(z), sep = "_")
      }
    }
    padNA(z, rowsneeded = nrows, first = first)
  })
  do.call(cbind, datalist)
}

