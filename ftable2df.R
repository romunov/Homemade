#' How to convert an ftable to a data.frame.
#' 
#' @author Roman Luštrik
#' @return A list of two.
#' \item{ftable}{The original ftable result that was passed onto the \code{ftable2df} function.}
#' \item{data.frame}{A raw version of the data.frame that is used to print the table.}
#' @keywords print multivariate
#' @param x \code{ftable} object.
#' @param ... See \code{\link{ftable}}.

ftable2df <- function(x, ...) {
## I am changing some stats functions
## (print.ftable, write.ftable)
## so that I can catch the modified output that
## can be molded into a data.frame.
	print.ftable2 <- function (x, digits = getOption("digits"), ...) {
		write.ftable2(x, quote = FALSE, digits = digits)
	}
	
	write.ftable2 <- function (x, file = "", quote = TRUE, append = FALSE,
			digits = getOption("digits")) {
		r <- stats:::format.ftable(x, quote = quote, digits = digits)
		cat(t(r), file = file, append = append, sep = c(rep(" ", 
								ncol(r) - 1), "\n"))
		x <- list(ftable = x, data.frame = as.data.frame(r)) # this is the only new added line - we want the raw output, too
		invisible(x)
	}
	
	print.ftable2(x, ...)
}

#' @examples 
#' \dontrun
#' my.table <- ftable(Titanic, row.vars = 1:3)
#' my.table <- ftable2df(my.table) # you can access the original table with object$
#' 
#' require(xlsx)
#' write.xlsx(x = my.table$data.frame, file = "write.df.xlsx", sheetName = "TestTable",
#'		row.names = FALSE, col.names = FALSE)