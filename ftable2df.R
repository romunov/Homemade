#' How to convert an (f)table to a data.frame.
#' 
#' These two functions perform print.* for ftable and table class. They can be used in the same
#' way. The side effect is that the returned object is now a list of two, where the first element
#' (\code{table}) is as expected output of (f)table and the second element (\code{data.frame}) is
#' an actual \code{data.frame} that can be passed to write functions, e.g. write.xlsx.
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

table2df <- function (x, digits = getOption("digits"), quote = FALSE, na.print = "", 
		zero.print = "0", justify = "none", ...) {
	
	xx <- format(unclass(x), digits = digits, justify = justify)
	if (any(ina <- is.na(x))) 
		xx[ina] <- na.print
	if (zero.print != "0" && any(i0 <- !ina & x == 0) && all(x == 
					round(x))) 
		xx[i0] <- sub("0", zero.print, xx[i0])
	
	if (is.numeric(x) || is.complex(x)) {
		print(xx, quote = quote, right = TRUE, ...)
	}	else {
		print(xx, quote = quote, ...)
	} 
	
	## mold xx to a data.frame
	
	# prepare column and row names
	var.names <- names(attributes(x)$dimnames)
	rowname <- var.names[1]
	colname <- var.names[2]
	
	## We strip x to a matrix, add row names and then construct two labels,
	## one will add column label and the other will add row label and column names
	raw.x <- matrix(as.numeric(x), nrow = nrow(x)) 
	first.row <- matrix(c("", colname, rep("", length(colnames(x))-1)), nrow = 1)
	second.row <- matrix(c(rowname, colnames(x)), nrow = 1)
	first.column <- matrix(rownames(x), ncol = 1)
	xx <- rbind(first.row, second.row, cbind(first.column, raw.x))
	
	x <- list(table = x, data.frame = as.data.frame(xx))
	invisible(x)
}

#' @examples 
#' \dontrun
#' my.ftable <- ftable(Titanic, row.vars = 1:3)
#' my.ftable <- ftable2df(my.ftable) # you can access the original table with object$
#' 
#' require(xlsx)
#' write.xlsx(x = my.ftable$data.frame, file = "write.df.xlsx", sheetName = "TestfTable",
#'		row.names = FALSE, col.names = FALSE)
#' 
#' #' require(xlsx)
#' my.table <- with(warpbreaks, table(wool, tension))
#' my.table <- table2df(my.table)
#' 
#' write.xlsx(x = my.table$data.frame, file = "write.df.xlsx", sheetName = "TestTable",
#'		row.names = FALSE, col.names = FALSE)