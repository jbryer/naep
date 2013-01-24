.onLoad <- function(libname, pkgname) {
	require(tcltk)
	dir = system.file(package="naep")
 	addTclPath(path=dir)
	bwidgetloaded = tclRequire("BWidget")
	if(class(bwidgetloaded) != 'tclObj') {
		warning(paste("BWidget did not load. Make sure it is contained in the ",
					  "package directory. The variable selection dialog box will not work.",
					  sep=''))
	} else {
		message("BWidget loaded successfully.")
	}
}

#' National Assessment of Educational Progress (NAEP)
#' 
#' This package provides functions to read data from NAEP restricted data CDs or
#' DVDs. This obviates the need for the NAEP Data Explorer (which is Windows only).
#' The resulting data frame will have correctly coded varaibles. Additional
#' functions provide access to the data dictionary as a data frame. Variable
#' selection can be done through a dialog box or by character lists.
#' 
#' @name naep-package
#' @aliases naep
#' @docType package
#' @title National Assessment of Educational Progress (NAEP)
#' @author \email{jason@@bryer.org}
#' @keywords package naep nces data
NULL


