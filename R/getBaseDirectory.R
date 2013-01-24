#' Select the root directory of NAEP data disc.
#' 
#' Provides a directory selection box to select the root of the NAEP data DVD.
#' This function also provides some basic verification that the directory selected
#' is a proper NAEP dataset.
#'
#' @title Select the base directory for the NAEP data
#' @return string with the path to the NAEP data
#' @export
getBaseDirectory <- function() {
	dir = getwd()
	if(Sys.info()['sysname'] == 'Windows') {
		dir = choose.dir()
	} else {
		dir = tclvalue(tkchooseDirectory())
 	}
	#listing = list.files(dir, full.names=FALSE, include.dirs=TRUE)
	if(!file.exists(paste(dir, '/Select/Parms', sep=''))) {
		stop("Invalid directory selected.")
	}
	return(dir)
}

