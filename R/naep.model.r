#' Executes statistical analysis using the replicate weights and multiple
#' plausible values.
#' 
#' This function wraps functions from the \code{survey} package for the analysis
#' of complex survey designs. Specifically, this function will perform statistical
#' analyses utilizing the provided replicate weights and multiple plausible
#' values (if specified in the model). The function will return a list of class
#' \code{naep.model} with the following elements:
#' \itemize{
#'     \item{combined}{ the combined results containing estimates and standard errors.}
#'     \item{formula}{ the model formula.}
#'     \item{models}{ a list of the individual models before being combined. This
#'           has a length of five and contains the results of each \code{svyFUN}
#'           call for each of the five plausible values. Note that this is only
#'           present for models containing plausible values.}
#'     \item{svyFUN}{ the name of the \code{survey} function called.}
#'     \item{variable}{ the name of the plausible value.}
#' }
#' 
#' @param formula model formula
#' @param var the variable prefix for an outcome with multiple plausible values
#'        (e.g. MRPCM for overall composite math value for math assessment)
#' @param naepData a NAEP data object.
#' @param svyFUN the function to use from the survey package.
#' @param ... other parameters passed to the survey function
#' @return an object of class \code{naep.model} with \code{print} and \code{summary}
#'         methods defined.
#' @export
naep.model <- function(formula, var=NULL, naepData, svyFUN=svyglm, ...) {
	vars <- all.vars(formula)
	vars <- vars[vars != '.']
	result <- list()
	result$formula <- formula
	result$variable <- var
	result$svyFUN <- match.call()['svyFUN']
	if(!is.null(var)) {
		vars <- vars[vars != var]
		d1 <- naepData$data[,c(paste(var,'1', sep=''), vars)]
		d2 <- naepData$data[,c(paste(var,'2', sep=''), vars)]
		d3 <- naepData$data[,c(paste(var,'3', sep=''), vars)]
		d4 <- naepData$data[,c(paste(var,'4', sep=''), vars)]
		d5 <- naepData$data[,c(paste(var,'5', sep=''), vars)]
		implist <- imputationList(list(d1, d2, d3, d4, d5))
		result$models = list()
		for(i in seq_len(length(implist$imputations))) {
			fs <- as.character(formula)
			fs <- sub(var, paste(var, i, sep=''), fs)
			f <- as.formula(paste(fs[2], fs[1], fs[3]))
			if(length(fs) < 3) {
				f <- as.formula(paste(fs[1], fs[2]))
			}
			result$models[[i]] <- svyFUN(f, naepData$design, ...)
		}
		result$combined <- MIcombine(result$models)
		varpos = paste(var,'1', sep='') %in% names(result$combined$coefficients)
		names(result$combined$coefficients)[varpos] = var
	} else {
		result$combined <- svyFUN(formula, naepData$design, ...)
	}
	class(result) <- 'naep.model'
	return(result)
}

#' Prints summary of the combined NAEP model.
#' 
#' @param x a naep.model object.
#' @param ... other parameters passed to \code{print}.
#' @export
print.naep.model <- function(x, ...) {
	invisible(print(x$combined, ...))
}

#' Prints summary of the combined NAEP model.
#' 
#' @param object a naep.model object.
#' @param ... other parameters passed to \code{summary}.
#' @export
summary.naep.model <- function(object, ...) {
	x <- object$formula
	attr(x, ".Environment") <- NULL
	cat(paste('NAEP results for with ', object$svyFUN, ' function call.\n', sep=''))
	print.default(unclass(x))
	invisible(summary(object$combined, ...))
}

