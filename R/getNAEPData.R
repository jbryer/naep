#' Read the NAEP database and return a data object.
#'
#' This function will read data from the NAEP data discs and return a list containing
#' the following:
#' 
#' \itemize{
#'     \item{data}{ a data frame containing the read data.}
#'     \item{catalog}{ a data frame containing metadata.}
#'     \item{design}{ the design of the complex survey from \code{\link{svydesign}}.}
#'     \item{grade}{ the grade level.}
#'     \item{subject}{ the subject.}
#'     \item{year}{ the year.}
#'     \item{repweights}{ a character vector containing the names of the replicate weights.}
#'     \item{sample}{ the sample (either AT or PM)}
#'     \item{sourceDir}{ the directory where the data was read from.}
#'     \item{type}{ the type of data (either student or school).}
#' }
#' 
#' @title Read NAEP data.
#' @param year the year of the NAEP data.
#' @param grade the grade to read.
#' @param subject the subject to read (e.g. Math, Read)
#' @param directory the root directory of the NAEP data disc.
#' @param vars the variables to return. If NULL, the default set will be returned.
#' @param sample either AT or PM.
#' @param type the type of data to read (i.e. Student or School)
#' @export
getNAEPData <- function(year, grade, subject, directory=NULL, vars=NULL, 
		sample=c('AT', 'PM'), type=c('Student','School')
) {
	if(is.null(directory)) {
		directory = getBaseDirectory()
	}
	
 	catalog = getNAEPCatalog(year=year, grade=grade, subject=subject, 
 							 directory=directory, sample=sample, type=type[1])

	if(tolower(type[1]) == 'student') { type = 'T' }
	else if(tolower(type[1]) == 'school') { type = 'C' }
	
 	if(is.null(vars)) {
  		message("vars not specified, using default set of variables")
 		vars = catalog[which(catalog$PreselectFlag == 1),'FieldName']
 	}
	
	#If any of the replicate weights are not listed in vars, add them
	repweights <- c(paste("SRWT0", 1:9, sep=''), paste("SRWT", 10:62, sep=''))
	vars <- c(vars, repweights[!repweights %in% vars])
	vars <- c(vars, ifelse('SRWT' %in% vars, character(), 'SRWT'))
	
	#Get the widths and set the variables we don't wish to return to negative
	w = catalog$Width
	w[-which(catalog$FieldName %in% vars)] = -1 * w[-which(catalog$FieldName %in% vars)]
	
	file = paste(directory, '/Data/', toupper(substr(subject, 1,1)), (year-1969), 
				 'N', type[1], ifelse(grade==4, 1, 2), sample, '.dat', sep='')
	thedata = read.fwf(file, widths=w)
	
	catalog2 = catalog[which(catalog$FieldName %in% vars),]
	catalog2 = catalog2[order(catalog2$Start),]
	
	if(ncol(thedata) != nrow(catalog2)) {
		#This should not happen but we will check to make sure.
		stop('Number of columns returned does not match the catalog.')
	}
	
	names(thedata) = catalog2$FieldName
	
	dec = catalog2[catalog2[,'Decimal'] > 0, c('FieldName','Decimal')]
	if(nrow(dec) > 0) {
		for(i in 1:nrow(dec)) {
			thedata[,dec[i,'FieldName']] = thedata[,dec[i,'FieldName']] / 10 ^ dec[i,'Decimal']
		}
	}
	
	codes = catalog2[!is.na(catalog2[,'NumCodeValues']) & catalog2[,'NumCodeValues'] > 0, 
					 c('FieldName', 'NumCodeValues', 'CodeValues')]
	trim <- function(x) { return(sub(" +$", "", x)) }
	if(nrow(codes) > 0) {
		for(i in 1:nrow(codes)) {
			cv = codes[i,'CodeValues']
			cv = paste(' ', cv, sep='')
			levels = as.numeric()
			labels = as.character()
			pos = 1
			while(pos < nchar(cv)) {
				levels[length(levels) + 1] = as.integer(substr(cv, pos, pos + 1))
				labels[length(labels) + 1] = trim(substr(cv, pos + 2, pos+20))
				pos = pos + 28
			}
			thedata[,codes[i,'FieldName']] = factor(thedata[,codes[i,'FieldName']], 
													levels=levels, labels=labels)
		}
	}
	
	results <- list()
	results$data <- thedata
	results$catalog <- catalog2
	results$repweights <- repweights
	results$design <- svrepdesign(data=thedata, 
							 weights=thedata[,"ORIGWT"], 
							 repweights=thedata[,repweights], 
							 rscales=1, scale=1,
							 combined.weights=TRUE,
							 type="JKn")
	results$year <- year
	results$grade <- grade
	results$subject <- subject
	results$sourceDir <- directory
	results$sample <- sample
	results$type <- type[1]
	class(results) <- 'naep'
	return(results)
}

