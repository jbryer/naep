#' Read NAEP catalog
#' 
#' Read the NAEP catalog and return a data frame of the available variables
#' in the NAEP database.
#'
#' @param year the year of the NAEP data.
#' @param grade the grade to read.
#' @param subject the subject to read (e.g. Math, Read)
#' @param directory the root directory of the NAEP data disc.
#' @param sample either AT or PM.
#' @param type the type of data to read (i.e. Student or School)
#' @title Return the NAEP catalog as a data frame
#' @export
getNAEPCatalog <- function(year, grade, subject, 
						   directory=getBaseDirectory(), 
						   sample=c('AT', 'PM'), 
						   type=c('Student','School')) {
	if(is.null(directory)) {
		directory = getBaseDirectory()
	}
	if(tolower(type[1]) == 'student') { 
		type = 'T' 
	} else if(tolower(type[1]) == 'school') { 
		type = 'C' 
	}
	
	file = paste(directory, '/Select/Parms/', toupper(substr(subject, 1,1)), 
				 (year-1969), 'N', type, ifelse(grade==4, 1, 2), sample, '.fr2', sep='')
	fr2Widths = c(8,4,2,1,1,-4,50,-4,-4,-4,-2,-2,-2,2,2000)
	catalog = read.fwf(file, widths=fr2Widths, fill=TRUE, strip.white=TRUE)
	names(catalog) = c('FieldName', 'Start', 'Width', 'Decimal', 'PreselectFlag', 
					   'Description', 'NumCodeValues', 'CodeValues')
	catalog[,'FieldName'] = as.character(catalog[,'FieldName'])
	catalog[,'CodeValues'] = as.character(catalog[,'CodeValues'])
	return(catalog)
}

