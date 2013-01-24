#' This function provides a dialog box for selecting variables to read from the
#' NAEP database. Returns a vector of variable names.
#'
#' @title Variable selection dialog box.
#' @param year the year of the NAEP data.
#' @param grade the grade to read.
#' @param subject the subject to read (e.g. Math, Read)
#' @param directory the root directory of the NAEP data disc.
#' @param sample either AT or PM.
#' @param type the type of data to read (i.e. Student or School)
#' @return a character vector with selected variables.
#' @export
selectVariables <- function(year=NULL, grade=NULL, subject=NULL, 
							directory=getBaseDirectory(), 
							sample=c('AT', 'PM'), 
							type=c('Student','School')) {
	print(paste("Selecting variables for grade", grade, subject))
 	
	if(is.null(directory)) {
		directory = getBaseDirectory()
	}
	
	catalog = getNAEPCatalog(year=year, grade, subject, directory, sample=sample, type=type[1])
	
	tt <- tktoplevel()
	#TODO: Change this label to the appropriate grade, subject, and year
	tkpack(tklabel(tt,text="National Assessment of Educational Progress (NAEP) Variable Selection"))
	sw <- tkwidget(tt,"ScrolledWindow",relief="sunken",borderwidth=2)
	sf <- tkwidget(sw,"ScrollableFrame")
	tcl(sw,"setwidget",sf)
	subfID <- tclvalue(tcl(sf,"getframe"))[]

	entryList <- list()
	valueList <- list()
	for(i in 1:nrow(catalog)) {
		cb <- tcl("checkbutton",paste(subfID,".",i,sep=""),
				  text=paste(catalog[i,'FieldName'],catalog[i,'Description']))
		if(catalog[i,'PreselectFlag'] == 1) { tktoggle(cb) }
		entryList[[i]] <- cb
		valueList[[i]] <- tclVar(catalog[i,"PreselectFlag"])
		tkconfigure(entryList[[i]], variable=valueList[[i]])
  		label = tcl("label", paste(subfID,".",i,'label',sep=""), 
  					text=paste(catalog[i,'Description']))
  		tkpack(entryList[[i]], anchor="w")
  		tkbind(entryList[[i]],"<FocusIn>",function() tcl(sf,"see",entryList[[i]]))
	}
	tkpack(sw,fill="both",expand="yes")
    ReturnValue <- c()
	OnOK <- function() {
		for(i in 1:length(valueList)) {
			val = tclvalue(valueList[[i]])
			if(val == "1") {
				ReturnValue <<- c(ReturnValue, catalog[i,'FieldName'])
			}
		}
	    tkdestroy(tt)
	}
	OK.button <- tkbutton(tt,text="OK",command=OnOK)
	tkpack(OK.button)
	tkfocus(tt)
	
	tkbind(tt, "<Destroy>", function() { tkgrab.release(tt) })
    tkwait.window(tt)

	return(ReturnValue)
}

