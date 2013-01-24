#https://github.com/hadley/devtools
install.packages(c('devtools', 'roxygen2'), repos="http://cran.r-project.org")

require(devtools)
require(roxygen2)

if(Sys.info()['sysname'] == 'Windows') {
	setwd("c:/Dropbox/My Dropbox/Projects/NAEP")
} else if(Sys.info()['sysname'] == 'Darwin') {
	setwd("~/Dropbox/Projects/NAEP")
}

document("naep")
install("naep")
library(naep)

build("naep", binary=FALSE)
build("naep", binary=TRUE)
check_doc("naep")
check("naep")

ls('package:naep')

