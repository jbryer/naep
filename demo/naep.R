require(naep)

#Test the functions
baseDir = getBaseDirectory()
catalog = getNAEPCatalog(year=2005, grade=8, subject="Math", sample='PM', directory=baseDir)
vars = selectVariables(year=2005, grade=8, subject="Math", sample='PM', directory=baseDir)
g8math = getNAEPData(year=2005, grade=8, subject="Math", sample='PM', 
					 directory=baseDir, vars=vars)

ls(g8math)
View(g8math$catalog[,c('FieldName','Description')])
summary(g8math$design)

#This is a list of available functions from the survey package
ls('package:survey')[grep('^svy*', ls('package:survey'))]

m1 <- naep.model(~ IEP, naepData=g8math, svyFUN=svytable, na.action=na.pass)
ls(m1)
round(m1$combined)

m2 <- naep.model(~ IEP + DSEX, naepData=g8math, svyFUN=svytable, na.action=na.pass)
round(m2$combined)
round(prop.table(m2$combined) * 100, digits=1)

m3 <- naep.model(~ MRPCM, var='MRPCM', naepData=g8math, svyFUN=svymean, na.rm=TRUE)
ls(m3)
print(m3)
summary(m3)

#To show that combining the multiple plausible values is not the same as the mean of means
avgScores = apply(g8math$data[,paste('MRPCM', 1:5, sep='')], 1, mean, na.rm=TRUE)
mean(avgScores, na.rm=TRUE)
rbind(as.data.frame(m3$models[[1]]),
	  as.data.frame(m3$models[[2]]), 
	  as.data.frame(m3$models[[3]]),
	  as.data.frame(m3$models[[4]]),
	  as.data.frame(m3$models[[5]]))

m4 <- naep.model(MRPCM ~ DSEX + IEP + ELL3 + SDRACEM + PARED, 
				 var='MRPCM', naepData=g8math, svyFUN=svyglm)
ls(m4)
m4$svyFUN
m4$combined
betas <- MIextract(m4$models, fun=coef)


