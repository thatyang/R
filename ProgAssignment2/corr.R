corr<-function(directory, threshold=0){
	corrresults<-numeric(0)
	filenames<-list.files(directory,pattern='*.csv')
	filepath<-file.path(directory,filenames)
	for (path in filepath){
		if (sum(complete.cases(read.csv(path)))>=threshold){
			filetoread<-read.csv(path)
#			patchfile<-c(patchfile,read.csv(path))
			corrresults<-c(corrresults,cor(filetoread$sulfate,filetoread$nitrate,use='pairwise.complete.obs'))
		}

	}
	return(corrresults)

#	newresults<-do.call(rbind,as.list(patchfile))

#	x<-newresults[,2]
#	y<-newresults[,3]
#	corrresults<-cor(x,y,use='pairwise.complete.obs')

}
