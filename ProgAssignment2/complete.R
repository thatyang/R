complete<-function(directory, id){
	count<-0
	finalresults<-matrix(nrow=0,ncol=2)
	for (i in id){
			modfilename<-''
			charid<-as.character(i)
		if (i<=9){
			modfilename<-paste('00',charid,'.csv',sep='')
		}
		else if (i>=10 && i<100){
			modfilename<-paste('0',charid,'.csv',sep='')
		}
		else if (i>=100){
			modfilename<-paste(charid,'.csv',sep='')
		}
#	filenames<-list.files(directory,pattern='*.csv')
	
	filepath<-file.path(directory,modfilename)
	patchfile<-read.csv(filepath)
	naresults<-complete.cases(patchfile)
	results<-sum(naresults)
	newresults<-cbind(i,results)
	count<-count+1
	finalresults<-rbind(finalresults,newresults)
	}

	colnames(finalresults)<-c('id','nobs')

	return(data.frame(finalresults))
}
