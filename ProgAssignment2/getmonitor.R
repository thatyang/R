getmonitor<-function(id,directory,summarize=FALSE){
	charid<-as.character(id)
	if (nchar(charid)==1){
		newid<-paste('00',charid,'.csv',sep='')
	} else if (nchar(charid)==2){
		newid<-paste('0',charid,'.csv',sep='')
	} else if (nchar(charid)==3){
		newid<-paste(charid,'.csv',sep='')
	} else {
		'no file found!'
	}

	newfilepath<-file.path(directory,newid)

	filedata<-read.csv(newfilepath)

	return(filedata)

	if(summarize==TRUE){
		summary(filedata)
	}
}