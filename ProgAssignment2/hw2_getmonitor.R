getmonitor<-function(id,directory,summarize=FALSE){
	m<-read.csv('directory/id')
	if (summarize==TRUE){
		summary(m)
	}
}