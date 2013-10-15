rankall<-function(outcome,num='best'){
    hpfile<-read.csv('outcome-of-care-measures.csv',colClasses='character')
    listofstate<-unique(hpfile[,7])
    resultset<-data.frame(matrix(NA, ncol = 2))
    if (outcome %in% c('heart attack','heart failure','pneumonia')){
        for (state in listofstate){
            if (num=='best'){num<-1}
            else if (num=='worst'){num<-(-1)}
            stateset<-subset(hpfile,State==state)
            if (outcome=='heart attack' && !stateset[,11] %in% 'Not Available'){
                statesetfull<-subset(stateset,!stateset[,11] %in% 'Not Available')
                haset<-as.numeric(statesetfull[,11])
                set_rank<-cbind(factor(haset),statesetfull[,2])
                set_rank_sorted<-set_rank[order(as.numeric(set_rank[,1]),set_rank[,2]),]
                if (!is.null(nrow(set_rank_sorted)) && length(set_rank_sorted)>0) {
                    if (nrow(set_rank_sorted)>=num){
                         if (num==(-1)) {num<-nrow(set_rank_sorted)}
                         set_rank_sorted_last<-cbind(1:nrow(set_rank_sorted),set_rank_sorted,state)
                         colnames(set_rank_sorted_last)<-c('rank','temprank','hospital','state')
                         result<-set_rank_sorted_last[set_rank_sorted_last[,'rank']==num,3:4]
                         resultset<-rbind(resultset,result)
                     }
                     else {
                        result<-c('NA',state)
                        resultset<-rbind(resultset,result)
                     }
                }
            }
            else if (outcome=='heart failure' && !stateset[,17] %in% 'Not Available'){
                statesetfull<-subset(stateset,!stateset[,17] %in% 'Not Available')
                haset<-as.numeric(statesetfull[,17])
                set_rank<-cbind(factor(haset),statesetfull[,2])
                set_rank_sorted<-set_rank[order(as.numeric(set_rank[,1]),set_rank[,2]),]
                if (!is.null(nrow(set_rank_sorted)) && length(set_rank_sorted)>0) {
                    if (nrow(set_rank_sorted)>=num){
                         if (num==(-1)) {num<-nrow(set_rank_sorted)}
                         set_rank_sorted_last<-cbind(1:nrow(set_rank_sorted),set_rank_sorted,state)
                         colnames(set_rank_sorted_last)<-c('rank','temprank','hospital','state')
                         result<-set_rank_sorted_last[set_rank_sorted_last[,'rank']==num,3:4]
                         resultset<-rbind(resultset,result)
                     }
                     else {
                        result<-c('NA',state)
                        resultset<-rbind(resultset,result)
                     }
                }
            }
            else if (outcome=='pneumonia' && !stateset[,23] %in% 'Not Available'){
                statesetfull<-subset(stateset,!stateset[,23] %in% 'Not Available')
                haset<-as.numeric(statesetfull[,23])
                set_rank<-cbind(factor(haset),statesetfull[,2])
                set_rank_sorted<-set_rank[order(as.numeric(set_rank[,1]),set_rank[,2]),]
                if (!is.null(nrow(set_rank_sorted)) && length(set_rank_sorted)>0) {
                    if (nrow(set_rank_sorted)>=num){
                         if (num==(-1)) {
                         worstnum<-nrow(set_rank_sorted)
                         set_rank_sorted_last<-cbind(1:nrow(set_rank_sorted),set_rank_sorted,state)
                         colnames(set_rank_sorted_last)<-c('rank','temprank','hospital','state')
                         result<-set_rank_sorted_last[set_rank_sorted_last[,'rank']==worstnum,3:4]
                         resultset<-rbind(resultset,result)}
                        else {
                         set_rank_sorted_last<-cbind(1:nrow(set_rank_sorted),set_rank_sorted,state)
                         colnames(set_rank_sorted_last)<-c('rank','temprank','hospital','state')
                         result<-set_rank_sorted_last[set_rank_sorted_last[,'rank']==worstnum,3:4]
                         resultset<-rbind(resultset,result)}
                       }
                     else {
                        result<-c('NA',state)
                        resultset<-rbind(resultset,result)
                     }
                     }
                }
            }
        }
    resultsetnew<-subset(resultset,complete.cases(resultset))
    colnames(resultsetnew)<-c('hospital','state')
    return(resultsetnew)
}
