rankhospital<-function(state,outcome,num='best'){
    hpfile<-read.csv('outcome-of-care-measures.csv',colClasses='character')
    listofstate<-unique(hpfile[,7])
#   print(listofstate)
#   print(state)
    if (state %in% listofstate && outcome %in% c('heart attack','heart failure','pneumonia')
 ){
    stateset<-subset(hpfile,State==state)
#   print('valid state and outcome')
       if (outcome=='heart attack'){
            outcomeha<-stateset[,11]
            outcomehaclean<-subset(outcomeha,!outcomeha %in% 'Not Available')
            statesetfull<-subset(stateset,!stateset[,11] %in% 'Not Available')
            if (num=='best'){
            haname<-subset(statesetfull[,2],as.numeric(statesetfull$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)==min(as.numeric(outcomehaclean)))
            return(sort(haname)[1])
            }
            else if (num=='worst'){
            haname<-subset(statesetfull[,2],as.numeric(statesetfull$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)==max(as.numeric(outcomehaclean)))
            return(sort(haname)[1])
            }
            else {
            set_rank<-cbind(statesetfull[,2],factor(statesetfull[,11]))
            colnames(set_rank)<-c('hospital','rank')
            haname<-set_rank[set_rank[,'rank']==num,1]
            return(sort(haname)[1])
            }
        }
        else if (outcome=='heart failure'){
            outcomehf<-stateset[,17]
            outcomehfclean<-subset(outcomehf,!outcomehf %in% 'Not Available')
            statesetfullhf<-subset(stateset,!stateset[,17] %in% 'Not Available')
            if (num=='best'){
            hfname<-subset(statesetfullhf[,2],as.numeric(statesetfullhf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)==min(as.numeric(outcomehfclean)))
            return(sort(hfname)[1])
            }
            else if (num=='worst'){
            hfname<-subset(statesetfullhf[,2],as.numeric(statesetfullhf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)==max(as.numeric(outcomehfclean)))
            return(sort(hfname)[1])
            }
            else {
            set_rank_hf<-cbind(statesetfullhf[,2],factor(statesetfullhf[,17]))
            colnames(set_rank_hf)<-c('hospital','rank')
            hfname<-set_rank_hf[set_rank_hf[,'rank']==num,1]
            return(sort(hfname)[1])
            }
        }
        else if (outcome=='pneumonia'){
            outcomep<-stateset[,23]
            outcomepclean<-subset(outcomep,!outcomep %in% 'Not Available')
            statesetfullp<-subset(stateset,!stateset[,23] %in% 'Not Available')
            if (num=='best'){
            pname<-subset(statesetfullp[,2],as.numeric(statesetfullp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)==min(as.numeric(outcomepclean)))
            return(sort(pname)[1])
            }
            else if (num=='worst'){
            pname<-subset(statesetfullp[,2],as.numeric(statesetfullp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)==max(as.numeric(outcomepclean)))
            return(sort(pname)[1])
        }
            else {
            set_rank_p<-cbind(statesetfullp[,2],factor(statesetfullp[,17]))
            colnames(set_rank_p)<-c('hospital','rank')
            pname<-set_rank_p[set_rank_p[,'rank']==num,1]
            return(sort(pname)[1])
            }
        }
        else {
       }
    }
    else if (!state %in% listofstate) stop('invalid state')
    else if (!outcome %in% c('heart attack','heart failure','pneumonia')) stop('invalid outcome')
}


