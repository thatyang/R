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
            haname<-subset(stateset[,2],stateset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==as.character(min(as.numeric(outcomehaclean))))
            return(haname)
            }
            else if (num=='worst'){
            haname<-subset(stateset[,2],stateset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==as.character(max(as.numeric(outcomehaclean))))
            return(haname)
            }
            else {
            set_rank<-cbind(statesetfull[,2],factor(statesetfull[,11]))
            colnames(set_rank)<-c('hospital','rank')
            haname<-set_rank[set_rank[,'rank']==num,1]
            return(haname)
            }
        }
        else if (outcome=='heart failure'){
            outcomehf<-stateset[,17]
            outcomehfclean<-subset(outcomehf,!outcomehf %in% 'Not Available')
            hfname<-subse(stateset[,2],stateset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==as.character(min(as.numeric(outcomehfclean))))
            return(hfname)
        }
        else if (outcome=='pneumonia'){
            outcomep<-stateset[,23]
            outcomepclean<-subset(outcomep,!outcomep %in% 'Not Available')
            pname<-subset(stateset[,2],stateset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==as.character(min(as.numeric(outcomepclean))))
            return(pname)
        }
        else {
       }
    }
    else if (!state %in% listofstate) stop('invalid state')
    else if (!outcome %in% c('heart attack','heart failure','pneumonia')) stop('invalid outcome')
}


