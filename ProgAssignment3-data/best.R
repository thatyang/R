best<-function(state,outcome){
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
            haname<-subset(stateset[,2],stateset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==as.character(min(as.numeric(outcomehaclean))))
            return(haname)
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


