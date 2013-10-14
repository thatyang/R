best<-function(state,outcome){
    hpfile<-read.csv('outcome-of-care-measures.csv',colclasses='character')
    listofstate<-unique(hpfile[,7])
#   print(listofstate)
#   print(state)
    if (state %in% listofstate && outcome %in% c('heart attack','heart failure','pneumonia')
 ){
    stateset<-subset(hpfile,state==state)
#   print('valid state and outcome')
       if (outcome=='heart attack'){
            outcomeha<-stateset[,11]
            outcomehaclean<-subset(outcomeha,!outcomeha %in% 'not available')
            haname<-subset(stateset[,2],stateset$hospital.30.day.death..mortality..rates.from.heart.attack==as.character(min(as.numeric(outcomehaclean))))
            return(haname)
        }
        else if (outcome=='heart failure'){
            outcomehf<-stateset[,17]
            outcomehfclean<-subset(outcomehf,!outcomehf %in% 'not available')
            hfname<-subset(stateset[,2],stateset$hospital.30.day.death..mortality..rates.from.heart.failure==as.character(min(as.numeric(outcomehfclean))))
            return(hfname)
        }
        else if (outcome=='pneumonia'){
            outcomep<-stateset[,23]
            outcomepclean<-subset(outcomep,!outcomep %in% 'not available')
            pname<-subset(stateset[,2],stateset$hospital.30.day.death..mortality..rates.from.pneumonia==as.character(min(as.numeric(outcomepclean))))
            return(pname)
        }
        else {
       }
    }
    else if (!state %in% listofstate) stop('invalid state')
    else if (!outcome %in% c('heart attack','heart failure','pneumonia')) stop('invalid outcome')
}


