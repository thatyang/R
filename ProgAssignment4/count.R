count<-function(cause=NULL){
    if (!is.null(cause) && (cause %in% c('asphyxiation','blunt force','other','shooting','stabbing','unknown'))){
        homicides<-readLines('homicides.txt')
        if (cause=='asphyxiation'){
            return (length(grep('[Cc]ause: [Aa]sphyxiation',homicides)))
        }
        else if (cause=='blunt force'){
            return (length(grep('[Cc]ause: [Bb]lunt [Ff]orce',homicides)))
        }
        else if (cause=='other'){
            return (length(grep('[Cc]ause: [Oo]ther',homicides)))
        }
        else if (cause=='shooting'){
            return (length(grep('[Cc]ause: [Ss]hooting',homicides)))
        }
        else if (cause=='stabbing'){
            return (length(grep('[Cc]ause: [Ss]tabbing',homicides)))
        }
        else if (cause=='unknown'){
            return (length(grep('[Cc]ause: [Uu]nknown',homicides)))
        }
        else stop('invalid')
    }
    else stop ('invalid')
}
