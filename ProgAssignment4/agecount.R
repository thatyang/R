agecount<-function(age=null){
    if (!is.null(age)){
        return (length(grep(paste(' ',age,' years old',sep=''),homicides)))
    }
    else stop('invalid')
}
