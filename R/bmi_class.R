bmi_class <- function(bmi,labels=TRUE) {
    ## Who: http://apps.who.int/bmi/index.jsp?introPage=intro_3.html
    res <- ifelse(bmi<18.5,1, ifelse(bmi<25,2,  ifelse(bmi<30,3,4)))
    lab <- if(labels) {
        c("Underweight","Normal range", "Overweight","Obese")
    } else {
        c("[min-18.5)","[18.5-25)","[25-30)", "[30-max]")
    }
    
    factor(res, levels=1:4, labels=lab)
}
