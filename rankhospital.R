rankhospital <- function(state, outcome, num = "best"){
        options(warn=-1)
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        colnames(dat)[11] <- "heart attack"
        colnames(dat)[17] <- "heart failure"
        colnames(dat)[23] <- "pneumonia"
        dat$`heart attack` <- as.numeric(dat$`heart attack`)
        dat$`heart failure` <- as.numeric(dat$`heart failure`)
        dat$pneumonia <- as.numeric(dat$pneumonia)
        
        vstate <- as.vector(
                (as.data.frame(table(dat[ ,7])))[,1]
        )
        voutcome <- c("heart attack", "heart failure", "pneumonia")
        voutcome <- as.vector(voutcome)
        
        if (! state %in% vstate){
                stop("invalid state", call. = T)
        }
        
        if (! outcome %in% voutcome){
                stop("invalid outcome", call. = T)
        }
        dat <- dat[, c(2,7,11,17,23)]
        
        spl <- split(dat, dat$State)
        
        use <- spl[[state]]
        
        sorted <- use[order(use[,outcome], use$Hospital.Name),]
        
        Rank <- 1:nrow(sorted)
        
        finaldata <- cbind(sorted, Rank)
        
        if (outcome == "heart attack"){
                finaldata <- finaldata[,c(1,3,6)]
        }
        else if(outcome == "heart failure"){
                finaldata <- finaldata[,c(1,4,6)]
        }
        else {
                finaldata <- finaldata[,c(1,5,6)]
        }
        names(finaldata)[2] <- "Rate"
        
        finaldata <- na.omit(finaldata)
        
        if (num == "best"){
                return(finaldata[1,1])
        }
        else if (num == "worst"){
                return(finaldata[nrow(finaldata),1])
        }
        else if (num > nrow(finaldata)){
                return(NA)
        }
        else {
                num <- as.numeric(num)
                return(finaldata[num,1])
        }
}





