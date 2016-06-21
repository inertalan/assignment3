best <- function(state, outcome){
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
        
        if (outcome == "heart attack"){
                use <- use[,c(1,2,3)]
        }
        else if (outcome == "heart failure"){
                use <- use[,c(1,2,4)]
        }
        else {
                use <- use[,c(1,2,5)]
        }
        
        use <- na.omit(use)

        sorted <- use[order(use[,outcome], use$Hospital.Name),]
        
        return(sorted[1,1])
        
}





