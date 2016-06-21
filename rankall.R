rankall <- function(outcome, num = "best") {
        ## Read outcome data options(warn=-1)
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        colnames(dat)[11] <- "heart attack"
        colnames(dat)[17] <- "heart failure"
        colnames(dat)[23] <- "pneumonia"
        dat$`heart attack` <- as.numeric(dat$`heart attack`)
        dat$`heart failure` <- as.numeric(dat$`heart failure`)
        dat$pneumonia <- as.numeric(dat$pneumonia)
        
        voutcome <- c("heart attack", "heart failure", "pneumonia")
        voutcome <- as.vector(voutcome)
        ## Check that state and outcome are valid
        if (! outcome %in% voutcome){
                stop("invalid outcome", call. = T)
        }
        dat <- dat[, c(2,7,11,17,23)]
        ## For each state, find the hospital of the given rank
        spl <- split(dat, dat$State)
        
        subspl <- function(da){
                
                if (outcome == "heart attack"){
                         subda<- da[,c(1,2,3)]
                }
                else if(outcome == "heart failure"){
                        subda <- da[,c(1,2,4)]
                }
                else {
                        subda <- da[,c(1,2,5)]
                }
                return(subda)
        }
        
        subdata <- lapply(spl, subspl)
        
        use <- lapply(subdata, na.omit)
        
        hrank <- function(ll){
                sorted <- ll[order(ll[,outcome], ll$Hospital.Name),]
                
                if (num == "best"){
                        finaldata <- sorted[1,1:2]
                }
                else if (num == "worst"){
                        finaldata <- sorted[nrow(sorted),1:2]
                }
                else if (num > nrow(sorted)){
                        Hospital.name <- NA
                        
                        finaldata <- data.frame(Hospital.name,sorted[,2])
                        finaldata <- finaldata[1,]
                        
                }
                else {
                        num <- as.numeric(num)
                        finaldata <- sorted[num,1:2]
                }
                
                names(finaldata) <- c("hospital", "state")
                return(finaldata)
                
                
        }
        
        hlist <- lapply(use, hrank)
        
        dd <- data.frame()
                
        for (i in 1:length(hlist)){
                dd<- rbind(dd,hlist[[i]])
        }
        
       
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return(dd)
}


