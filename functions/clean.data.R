## This file has two functions to clean raw data obtained from the Survey Gizmo 
# platform, formatted according to the output containing the two reliability
# check pairs.

## The function "clean.data.pairwise" outputs the data with the subjects as variables
# and the pairs as cases. It takes answers from the raw data (disregarding
# the reliability check pairs), removes the ratings that were reported as "Not
# confident" if conf.1.rm = TRUE (default) and outputs a data frame with the 
# following columns:
#         Pair number
#         Track 1 name
#         Track 2 name
#         Each participant as a column
clean.data.pairwise <- function(raw.data, conf.1.rm = FALSE, tracknames = FALSE, polarize = FALSE){
        #         Get the ratings from the raw data
        if (ncol(raw.data) >= 453){
                data = raw.data[, seq(27, 406, 2)]
                colnames(data) <- seq(1,190)
                
        } else if (ncol(raw.data) == 101){
                data = raw.data[, seq(22, 57, 2)]
                colnames(data) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
        }
        #         Get the confidence levels for the ratings
        if (ncol(raw.data) >= 453){
                conf = raw.data[, seq(28, 406, 2)]
                colnames(conf) <- seq(1,190)
                
        } else if (ncol(raw.data) == 101){
                conf = raw.data[, seq(23, 57, 2)]
                colnames(conf) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
        } 
        #         Remove the ratings reported as "Not confident"
        if (conf.1.rm == T) data[conf == 1] = NA
        if (polarize == TRUE) {
                data[data == 2] <- 1
                data[data == 3 | data == 4] <- 2
        }
        #         Number the pairs and transpose the database
        if (tracknames == TRUE) {
                tdata <- cbind(seq(1,190), t(x=data))
                colnames(tdata) <- c("PairNumber", seq(1, length(data[,1])))
        } else {
                tdata <- t(data)
        }
        #         Load the pair list and merge it with the database
        if (tracknames == TRUE) {
                source("functions/tracklist.R")
                merge(x=tracklist, y=tdata, by="PairNumber")
        } else {
                tdata
        }
}

## The function "clean.data.subjectwise" outputs the data with the pairs as variables
# and the subjects as cases. It takes answers from the raw data (disregarding
# the reliability check pairs), and removes the ratings that were reported as "Not
# confident" if conf.1.rm = TRUE (default). 
clean.data.subjectwise <- function(raw.data, conf.1.rm = FALSE, polarize = FALSE){
        #         Get the ratings from the raw data
        if (ncol(raw.data) >= 453){
                data = raw.data[, seq(27, 406, 2)]
                colnames(data) <- seq(1,190)
                
        } else if (ncol(raw.data) == 101){
                data = raw.data[, seq(22, 57, 2)]
                colnames(data) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
        }
        #         Get the confidence levels for the ratings
        if (ncol(raw.data) >= 453){
                conf = raw.data[, seq(28, 406, 2)]
                colnames(conf) <- seq(1,190)
                
        } else if (ncol(raw.data) == 101){
                conf = raw.data[, seq(23, 57, 2)]
                colnames(conf) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
        }  
        #         Remove the ratings reported as "Not confident"
        if (conf.1.rm == T) data[conf == 1] = NA
        if (polarize == TRUE) {
                data[data == 2] <- 1
                data[data == 3 | data == 4] <- 2
        }
        #         Number the pairs (column-wise)
        data
}

confidence.values <- function(raw.data, conf.1.rm = FALSE, polarize = FALSE){
        #         Get the confidence levels for the ratings
        if (ncol(raw.data) >= 453){
                conf = raw.data[, seq(28, 406, 2)]
                colnames(conf) <- seq(1,190)
                
        } else if (ncol(raw.data) == 101){
                conf = raw.data[, seq(23, 57, 2)]
                colnames(conf) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
        } 
        if (polarize == TRUE) {
                data[data == 2] <- 1
                data[data == 3 | data == 4] <- 2
        }
        #         Remove the ratings reported as "Not confident"
        if (conf.1.rm == T) conf[conf == 1] = NA
        #         Number the pairs (column-wise)
        conf
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014