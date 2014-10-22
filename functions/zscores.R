zscores <- function(data) {
#         Load the pair list.
        source("tracklist.R")
#         Load helper functions
        source("reliability.check.R")
        source("clean.data.R")
#         Run the data through the functions
#         Remove unreliable cases
        reliable.data <- reliability.check(data)
#         Clean and format the remaining cases
        clean.data <- clean.data.pairwise(raw.data=reliable.data)
        zscores <- seq(1,190)
        for (i in 4:ncol(clean.data)) {
                b <- scale(clean.data[,i])
                zscores <- cbind(zscores, b)
        }
        colnames(zscores) <- c("PairNumber", seq(1,ncol(zscores) - 1))
        zscore.means <- merge(tracklist, (cbind(zscores, rowMeans(zscores[,2:ncol(zscores)], na.rm=T))), by="PairNumber")
        zscore.means.ordered <- zscore.means[order(abs(zscore.means[,ncol(zscore.means)])),]
        
}
# 
# datafull <- read.delim("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/Analysis/20140602/tc.csv")
# datasafe <- read.delim("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/Analysis/20140602/tc(no dubious).csv")
# 
# a <- zscores(datafull)
# b <- zscores(datasafe)