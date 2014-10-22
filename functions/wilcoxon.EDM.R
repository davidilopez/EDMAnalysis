wilcoxon.EDM <- function(data1, general.data, conf.1.rm = TRUE, polarize = FALSE, iterations = 1000){
        require("irr")
        source("clean.data.R")
        source("reliability.check.R")
        data.1 <- clean.data.subjectwise(reliability.check(data1), conf.1.rm=conf.1.rm, polarize=polarize)
        general.1 <- clean.data.subjectwise(reliability.check(general.data), conf.1.rm=conf.1.rm, polarize=polarize)
        desc.data1 <- as.data.frame(describe(data.1))
        desc.general1 <- as.data.frame(describe(general.1))
        
        tmin <- min(desc.data1$n)
        fd <- as.data.frame(matrix(nrow=0, ncol=1))
        for (i in 1:iterations) {
                dta <- data.1[sample(ncol(data.1)),]
                gnr <- data.1[sample(ncol(general.1)),]
                dtm2 <- t(apply(t(dta), 1, function(x) x[order(is.na(x))])) # sort NAs to end of ea row
                gnrm2 <- t(apply(t(gnr), 1, function(x) x[order(is.na(x))])) # sort NAs to end of ea row
                
                tdt <- dtm2[, 1:tmin]
                tgnr <- gnrm2[, 1:tmin]
                
                wilcoxon <- wilcox.test(tdt, tgnr)
                if (i == 1) colnames(fd) = colnames(wilcoxon$statistic)
                fd <- rbind(fd, wilcoxon$statistic)
        }
        print(paste("Minimum ratings: ", as.character(tmin)))
        print(describe(fd))
        
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014
