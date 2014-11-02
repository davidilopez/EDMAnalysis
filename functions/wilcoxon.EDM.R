wilcoxon.EDM <- function(data.1, data.2, conf.1.rm = FALSE, is.raw = TRUE, polarize = FALSE, iterations = 1000){
        require("irr")
        source("functions/clean.data.R")
        source("functions/reliability.check.R")
        if (is.raw == TRUE) {
                data.1 <- clean.data.subjectwise(reliability.check(data.1), conf.1.rm=conf.1.rm, polarize=polarize)
                data.2 <- clean.data.subjectwise(reliability.check(data.2), conf.1.rm=conf.1.rm, polarize=polarize)
        }
        desc.data.1 <- as.data.frame(describe(data.1))
        desc.data.2 <- as.data.frame(describe(data.2))
        tmin <- min(c(min(desc.data.1$n), min(desc.data.2$n)))
        fd <- as.data.frame(matrix(nrow=0, ncol=1))
        for (i in 1:iterations) {
                dta <- data.1[sample(ncol(data.1)),]
                gnr <- data.1[sample(ncol(data.2)),]
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
