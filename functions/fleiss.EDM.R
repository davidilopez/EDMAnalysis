fleiss.EDM <- function(data, is.raw = TRUE, conf.1.rm = FALSE, rel = FALSE, polarize = FALSE, iterations = 1000, min.ratings = 3, selection = "LONG"){
        require("irr")
        require('psych')
        source("functions/clean.data.R")
        source("functions/reliability.check.R")
        if (is.raw == TRUE) {
                if (rel == TRUE) {
                        data <- reliability.check(data)
                }
                dt <- clean.data.subjectwise(data, conf.1.rm=conf.1.rm, polarize=polarize)
                if (selection != "LONG") {
                        dt <- dt[,c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)]
                }
        } else {
                dt <- data
        }

        kik <- sum(colSums(!is.na(dt)) < min.ratings)
        dt <- dt[,colSums(!is.na(dt)) >= min.ratings]

        desc <- as.data.frame(describe(dt))
        tmin <- min(desc$n)
        fd <- as.data.frame(matrix(data = 0, nrow=iterations, ncol=5, dimnames = list(seq(1,iterations), c('Subjects', 'Raters', 'Kappa', 'z', 'p.value'))))
        for (i in 1:iterations) {
                d <- dt[sample(nrow(dt)),]
                m2 <- t(apply(t(d), 1, function(x) x[order(is.na(x))])) # sort NAs to end of each row
                t <- m2[, 1:tmin]
                fleiss <- kappam.fleiss(t)
#                 if (i == 1) colnames(fd) = colnames(fleiss$results)
                fd$Subjects[i] <- fleiss$subjects
                fd$Raters[i] <- fleiss$raters
                fd$Kappa[i] <- fleiss$value
                fd$z[i] <- fleiss$statistic
                fd$p.value[i] <- fleiss$p.value
        }

        print(paste("Minimum ratings: ", as.character(tmin)))
        print(paste("Number of subjects: ", nrow(dt)))
        print(paste("Number of pairs: ", ncol(dt)))
        if (kik != 0){
                print(paste("Number of pairs with less than", as.character(min.ratings),"ratings:", kik))
        }
        desc.fd <- describe(fd)
        print(desc.fd[3:5, c("mean", "sd", "median", "min", "max")])

}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014
