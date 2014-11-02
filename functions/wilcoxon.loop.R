wilcoxon.loop <- function(data.1, data.2) {
        if (ncol(data.1) != ncol(data.2)) {
                stop("Both datasets need to have the same amount of pairs.")
        }
        output <- matrix(data = NA, nrow = ncol(data.1), ncol = 3, dimnames = list(colnames(data.1), c("W", "p.value", " ")))
        for ( i in 1:ncol(data.1) ) {
                temp <- wilcox.test(x = data.1[, i], y = data.2[, i])
                output[i, 1] <- temp$statistic
                output[i, 2] <- temp$p.value
                if (temp$p.value < 0.001){
                        output[i, 3] <- "***"
                } else if (temp$p.value < 0.01) {
                        output[i, 3] <- "**"
                } else if (temp$p.value < 0.05) {
                        output[i, 3] <- "*"
                } else {
                        output[i, 3] <- " "
                }
        }
        return(output)
}