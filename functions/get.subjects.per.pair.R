get.subjects.per.pair <- function(data, polarize = FALSE){
        subject.vs.pairs <- matrix(nrow = nrow(data), ncol = 60)
        rownames(subject.vs.pairs) <- 1:nrow(data)
        subject.vs.score <- matrix(nrow = nrow(data), ncol = 60)
        rownames(subject.vs.score) <- 1:nrow(data)
        for (i in 1:nrow(data)){
                subject.vs.pairs[i,] <- as.numeric(colnames(data[i,!is.na(data[i,])]))
                subject.vs.score[i,] <- as.numeric(data[i,!is.na(data[i,])])
        }
        if (polarize == TRUE) {
                subject.vs.score[subject.vs.score == 2] <- 1
                subject.vs.score[subject.vs.score == 3 | subject.vs.score == 4] <- 2
        }
        final <- list(By.pair = subject.vs.pairs, By.scores = subject.vs.score)
}

for (i in 1:190){
        tmp <- kappam.fleiss(t(final$By.scores[final$By.pair == i]))
        if (tmp$value < 0){
                print(paste("Fleiss' Kappa for pair", as.character(i), ":", as.character(tmp$value), ", p value:", as.character(tmp$p.value)))        
        }
        
}

timbre.pair.4point <- get.subjects.per.pair(clean.timbre)
timbre.pair.2point <- get.subjects.per.pair(clean.timbre, polarize = T)

for (i in highest.sd.pairs){
        hist(timbre.pair.4point$By.scores[timbre.pair.4point$By.pair == i], main = paste("Pair", as.character(i), "(4 point scale)"), xlab = "Rating")  
}

for (i in highest.sd.pairs){
        hist(timbre.pair.2point$By.scores[timbre.pair.2point$By.pair == i], main = paste("Pair", as.character(i), "(2 point scale)"), xlab = "Rating")  
}



