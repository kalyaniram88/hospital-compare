best <- function(state, outcome) {
    oc <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    oc[,11] <- suppressWarnings(as.numeric(oc[,11]))
    oc[,17] <- suppressWarnings(as.numeric(oc[,17]))
    oc[,23] <- suppressWarnings(as.numeric(oc[,23]))
    states <- unique(oc$State)
    conditions <- c('heart attack', 'heart failure', 'pneumonia')
    if (!state %in% states) { stop('invalid state') }
    if (!outcome %in% conditions) { stop('invalid outcome') }
    soc <- oc[grep(state, oc$State, ignore.case=T),]
    if (outcome == 'heart attack' ) { selector <- 11 }
    if (outcome == 'heart failure' ) { selector <- 17 }
    if (outcome == 'pneumonia' ) { selector <- 23 }

    sorted <- soc[order(soc[,selector],soc[,2]),]
    sorted[1,2]
}