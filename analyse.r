#!/usr/bin/Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0)
    ## args <- c('H:\\Data\\4_*')
    ## args <- c('H:\\Data\\04_TFA-SEC9-ABS_I80_H3BO3+28kV_20°C-Rep1.asc')
    args <- c('04_*')

library(stringr)

loadData <- function(fileName) {
    inp <- readLines(fileName, encoding = 'latin1')

    ret <- list()

    headLines <- str_subset(inp, '^[\\w\\s]+:\t')
    for (s in strsplit(headLines, '\t')) {
        key <- gsub(':$', '', s[1])
        val <- if (length(s) == 2) s[2] else s[2:length(s)]
        ret[[key]] <- val
    }

    if (exists('Acquisition Date and Time', ret))
        ret$`Acquisition Date and Time` <- as.POSIXct(ret$`Acquisition Date and Time`,
                                                       format = '%m/%d/%Y %H:%M:%S')

    colLens = as.integer(head(ret$`Total Data Points`, -1))
    ret$d = matrix(NA, nrow = max(colLens), ncol = length(colLens))

    begin <- length(headLines) + 1
    col <- 1
    for (len in colLens) {
        ret$d[seq_len(len), col] <- as.integer(inp[begin:(begin + len - 1)])

        begin <- begin + len
        col <- col + 1
    }

    return(ret)
}

ds <- sapply(Sys.glob(args), function (fn) loadData(fn),
             USE.NAMES = TRUE, simplify = FALSE)
d <- ds[[1]]

## typeof(ds)
## d$d
## lapply(ds, function (x) x$`Total Data Points`)

clock <- seq(max(sapply(ds, function (x) nrow(x$d)))) / as.double(d$`Sampling Rate`[1])

colours <- rainbow(length(ds))

fileName <- gsub('-Rep.*', '', names(ds)[1])

# placement of diagrams per page (ROWS, COLS)
## par(mfrow = c(2, 1))
for (chan in seq(ncol(d$d))) {
    val = matrix(NA, length(clock), length(ds))
    for (i in seq(length(ds))) {
        dval <- ds[[i]]$d[,chan] * as.double(ds[[i]]$`Y Axis Multiplier`[chan])
        val[1:length(dval), i] = dval
    }

    matplot(clock * as.double(d$`X Axis Multiplier`[chan]), val,
            type = 'l', lty = 1, col = colours,
            main = paste('Channel', chan, 'of', fileName),
            xlab = d$`X Axis Title`[chan],
            ylab = d$`Y Axis Title`[chan])
    legend('topright', gsub('.*-Rep(\\d+).*', 'Repetition \\1', names(ds[seq(ncol(val))])),
           fill = colours)
}

print(warnings())
