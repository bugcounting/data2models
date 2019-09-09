#!  /usr/bin/env Rscript

## Text of email message at `path`
body <- function (path) {
    # open file at `path` in read text mode (rt)
    fp <- file(path, open="rt", encoding="latin1")
    # extract lines as vector
    text <- readLines(fp)
    # drop lines before first empty line
    msg <- text[seq(which(text=="")[1]+1, length(text), 1)]
    close(fp)
    # return body as a single string with line breaks
    return(paste(msg, collapse="\n"))
}

## Paths to data
spam.path <- "spam/"
spam2.path <- "spam_2/"
easyham.path <- "easy_ham/"
easyham2.path <- "easy_ham_2/"
hardham.path <- "hard_ham/"

## List all files in `spam.path`
spam.files <- dir(spam.path)
## Remove file `cmds`, which is just a script
spam.files  <- spam.files[spam.files != "cmds"]
## extract body of all spam messages
spam.bodies <- sapply(spam.files, function(p) body(paste(spam.path, p, sep="")))

## Same for ham files
ham.files <- dir(easyham.path)
ham.files <- ham.files[ham.files != "cmds"]
## Make ham/spam sets balanced
ham.files <- ham.files[1:length(spam.files)]
ham.bodies <- sapply(ham.files, function(p) body(paste(easyham.path, p, sep="")))


library(tm)
term.doc.matrix <- function (docs) {
    # create corpus object (package tm)
    corpus <- Corpus(VectorSource(docs))
    # options
    options <- list(stopwords=TRUE,         # remove stopwords (common English words)
                    removePunctuation=TRUE, # remove punctuation
                    removeNumbers=TRUE,     # remove numbers
                    minDocFreq=2)           # remove words that appear only once
    tdm <- TermDocumentMatrix(corpus, options)
    return(as.matrix(tdm))
}

## Data frame with statistics about words in `docs`:
##   term: the word
##   count: how many times the word appeared
##   freq.found: fraction of documents where the word appeared
##   density: count/sum(count)
term.freq.table <- function (docs) {
    spam.matrix <- term.doc.matrix(docs)
    spam.counts <- rowSums(spam.matrix)
    spam.df <- data.frame(term=names(spam.counts), count=as.numeric(spam.counts),
                          stringsAsFactors=FALSE)
    n.docs <- ncol(spam.matrix)
    spam.df$freq.found <- apply(spam.matrix, 1, function (r) { sum(r > 0)/n.docs })
    spam.df$density <- spam.df$count / sum(spam.df$count)
    return(spam.df)
}

spam.df <- term.freq.table(spam.bodies)
## top-10 terms in order of frequency
spam.top10 <- head(spam.df[order(-spam.df$freq.found), ], 10)
## which is not the same as overall density
spam.top10.dens <- head(spam.df[order(-spam.df$density), ], 10)

## Same for ham
ham.df <- term.freq.table(ham.bodies)
ham.top10 <- head(ham.df[order(-ham.df$freq.found), ], 10)
## which is not the same as overall density
ham.top10.dens <- head(ham.df[order(-ham.df$density), ], 10)

