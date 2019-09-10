#!  /usr/bin/env Rscript

## Text of email message at `path`
body <- function (path)
{
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
term.doc.matrix <- function (docs)
{
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
term.freq.table <- function (docs)
{
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

## merge both datasets
training.df <- rbind(spam.df, ham.df)
## add a label that identifies whether an entry is about "spam" or "ham"
training.df$spam.ham <- as.factor(c(rep("spam", nrow(spam.df)), rep("ham", nrow(ham.df))))


example.msg.path <- paste(hardham.path, "0250.7c6cc716ce3f3bfad7130dd3c8d7b072", sep="")


## Posterior probability that message `msg.path` is of the same kind
## as the data in `training.df`.
##   - msg.path: path to text file with email message to be classified
##   - training.df: data frame with probabilities of terms in messages
##                  (used to compute likelihoods)
##   - prior: prior probability that a message is of that kind
##   - missing: likelihood assigned to terms not in the training set
posterior <- function(msg.path, training.df, prior=0.5, missing=1e-6)
{
    msg <- body(msg.path)
    msg.matrix <- term.doc.matrix(msg)  # matrix with one column only
    term.counts <- rowSums(msg.matrix)
    ## terms in `msg` that are also in training set
    common.terms <- intersect(names(term.counts), training.df$term)
    ## likelihood P(data | training)
    likelihood <- if(length(common.terms) > 0)
                      prod(training.df$freq.found[match(common.terms, training.df$term)], na.rm=TRUE)
                  else missing^length(term.counts)
    ## posterior = likelihood * prior
    return(likelihood * prior)
}

## For each message in files `docs`, is it more likely to be spam than ham?
##   - docs: paths to text files with email messages to be classified
##   - training.spam: data frame with probabilities of terms in spam messages
##   - training.ham: data frame with probabilities of terms in ham messages
##   - prior.spam: prior probability that a message is spam
##   - missing: likelihood assigned to terms not in the training set
is.spam <- function(docs, training.spam, training.ham, prior.spam=0.5, missing=1e-6) {
    post.spam <- sapply(docs, function (p) posterior(p, training.spam, prior.spam, missing))
    post.ham <- sapply(docs, function (p) posterior(p, training.ham, 1-prior.spam, missing))
    data.frame(msg=docs, post.spam=post.spam, post.ham=post.ham, is.spam=post.spam > post.ham)
}

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]
hardham.docs <- sapply(hardham.docs, function(p) paste(hardham.path, p, sep=""))

hardham.classify <- is.spam(hardham.docs, spam.df, ham.df)
## summary(hardham.classify)

positives <- sum(hardham.classify$is.spam)
negatives <- sum(!hardham.classify$is.spam)
misclassified <- positives/(positives + negatives)
