#!  /usr/bin/env Rscript

## Text of email message at `path`
body <- function (path, lingspam=FALSE)
{
    # open file at `path` in read text mode (rt)
    fp <- file(path, open="rt", encoding="latin1")
    # extract lines as vector
    text <- readLines(fp)
    # drop lines before first empty line
    msg <- if (lingspam) text else text[seq(which(text=="")[1]+1, length(text), 1)]
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
spam.docs <- sapply(spam.files, function(p) paste(spam.path, p, sep=""))
## extract body of all spam messages
spam.bodies <- sapply(spam.files, function(p) body(paste(spam.path, p, sep="")))

## Same for ham files
ham.files <- dir(easyham.path)
ham.files <- ham.files[ham.files != "cmds"]
easyham.docs <- sapply(ham.files, function(p) paste(easyham.path, p, sep=""))
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
##   - term: the word
##   - count: how many times the word appeared
##   - freq.found: fraction of documents where the word appeared
##   - density: count/sum(count)
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
    ## Likelihood for independent data points: product of P(data | training)
    ## For terms not in the training set, we use a small probability `missing`
    likelihood <- prod(training.df$freq.found[match(common.terms, training.df$term)]) *
                                    missing^(length(term.counts)-length(common.terms))
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

spam2.docs <- dir(spam2.path)
spam2.docs  <- spam2.docs[spam2.docs != "cmds"]
spam2.docs <- sapply(spam2.docs, function(p) paste(spam2.path, p, sep=""))

easyham2.docs <- dir(easyham2.path)
easyham2.docs  <- easyham2.docs[easyham2.docs != "cmds"]
easyham2.docs <- sapply(easyham2.docs, function(p) paste(easyham2.path, p, sep=""))

hardham.classify <- is.spam(hardham.docs, spam.df, ham.df)
## summary(hardham.classify)
spam2.classify <- is.spam(spam2.docs, spam.df, ham.df)
easyham2.classify <- is.spam(easyham2.docs, spam.df, ham.df)

positives <- sum(hardham.classify$is.spam)
negatives <- sum(!hardham.classify$is.spam)
false.pos <- positives/(positives + negatives)

## Changing priors only has some effect on examples where classification is mixed
hardham.classify.2 <- is.spam(hardham.docs, spam.df, ham.df, prior=0.05)
spam2.classify.2 <- is.spam(spam2.docs, spam.df, ham.df, prior=0.05)
easyham2.classify.2 <- is.spam(easyham2.docs, spam.df, ham.df, prior=0.05)


performance <- function(classify.df, label)
{
    if (length(label) != nrow(classify.df))
        label <- rep(label[1], nrow(classify.df))
    df <- cbind(classify.df, label=label)
    true.positives <- sum(df$is.spam & df$label)
    true.negatives <- sum(!df$is.spam & !df$label)
    false.positives <- sum(df$is.spam & !df$label)
    false.negatives <- sum(!df$is.spam & df$label)
    list(true.positives=true.positives, true.negatives=true.negatives,
         false.positives=false.positives, false.negatives=false.negatives,
         accuracy=(true.positives+true.negatives)/(true.positives+true.negatives+false.positives+false.negatives),
         false.discovery.rate=false.positives/(false.positives+true.positives),
         false.omission.rate=false.negatives/(false.negatives+true.negatives))
}

labels <- c(rep(FALSE, nrow(hardham.classify)),
            rep(TRUE, nrow(spam2.classify)),
            rep(FALSE, nrow(easyham2.classify)))
all.classify <- rbind(hardham.classify, spam2.classify, easyham2.classify)
performance(all.classify, labels)


ls.path <- "lingspam_public/bare/"
ls.test.path <- "lingspam_public/bare/part10/"

ls.test.docs <- dir(ls.test.path)
ls.test.docs <- sapply(ls.test.docs, function(p) paste(ls.test.path, p, sep=""))
## Create correct classification label (TRUE == is.spam) according to filename
fnames <- names(ls.test.docs)
labels <- rep(FALSE, length(ls.test.docs))
labels[sapply(fnames, function(fn) grepl("spmsg.*[.]txt", fn))] <- TRUE

ls.test.classify <- is.spam(ls.test.docs, spam.df, ham.df)
performance(ls.test.classify, labels)
