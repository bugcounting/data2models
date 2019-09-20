#!  /usr/bin/env Rscript

install.packages(c("tm", "rpicosat"))

sa.archives  <- c("20030228_easy_ham.tar.bz2",
                  "20030228_easy_ham_2.tar.bz2",
                  "20030228_spam.tar.bz2",
                  "20030228_spam_2.tar.bz2",
                  "20021010_hard_ham.tar.bz2",
                  "20030228_hard_ham.tar.bz2")
for (f in sa.archives) {
    download.file(paste("https://spamassassin.apache.org/old/publiccorpus/", f, sep=""), f)
    untar(f, list=FALSE)
}

f  <- "lingspam_public.tar.gz"
download.file(paste("http://www.aueb.gr/users/ion/data/", f, sep=""), f)
untar(f, list=FALSE, files="lingspam_public/bare")
