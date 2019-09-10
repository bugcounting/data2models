#! /bin/bash

for f in 20030228_easy_ham.tar.bz2 20030228_easy_ham_2.tar.bz2 20030228_spam.tar.bz2 20030228_spam_2.tar.bz2 20021010_hard_ham.tar.bz2 20030228_hard_ham.tar.bz2; do
	 wget "https://spamassassin.apache.org/old/publiccorpus/$f"
	 tar xvjf "$f"
	 rm "$f"
done

wget "http://www.aueb.gr/users/ion/data/lingspam_public.tar.gz"
tar xvf lingspam_public.tar.gz lingspam_public/bare
rm lingspam_public.tar.gz

