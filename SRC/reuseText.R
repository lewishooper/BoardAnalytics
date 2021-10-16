### TEST FILE TextReuse
library(textreuse)
file<-system.file("extdata/ats/remember00palm.txt",package='textreuse')
doc<-TextReuseTextDocument(file=file,meta=list("publisher"= "ATS"),
                           tokenizer=tokenize_ngrams, n=5,
                           keep_tokens = TRUE)
doc2<-TextReuseTextDocument(file="~/BoardAnalytics/LHSC.txt",meta=list("publisher"= "LHSC"),
                           tokenizer=tokenize_ngrams, n=5,
                           keep_tokens = TRUE)
dir <- system.file("extdata/ats", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir, tokenizer = tokenize_ngrams, n = 5,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = TRUE)

corpus2 <- TextReuseCorpus(dir = "~/BoardAnalytics/LHSCText", tokenizer = tokenize_ngrams, n = 5,
                           minhash_func = minhash, keep_tokens = TRUE,
                           progress = TRUE)

corpus2[[6]][1]

names(corpus)
corpus["remember00palm"]
str_length(corpus2[[15]])
wordcount(corpus2)
jaccard_similarity(corpus2[[20]], 
                   corpus2[[19]])
LHSCCOmparisions<-pairwise_compare(corpus2,jaccard_similarity,progress=TRUE)
candidates<-pairwise_candidates(LHSCCOmparisions)
minhash <- minhash_generator(n = 240, seed = 3552)
head(minhash(c("turn tokens into", "tokens into hashes", "into hashes fast")))                            
