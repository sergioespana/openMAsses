library(keypress)
options(width = 120)

t = c("Mary", "sees", "John", "Jones", "and", "John", "Jones", "sees", "John", "Miller")
table(t)
plot(as.factor(t))

library(pdftools)
txt = pdf_text("ubd_strategicplan_en.pdf")
tfull = unlist(txt)
tokens = unlist(strsplit(tfull, "[\\W]", perl = TRUE))
t2 = tokens[tokens != ""]
df = data.frame(token = as.factor(t2))
head(summary(df$token))
par(mar = c(7, 6, 4, 1))
barplot(head(summary(df$token), n = 25), las = 2, ylim = c(0, 1200))
contentwords = summary(df$token)[c(5, 7, 14, 15, 17, 20, 23, 25, 27, 28, 31, 32, 34, 35, 36, 37, 38, 40, 41, 42, 43, 44, 45, 46, 47)]
barplot(contentwords, las = 2, ylim = c(0, 500))
df3x = data.frame(token = as.factor(tolower(t2)))
barplot(summary(df3x$token)[c(5, 7, 12, 14, 15, 17, 18, 20, 25, 27, 30, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 46, 48, 49)], las = 2, ylim = c(0, 500))
barplot(summary(df3x$token)[c(5, 7, 12, 14, 15, 17, 18, 20, 25, 33, 34, 35, 36, 37, 39, 40, 41, 43, 44, 46, 48, 49, 50, 51, 52)], las = 2, ylim = c(0, 500))

library(htmltab)
tentamens = htmltab("http://www.cs.uu.nl/education/tentamen.php", which = 1)
vak = "Algoritmiek"
tentamens[tentamens[, 1] == vak,]

library(readr)
library(stringr)
t2 = read_lines("tentamens.htm")
t2[126:130]
t3 = paste(t2, collapse = " ")
pattern = "Algoritmiek.*?<td.*?<td.*?>(.*?)</td>"
m = str_match(t3, pattern)
m[1, 2]

library(tm)
doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

doc.list <- c(doc1, doc2, doc3, doc4, doc5, doc6, doc7)

my.docs <- VectorSource(doc.list)
my.corpus <- Corpus(my.docs)
my.corpus <- tm_map(my.corpus, removePunctuation)

tdm <- TermDocumentMatrix(my.corpus)
tdm.tfidf = weightTfIdf(tdm)
colnames(tdm.tfidf) <- c("doc1", "doc2", "doc3", "doc4", "doc5", "doc6", "doc7")

tm_term_score(tdm.tfidf, c("best", "food"))