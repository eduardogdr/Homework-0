library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
x <- str_split(txt[9], "\n")
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)

s<-str_trim(s)
s

header_index<-str_which(s,"2015")
header_index

tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month

tail_index <- str_which(s,"Total")
tail_index

n <- str_count(s, "\\d+")
sum(n == 1)


out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
colnames(s)<-c("day", "2015","2016","2017","2018")
class(s)<-"numeric"

mean(s[,2])
mean(s[1:19,4])
mean(s[20:30,4])

