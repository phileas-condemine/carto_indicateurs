# synonyme=readLines("synonymes/sortieDES.txt")
# http://crisco.unicaen.fr/des/
synonyme=jsonlite::fromJSON("synonymes/sortieDES.txt")
head(synonyme)

synonymes_simple=sapply(synonyme,function(x)x$synonymes)
library(data.table)
synonymes_simple_dt=pbapply::pblapply(1:length(synonymes_simple),function(x){
  data.table(from=names(synonymes_simple)[x],
             to=synonymes_simple[x][[1]])})
synonymes_simple_dt=do.call("rbind",synonymes_simple_dt)

load("data/init_data2.RData")

init_vars_to_show=
  c("Base","Indicateur",
    "fixed_prod"
  )
library(dplyr)
corpus=index$Indicateur
corpus=tolower(corpus)

words_from=sort(unique(synonymes_simple_dt$from))
words_to=unique(synonymes_simple_dt$to)%>%sort


library(stringr)
usage_tiret=str_extract(words_from,pattern = "-([A-z]+)$")
usage_tiret=usage_tiret%>%na.omit%>%table%>%sort(decreasing = T)
# dur à gérer parce que parfois c'est des expressions, parfois des formes inclusives

to_remove=c("-e","-trice","-se","-ne","-euse")
str_to_fix=rep("",times=length(to_remove))
names(str_to_fix) <- to_remove
words_from <- str_replace_all(words_from,str_to_fix)
words_to <- str_replace_all(words_to,str_to_fix)

words=unique(c(words_from,words_to))
words=tolower(words)
matching=pbapply::pblapply(words,function(term){
  str_which(corpus,fixed(term))})

names(matching) <- words
save(matching,file="synonymes/synonymes_DES.RData")
matching_df=stack(lapply(matching,length))
names(matching_df) <- c("nb","synonyme")
matching_df$synonyme=as.character(matching_df$synonyme)
matching_df=data.table(matching_df)
matching_sub=matching_df[nb>0]
matching_sub$regex=paste0("(^| )(",matching_sub$synonyme,")( |$)")

matching_precis=pbapply::pblapply(matching_sub$regex,function(term){
  str_which(corpus,term)})
names(matching_precis) <- matching_sub$synonyme
save(matching_precis,file="synonymes/synonymes_precis_DES.RData")
matching_df=stack(lapply(matching_precis,length))
names(matching_df) <- c("nb","synonyme")
matching_df$synonyme=as.character(matching_df$synonyme)
matching_df=data.table(matching_df)
matching_sub=matching_df[nb>0]

names(matching_sub) <- c("freqa","word1")
utiles=merge(synonymes_simple_dt,matching_sub,by.x="from",by.y="word1")
names(matching_sub) <- c("freqb","word2")
utiles=merge(utiles,matching_sub,by.x="to",by.y="word2")

utiles <- utiles%>% 
  rowwise() %>%
  mutate(word1=sort(c(to,from))[1],
         word2=sort(c(to,from))[2],
         freq1=pmax(freqa,freqb),
         freq2=pmin(freqa,freqb))%>%
  select(word1,word2,freq1,freq2)%>%
  distinct()%>%
  mutate(freq=freq1+freq2)%>%
  arrange(-freq)%>%
  select(-freq)
View(utiles)
openxlsx::write.xlsx(utiles,file = "synonymes/synonymes_DES_utiles.xlsx")
