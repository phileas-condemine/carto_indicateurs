library(data.table)
library(stringr)
library(dplyr)
library(readxl)
library(tidyr)
library(dplyr)
library(rdrop2)
# token <- drop_auth()
# saveRDS(token, "www/droptoken.rds")
token <- readRDS("www/droptoken.rds")

# Then pass the token to each drop_ function
drop_acc(dtoken = token)
from_gs=T
update_cooc = T
update_termfreq = T

vars_to_edit = c("Base","Indicateur",
                 "Famille",
                 "Source",
                 "Producteurs"
                 ,"Classement_producteur_Niveau_3__"
                 ,"Classement_producteur_Niveau_2"
                 ,"Classement_producteur_Niveau_1__"
)

remove_accents=function(x)x%>%stringi::stri_trans_general("Latin-ASCII")%>%iconv(to="UTF-8")

if (from_gs){
  download.file("https://docs.google.com/spreadsheets/d/1eexvhXHK9c-EbpegU8lL0xWlqRB6XhDKj_MxO4pGORQ/export?format=xlsx",
                "data/latest_indicateurs.xlsx",mode="wb")
  
  data <- read_excel("data/latest_indicateurs.xlsx")
  index <- data%>%select(-contains("tag_"))%>%
    arrange(index)%>% 
    mutate_at(vars_to_edit,remove_accents)%>% 
    mutate_at(vars_to_edit,toupper)%>%
    data.table()
  
  # sum(rowSums(is.na(data[,c(paste0("tag_",1:11)),with=F]))==11)
  
  
  tag_pred <- data%>%
    select(index,contains("tag_"))%>%
    mutate_all(funs(ifelse(is.na(.),"",.)))%>%
    unite(tags,-index,sep=",")%>%
    mutate(tags=gsub(",+",",",tags),tags=gsub(",$","",tags))%>%
    separate_rows(tags,sep=",")%>%
    mutate(value=1)%>%
    dcast(index~tags,value.var="value")%>%select(-Var.2)%>%
    mutate_all(funs(ifelse(is.na(.),0,.)))%>%
    arrange(index)%>%
    data.table()
  tag_names = names(tag_pred)[-1]
}

tags_class=fread("data/classif_tags.csv",encoding="UTF-8")

tags_class_vec=c(tags_class$valeur)
names(tags_class_vec) <- tags_class$alias

loc_sep=which(tags_class_vec=="")
loc_sep=c(loc_sep,"END"=nrow(tags_class)+1)
size_classes=diff(loc_sep)-1
names(size_classes)<- names(loc_sep)[1:(length(loc_sep)-1)]
class_for_split=rep.int(names(size_classes),size_classes)
tags_class_vec=tags_class_vec[!tags_class_vec==""]
tags_class_list=split(tags_class_vec,class_for_split)

save(tag_pred,tags_class_vec,
     index,tags_class_list,tag_names,file="data/init_data2.RData")

rdrop2::drop_upload(file = "data/init_data2.RData",dtoken = token)



if(update_termfreq){
  
  init_vars_to_show=
  c("Base","Indicateur",
    "Producteurs"
  )
  full_text = do.call("paste",index[,init_vars_to_show,with=F])
  full_text=toupper(full_text)
  
  full_text=gsub(x = full_text,
                 pattern = "[[:punct:]]",
                 replacement=" ")
  full_text=tm::removePunctuation(full_text)
  full_text=tm::removeNumbers(full_text)

  full_text=tm::stripWhitespace(full_text)
  full_text_split=str_split(full_text,pattern = " ")
  names(full_text_split) <- paste0(index$index,"_")
  full_text_split=unlist(full_text_split)
  full_text_split=data.table(index=names(full_text_split),
                             word=unname(full_text_split))
  full_text_split$index=stringr::str_extract(full_text_split$index,"^.+_")
  full_text_split$index=gsub("_","",full_text_split$index)
  # full_text_split$index=substr(full_text_split$index,1,str_length(full_text_split$index)-1)
  full_text_split=unique(full_text_split)
  full_text_split=full_text_split[!grepl(pattern = "^(([0-9])|( ))+$",
                                         full_text_split$word)]
  full_text_split[,word:=factor(word)]
  words=levels(full_text_split$word)
  words_lower=tolower(words)
  words_freq = full_text_split[,.(freq=.N),by=word]
  words_freq[,word:=tolower(word)]
  words_stem = data.table(word=words,word_lower=words_lower,
                          word_stem=tm::stemDocument(words_lower,"fr"),
                          word_len = nchar(words_lower),ordre_levels=1:length(words))
  words_stem = merge(words_stem,words_freq,by.x="word_lower",by.y="word")
  setorder(words_stem,word_len,-freq)
  # Une façon de récupérer la forme la plus courte et parmi les candidates on prend la plus fréquente
  words_stem[,word_fix:=word[1],by="word_stem"]
  setorder(words_stem,ordre_levels)
  full_text_split = merge(full_text_split,words_stem[,c("word","word_stem","word_fix")],by="word")
  full_text_split[,word:=NULL]
  setnames(full_text_split,"word_fix","word")
  # levels(full_text_split$word) <- toupper(words_stem$word_fix)
  full_text_split[,word:=as.character(toupper(word))]
  full_text_split[,freq:=.N,by="word"]
  full_text_split=full_text_split[freq>2]
  full_text_split = full_text_split[,word_stem:=toupper(word_stem)]
  full_text_split = full_text_split[word!=""]
  full_text_split[,freq:=NULL]
  
  
  stopwords_vec=stopwords::stopwords(language = "fr")
  stopwords_vec=c(stopwords_vec,"na"," ","","a")%>%toupper()
  # full_text_split$word <- enc2native(full_text_split$word)
  save(stopwords_vec,full_text_split,file="data/term_freq.RData")
  rdrop2::drop_upload(file = "data/term_freq.RData",dtoken = token)
  
}

### PREP NETWORK PRODUCTEURS

if(update_cooc){
  cooc_from_vec=function(vec){
    vec[vec==""]<-"Inconnu"
    vec=strsplit(vec,",")
    
    
    vec=pbapply::pblapply(vec,function(x){
      res<-rep(1,length(x))
      res<-matrix(res,ncol=length(x))
      colnames(res)<-x
      res<-data.table(res)
      res
    })
    vec=rbindlist(vec,fill=T)
    vec$index=index$index
    vec_sp=melt(vec,id.vars = "index",variable.factor = F)
    vec_sp=na.omit(vec_sp)
    vec_sp$value=NULL
    cooc=merge(vec_sp,vec_sp,by="index",allow.cartesian = T)
    cooc
  }
  
  cooc_producteurs=cooc_from_vec(index$Producteurs)
  cooc_source=cooc_from_vec(index$Source)
  save(cooc_producteurs,cooc_source,file="data/cooc.RData")
  rdrop2::drop_upload(file = "data/cooc.RData",dtoken = token)
  
}