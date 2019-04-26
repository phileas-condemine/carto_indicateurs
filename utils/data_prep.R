tags_applied=fread("data/3105_Index des indicateurs tagges.csv",encoding = "Latin-1")


index=fread("data/29032018_Index2.csv",encoding = "Latin-1")


acro_extraction=function(nm){
  unlist(pbapply::pblapply(strsplit(nm,","),
                    function(x)paste(lapply(x,function(y){
    xtrct=str_extract(y,pattern = "(\\()(([A-z]|é|è|Î|à| |\\-)+)(\\))")
    xtrct=gsub("\\(","",xtrct)
    xtrct=gsub("\\)","",xtrct)
    ifelse(is.na(xtrct),y,xtrct)
  }),collapse=",")))
# table(fixed_prod)
}
index$Producteurs=acro_extraction(index$Producteur)
index$Source=acro_extraction(index$Source)
index$Base=acro_extraction(index$Base)






# producteurs=unique(unlist(strsplit(index$Producteur,",")))
# producteurs_acronymes=str_extract(string = producteurs,pattern = "(\\()([A-z]+)(\\))")
# producteurs_acronymes=gsub("\\(","",producteurs_acronymes)
# producteurs_acronymes=gsub("\\)","",producteurs_acronymes)
# Producteurs=data.table(Producteur=producteurs,Producteur_acronyme=producteurs_acronymes)
# Producteurs[is.na(Producteur_acronyme),Producteur_acronyme:=Producteur]
# prod_acro=unique(Producteurs$Producteur_acronyme)
# set.seed(1)
index=index[,-1]

names(index) <- enc2utf8(names(index))
table(index$Producteur)
index=index%>%mutate_if(is.character,factor)%>%data.table
index$random_order=sample(nrow(index))
setorder(index,random_order)
load("data/indicateurs_a_tagger.RData")
tag_pred=indicateurs_pred[,c("indic_id","tag1","tag2","tag3"),with=F]
setnames(tag_pred,"indic_id","index")
tag_names=unique(c(as.character(tag_pred$tag1),
         as.character(tag_pred$tag2),
         as.character(tag_pred$tag3)))
tags_class=fread("data/classif_tags.csv")
tags_class$alias=iconv(tags_class$alias,to="UTF-8")
tags_class_vec=c(tags_class$valeur)
names(tags_class_vec) <- tags_class$alias

loc_sep=which(tags_class_vec=="")
loc_sep=c(loc_sep,"END"=nrow(tags_class)+1)
size_classes=diff(loc_sep)-1
names(size_classes)<- names(loc_sep)[1:(length(loc_sep)-1)]
class_for_split=rep.int(names(size_classes),size_classes)
tags_class_vec=tags_class_vec[!tags_class_vec==""]
tags_class_list=split(tags_class_vec,class_for_split)

# tag_pred$tag1=as.character(tag_pred$tag1)
# tag_pred$tag2=as.character(tag_pred$tag2)
# tag_pred$tag3=as.character(tag_pred$tag3)
tag_pred=tag_pred%>%mutate_at(c("tag1","tag2","tag3"),as.character)%>%data.table

for (i in which(sapply(index,is.factor))){
  print(i)
  table(Encoding(levels(index[[i]])))
  levels(index[[i]]) <- iconv(levels(index[[i]]),"latin1","UTF-8")
  index[[i]] <- as.character(index[[i]])
  index[[i]] <- enc2native(index[[i]])
  table(Encoding(index[[i]]))

}

save(tag_pred,
     # prod_acro,
     index,tags_class_list,tag_names,file="data/init_data.RData")

# full_text=pbapply::pbapply(index,1,paste,collapse=" ")
# save(full_text,file="data/full_text.RData")


init_vars_to_show=
  c("Base","Indicateur",
    # "Famille",
    # "Source",
    "Producteur"
    # ,"Classement producteur Niveau 3"
    # ,"Classement producteur Niveau 2"
    # ,"Classement producteur Niveau 1"
  )


full_text=pbapply::pbapply(index[,init_vars_to_show,with=F],1,paste,collapse=" ")

full_text=tolower(full_text)
full_text=gsub(x = full_text,
                          pattern = "[[:punct:]]",
                          replacement=" ")
full_text=tm::stripWhitespace(full_text)
full_text_split=str_split(full_text,pattern = " ")
names(full_text_split) <- paste0(index$index,"_")
full_text_split=unlist(full_text_split)
full_text_split=data.table(index=names(full_text_split),
                           word=unname(full_text_split))
full_text_split$index=stringr::str_extract(full_text_split$index,"^.+_")
full_text_split$index=substr(full_text_split$index,1,str_length(full_text_split$index)-1)


full_text_split=unique(full_text_split)
full_text_split=full_text_split[!grepl(pattern = "^(([0-9])|( ))+$",
                                       full_text_split$word)]
stopwords_vec=stopwords::stopwords(language = "fr")
stopwords_vec=c(stopwords_vec,"na"," ","")
full_text_split$word <- enc2native(full_text_split$word)
save(stopwords_vec,full_text_split,file="data/term_freq.RData")


### PREP NETWORK PRODUCTEURS

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
