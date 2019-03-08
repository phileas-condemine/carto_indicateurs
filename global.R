# rsconnect::deployApp(appFileManifest = "manifest.txt",
#                      appName = "Cartographie_des_indicateurs",
#                      launch.browser = F,
#                      account = "drees",forceUpdate = T
#                       )


options(shiny.reactlog=TRUE) 
library(dplyr)
library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(V8)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(stringr)
library(markdown)
library(shinycssloaders)
# library(styler)
# library(shinyAce)
# library(shinyjqui)
# library(shinydashboardPlus)

############# DATA PREP #############
# tags_applied=fread("data/3105_Index des indicateurs tagges.csv",encoding = "Latin-1")
# 
# 
# index=fread("data/29032018_Index2.csv",encoding = "Latin-1")
# 
# fixed_prod=unlist(pbapply::pblapply(strsplit(index$Producteur,","),
#                   function(x)paste(lapply(x,function(y){
#   xtrct=str_extract(y,pattern = "(\\()(([A-z]|é|è|Î|à| |\\-)+)(\\))")
#   xtrct=gsub("\\(","",xtrct)
#   xtrct=gsub("\\)","",xtrct)
#   ifelse(is.na(xtrct),y,xtrct)
# }),collapse=",")))
# 
# table(fixed_prod)
# index$fixed_prod=fixed_prod
# 
# producteurs=unique(unlist(strsplit(index$Producteur,",")))
# producteurs_acronymes=str_extract(string = producteurs,pattern = "(\\()([A-z]+)(\\))")
# producteurs_acronymes=gsub("\\(","",producteurs_acronymes)
# producteurs_acronymes=gsub("\\)","",producteurs_acronymes)
# Producteurs=data.table(Producteur=producteurs,Producteur_acronyme=producteurs_acronymes)
# Producteurs[is.na(Producteur_acronyme),Producteur_acronyme:=Producteur]
# prod_acro=unique(Producteurs$Producteur_acronyme)
# set.seed(1)
# index=index[,-1]
# 
# names(index) <- enc2utf8(names(index))
# table(index$Producteur)
# index=index%>%mutate_if(is.character,factor)%>%data.table
# index$random_order=sample(nrow(index))
# setorder(index,random_order)
# load("data/indicateurs_a_tagger.RData")
# tag_pred=indicateurs_pred[,c("indic_id","tag1","tag2","tag3"),with=F]
# setnames(tag_pred,"indic_id","index")
# tag_names=unique(c(as.character(tag_pred$tag1),
#          as.character(tag_pred$tag2),
#          as.character(tag_pred$tag3)))
# tags_class=fread("data/classif_tags.csv")
# tags_class$alias=iconv(tags_class$alias,to="UTF-8")
# tags_class_vec=c(tags_class$valeur)
# names(tags_class_vec) <- tags_class$alias
# 
# loc_sep=which(tags_class_vec=="")
# loc_sep=c(loc_sep,"END"=nrow(tags_class)+1)
# size_classes=diff(loc_sep)-1
# names(size_classes)<- names(loc_sep)[1:(length(loc_sep)-1)]
# class_for_split=rep.int(names(size_classes),size_classes)
# tags_class_vec=tags_class_vec[!tags_class_vec==""]
# tags_class_list=split(tags_class_vec,class_for_split)
# 
# tag_pred$tag1=as.character(tag_pred$tag1)
# tag_pred$tag2=as.character(tag_pred$tag2)
# tag_pred$tag3=as.character(tag_pred$tag3)
# 
# save(tag_pred,prod_acro,index,tags_class_list,tag_names,file="init_data.RData")
############## 

# default global search value
if (!exists("default_search")) default_search <- ""
# default column search values
if (!exists("default_search_columns")) default_search_columns <- NULL


load("init_data.RData")

source_readme="https://raw.githubusercontent.com/phileas-condemine/carto_indicateurs/master/readme.md"

# full_text=pbapply::pbapply(index,1,paste,collapse=" ")
# save(full_text,file="full_text.RData")
load("data/full_text.RData")


init_vars_to_show=
  c("Base","Indicateur",
    # "Famille",
    # "Source",
    "fixed_prod"
  # ,"Classement producteur Niveau 3"
  # ,"Classement producteur Niveau 2"
  # ,"Classement producteur Niveau 1"
               )
# 
# full_text=pbapply::pbapply(index[,init_vars_to_show,with=F],1,paste,collapse=" ")
# 
# full_text=tolower(full_text)
# full_text=gsub(x = full_text,
#                           pattern = "[[:punct:]]",
#                           replacement=" ")
# full_text=tm::stripWhitespace(full_text)
# full_text_split=str_split(full_text,pattern = " ")
# names(full_text_split) <- paste0(index$index,"_")
# full_text_split=unlist(full_text_split)
# full_text_split=data.table(index=names(full_text_split),
#                            word=unname(full_text_split))
# full_text_split$index=stringr::str_extract(full_text_split$index,"^.+_")
# full_text_split$index=substr(full_text_split$index,1,str_length(full_text_split$index)-1)
# 
# 
# full_text_split=unique(full_text_split)
# full_text_split=full_text_split[!grepl(pattern = "^(([0-9])|( ))+$",
#                                        full_text_split$word)]
# stopwords_vec=stopwords::stopwords(language = "fr")
# stopwords_vec=c(stopwords_vec,"na"," ","")
# save(stopwords_vec,full_text_split,file="term_freq.RData")
load("term_freq.RData")
term_freq=full_text_split[,list(freq=.N),by="word"]
term_freq_global=term_freq[!word%in%stopwords_vec]
setorder(term_freq_global,-freq)
# term_freq

my_value_boxes <- function(input,output,session,loaded_data,subset_rows){
observe({
  if (!is.null(subset_rows())){
  my_data=loaded_data()[subset_rows()]#ajustement aux filtres de DT
  
}else my_data=loaded_data()#cas de base



  output$nb_indicateurs=renderValueBox({
    nb=nrow(my_data)
    box1<-valueBox(value=nb,subtitle = ifelse(nb<=1," Indicateur"," Indicateurs"),
             icon = icon("area-chart"),
             color = "green",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_indicateurs"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
    
  })
  output$nb_bases=renderValueBox({
    nb=length(unique(my_data$Base))
    box1<-valueBox(value=nb,subtitle = ifelse(nb<=1," Base"," Bases"),
             icon = icon("cube"),
             color = "orange",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_bases"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
  })
  output$nb_prod=renderValueBox({
    nb=length(unique(my_data$Producteur))
    box1<-valueBox(value=nb,subtitle = ifelse(nb<=1," Producteur"," Producteurs"),
             icon = icon("group"),
             color = "green",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_producteurs"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
    
  })

  output$nb_sources=renderValueBox({
    nb=length(unique(my_data$Source))
    box1<-valueBox(value=nb,subtitle = ifelse(nb<=1," Source"," Sources"),
             icon = icon("database"),
             color = "orange",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_sources"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
    
  })
  output$prod_ppal=renderValueBox({
    nm=strsplit(as.character(my_data$fixed_prod),",")%>%unlist%>%table%>%sort(decreasing=T)%>%head(1)%>%names
    box1<-valueBox(value=nm,
              subtitle = "Favori",
             icon = icon("star"),
             color = "blue",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_prod_principal"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
    
  })
  output$nb_tags=renderValueBox({
    nm=tag_pred[index%in%my_data$index,
                c("tag1","tag2","tag3")]%>%
      unlist()%>%
      # {c(.$tag1,.$tag2,.$tag3)}%>%
      unique()%>%length()
    box1<-valueBox(value=nm,
             # sapply(prod_acro,function(x)sum(grepl(pattern = x,my_data$Producteur)))%>%sort(decreasing = T)%>%head(1)%>%names
             # IL Y A QQCH A FAIRE AUTOUR DE overflow: auto; https://www.w3schools.com/css/css_align.asp
             subtitle = "Tags",
             icon = icon("filter"),
             color = "blue",href='#')
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"valuebox_nb_tags"
    return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
    
  })
  })
}

my_value_boxesUI <- function(id) {
  ns <- NS(id)
  f <- fluidRow(
    valueBoxOutput(ns("nb_indicateurs"),width = 2),
    valueBoxOutput(ns("nb_bases"),width = 2),
    valueBoxOutput(ns("nb_prod"),width = 2),
    valueBoxOutput(ns("nb_sources"),width = 2),
    valueBoxOutput(ns("prod_ppal"),width = 2),
    valueBoxOutput(ns("nb_tags"),width = 2)
  )
  return(f)
}





custom_DT=JS(
  "function(settings, json) {",
  # "console.log(this.api());",
  # "$('.dataTables_filter').wrap('<div').appendTo('#sidebarItemExpanded')",
  # "$('.dataTables_filter').css({'float':'inherit'",
  # ",'text-align':'center','vertical-align':'middle'});",
  # "$('.dataTables_filter input').wrap('<div class=&quot;search-bar-seule&quot; id=&quot;search-bar-seule&quot; ></div>');",
  # "$('input[type=search]').removeClass().addClass('selectize-input');",
  # "$('#sidebarItemExpanded > .dataTables_filter').remove();",
  # "$('#sidebarItemExpanded > .dataTables_length').remove();",
  # "$('.dataTables_filter').addClass('form-group shiny-input-container').appendTo('#sidebarItemExpanded');",
  # "$('.dataTables_length').addClass('form-group shiny-input-container').appendTo('#sidebarItemExpanded');",
  #MODIFICATION DU TEXTE DU SELECT DE LA LONGUEUR DE LA TABLE AFFICHEE
# "function replaceNodeText(){
#   console.log(this.nodeType);
#   if (this.nodeType == 3){
#     this.nodeValue = this.nodeValue.replace(replaceNodeText.find,replaceNodeText.replace);
#       } else {
#     $(this).contents().each(replaceNodeText);
#     }
#   }
#   replaceNodeText.find='Show';
#   replaceNodeText.replace='Afficher';
# $('.dataTables_length').contents().each(replaceNodeText);
#     replaceNodeText.find='entries';
#   replaceNodeText.replace='indicateurs';",
# "$('.dataTables_length').contents().each(replaceNodeText);",
# "$('select.shinyjs-resettable').css({'color':'#333'});",
# "$('.dataTables_paginate').insertAfter('.dataTables_scroll');",

# A MODIFIER POUR QUE CA MARCHE
# "$('th.sorting').parent().after('<tr> `yo` <\tr>')",

  # ".css({'float':'inherit','text-align':'right','vertical-align':'middle'})",
  # "$('.form-group.shiny-input-container').children().appendTo('.dataTables_filter')",
#MODIFICATION DU TITRE :
  "function setTextContents($elem, text) {",
  "  $elem.contents().filter(function() {",
  "    if (this.nodeType == Node.TEXT_NODE) {",
  "      this.nodeValue = text;",
  "    }",
  "  });",
  "}",
  "setTextContents($('.dataTables_filter label'), 'Recherche par mot(s) clef(s)');",
  "}"


)










