# rsconnect::deployApp(appFileManifest = "manifest.txt",
#                      appName = "Cartographie_des_indicateurs",
#                      launch.browser = F,
#                      account = "drees",forceUpdate = T
#                       )
library(dplyr)
library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(stringr)
library(markdown)

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
# 
# save(prod_acro,index,file="index_acro.RData")


load("index_acro.RData")
table(index$Producteur)

# load("data/indicateurs_a_tagger.RData")
# tag_pred=indicateurs_pred[,c("indic_id","tag1","tag2","tag3"),with=F]
# setnames(tag_pred,"indic_id","index")
# tag_names=unique(c(as.character(tag_pred$tag1),
#          as.character(tag_pred$tag2),
#          as.character(tag_pred$tag3)))
# tag_pred$tag1=as.character(tag_pred$tag1)
# tag_pred$tag2=as.character(tag_pred$tag2)
# tag_pred$tag3=as.character(tag_pred$tag3)
# save(tag_pred,tag_names,file="tag_pred.RData")

load("tag_pred.RData")

source_readme="https://raw.githubusercontent.com/phileas-condemine/carto_indicateurs/master/readme.md"

# full_text=pbapply::pbapply(index,1,paste,collapse=" ")
# save(full_text,file="full_text.RData")
load("data/full_text.RData")
head(index)


custom_DT=JS(
  "function(settings, json) {",
  "console.log(this.api());",
  "$('.dataTables_filter').css({'float':'inherit'",
  ",'text-align':'center','vertical-align':'middle'});",
  "$('.dataTables_filter input').wrap('<div class=&quot;search-bar-seule&quot; id=&quot;search-bar-seule&quot; ></div>');",
  "$('input[type=search]').removeClass().addClass('selectize-input');",
  # "$('.dataTables_length').children().appendTo('div.dataTables_filter')
                                     # .css({'float':'inherit','text-align':'right','vertical-align':'middle'})",
  # "$('.form-group.shiny-input-container').children().appendTo('.dataTables_filter')",
  "function setTextContents($elem, text) {",
  "  $elem.contents().filter(function() {",
  "    if (this.nodeType == Node.TEXT_NODE) {",
  "      this.nodeValue = text;",
  "    }",
  "  });",
  "}",
  "setTextContents($('.dataTables_filter label'), 'Recherche par mot(s) clef(s)');",
  "}")

vars_to_show=c("Base","Indicateur","Famille","Source","Producteur","Fréquence d'actualisation")


my_value_boxes <- function(input,output,session,loaded_data,subset_rows){
observe({
  if (!is.null(subset_rows())){
  my_data=loaded_data()[subset_rows()]#ajustement aux filtres de DT
}else my_data=loaded_data()#cas de base
  output$nb_indicateurs=renderValueBox({
    valueBox(value=nrow(my_data),subtitle = "Nombre d'indicateurs",
             icon = icon("area-chart"),
             color = "green")
  })
  output$nb_bases=renderValueBox({
    valueBox(value=length(unique(my_data$Base)),subtitle = "Nombre de bases",
             icon = icon("diamond"),
             color = "orange")
  })
  output$nb_prod=renderValueBox({
    valueBox(value=length(unique(my_data$Producteur)),subtitle = "Nombre de producteurs",
             icon = icon("group"),
             color = "green")
  })
  output$nb_sources=renderValueBox({
    valueBox(value=length(unique(my_data$Source)),subtitle = "Nombre de sources",
             icon = icon("diamond"),
             color = "orange")
  })
  output$prod_ppal=renderValueBox({
    valueBox(value=
               strsplit(my_data$fixed_prod,",")%>%unlist%>%table%>%sort(decreasing=T)%>%head(1)%>%names
               # sapply(prod_acro,function(x)sum(grepl(pattern = x,my_data$Producteur)))%>%sort(decreasing = T)%>%head(1)%>%names
             # IL Y A QQCH A FAIRE AUTOUR DE overflow: auto; https://www.w3schools.com/css/css_align.asp
             ,subtitle = "Producteur majoritaire",
             icon = icon("star"),
             color = "blue")
    
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
    valueBoxOutput(ns("prod_ppal"),width = 2)
  )
  return(f)
}





# jsCode <- "shinyjs.searchcontent = function() {
#   $('.dataTables_filter input').unbind().keyup( function() {
#   var value = $(this).val();
#   console.log(value);
#   return(value); // <-- the value
#   });}"