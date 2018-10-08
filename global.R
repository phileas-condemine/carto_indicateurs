# rsconnect::deployApp(appFileManifest = "manifest.txt",
#                      appName = "Cartographie_des_indicateurs",
#                      launch.browser = F,
#                      account = "drees",forceUpdate = T
#                       )

library(data.table)
library(DT)
library(shinyWidgets)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinydashboard)
# tags_applied=fread("data/3105_Index des indicateurs tagges.csv",encoding = "Latin-1")
index=fread("data/29032018_Index2.csv",encoding = "Latin-1")
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


# full_text=pbapply::pbapply(index,1,paste,collapse=" ")
# save(full_text,file="full_text.RData")
load("data/full_text.RData")
head(index)


custom_DT=JS(
  "function(settings, json) {",
  "console.log(this.api());",
  "$('.dataTables_filter').css({'float':'inherit'",
  ",'text-align':'center','vertical-align':'middle'});",
  "$('.dataTables_filter input').wrap('<div class=&quot;search-bar-seule&quot; ></div>');",
  "$('input[type=search]').removeClass().addClass('selectize-input');",
  "function setTextContents($elem, text) {",
  "  $elem.contents().filter(function() {",
  "    if (this.nodeType == Node.TEXT_NODE) {",
  "      this.nodeValue = text;",
  "    }",
  "  });",
  "}",
  "setTextContents($('.dataTables_filter label'), 'Recherche par mot(s) clef(s)');",
  "}")

vars_to_show=c("Base","Indicateur","Famille","Source","Producteur")





# jsCode <- "shinyjs.searchcontent = function() {
#   $('.dataTables_filter input').unbind().keyup( function() {
#   var value = $(this).val();
#   console.log(value);
#   return(value); // <-- the value
#   });}"