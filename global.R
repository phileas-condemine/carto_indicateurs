# library(reactlog)
library(dplyr)
library(tidyr)
library(data.table)
library(DT)
library(shiny)
library(shinyWidgets)
# library(V8)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(stringi)
library(stringr)
library(markdown)
library(shinycssloaders)
library(bsplus)
library(slickR)
library(plotly)
library(sunburstR)
library(igraph)
library(networkD3)
library(tidytext)
library(wordcloud)
library(clipr)
library(rclipboard)
library(readxl)
library(slackr)
library(mailR)
library(rdrop2)
# token <- drop_auth()
# saveRDS(token, "www/droptoken.rds")
token <- readRDS("www/droptoken.rds")

# Then pass the token to each drop_ function
drop_acc(dtoken = token)
slackr_setup(config_file = "www/slackr_config2.txt",echo = F)

options(shiny.reactlog=TRUE)

############# DATA PREP #############
# source("utils/data_prep.R",local = T)
############

# default global search value
if (!exists("default_search")) default_search <- ""
# default column search values
if (!exists("default_search_columns")) default_search_columns <- NULL

tags_class=fread("data/classif_tags.csv",encoding="UTF-8")


refresh=F
if (refresh){
  source("utils/data_prep.R")
}
rdrop2::drop_download("init_data2.RData","data/",overwrite = T,dtoken = token)
load("data/init_data2.RData")

init_vars_to_show=
  c("Base","Indicateur",
    # "Famille",
    # "Source",
    "Producteurs"
    # ,"Classement producteur Niveau 3"
    # ,"Classement producteur Niveau 2"
    # ,"Classement producteur Niveau 1"
  )








index[,random_order:=sample(.N)]
setorder(index,random_order)

source_readme="https://raw.githubusercontent.com/phileas-condemine/carto_indicateurs/master/readme.md"


rdrop2::drop_download("term_freq.RData","data/",overwrite = T,dtoken = token)
load("data/term_freq.RData")

term_freq=full_text_split[,list(freq=.N),by="word"]
term_freq_global=term_freq[!word%in%stopwords_vec]
term_freq_global$hash=openssl::md5(term_freq_global$word)%>%substr(1,7)

setorder(term_freq_global,-freq)

source("utils/send_mail.R",local = T)


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
      nb=length(unique(my_data$Producteurs))
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
      nm=strsplit(as.character(my_data$Producteurs),",")%>%unlist%>%table%>%sort(decreasing=T)%>%head(1)%>%names
      box1<-valueBox(value=nm,
                     subtitle = "Principal",
                     icon = icon("star"),
                     color = "blue",href=paste0("https://search.lilo.org/searchweb.php?q=",nm)#%>%URLencode
      )
      # box1$children[[1]]$attribs$class <- "action-button"
      box1$children[[1]]$attribs$id <- "valuebox_prod_principal"
      box1$children[[1]]$attribs$target <- "blank"
      
      return(box1)#https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
      
    })
    output$nb_tags=renderValueBox({
      
      # nm=tag_pred[index%in%my_data$index,
      #             c("tag1","tag2","tag3")]%>%
      #   unlist()%>%
      #   # {c(.$tag1,.$tag2,.$tag3)}%>%
      #   unique()%>%length()
      nm=tag_pred[index%in%my_data$index,
                  tolower(tag_names),with=F]%>%
        colSums()
      nm = nm[nm>0]
      nm = length(nm)
      box1<-valueBox(value=nm,
                     # sapply(prod_acro,function(x)sum(grepl(pattern = x,my_data$Producteur)))%>%sort(decreasing = T)%>%head(1)%>%names
                     # IL Y A QQCH A FAIRE AUTOUR DE overflow: auto; https://www.w3schools.com/css/css_align.asp
                     subtitle = "Thématique(s)",
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


########### CONNEXION MONGODB ###########
source("utils/mongo_db_connection.R",local = T)



########## CUSTOM JS for DT ############

custom_DT=JS(
  "function(settings, json) {",
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


######### LOAD CAROUSEL #########

source("utils/create_carousel.R",local=T)

# Change l'ordre des thèmes

tags_class_list=tags_class_list[c(3,4,2,1)]


rdrop2::drop_download("cooc.RData","data/",overwrite = T,dtoken = token)
load("data/cooc.RData")
# 
# bookmark.modal.js = "$('.ui.mini.modal')
# .modal({
#     blurring: false
# })
# $('#bookmark_modal').modal('show')
# ;"

