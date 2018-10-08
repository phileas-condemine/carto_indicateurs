function(input,output,session){
  to_plot=reactiveVal(index)
  tags_reac=reactiveVal()
  output$DT_to_render=renderDataTable(to_plot()[,vars_to_show,with=F],
                        options = list(searchHighlight = TRUE,
                                                  dom = "ftipr", 
                                                  initComplete = custom_DT),
                                       selection = 'single',rownames=F)
  # search_content=js$searchcontent()
  observeEvent(input$DT_to_render_cell_clicked,{
    if(length(input$DT_to_render_cell_clicked)>0){
    clicked=input$DT_to_render_cell_clicked
    row_clicked=clicked$row
    content=to_plot()[row_clicked]
    content_tags=tag_pred[index==content$index]
    tags_reac(c(content_tags$tag1,content_tags$tag2,content_tags$tag3))
    print(content$index)
    showModal(modalDialog(
        fluidRow(style="color:#0253a3;text-align:center; margin-top:-15px;font-size:large;background-image: linear-gradient(to bottom,#f5f5f5 0,#e8e8e8 100%);",content$Indicateur),
        fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                 column(6,{
          HTML(paste("<b>Base :</b>",content$Base,"<br>",
                     "<b>Producteur de la base :</b>",content$Producteur,"<br>"))
        }),column(6,{
          HTML(paste("<b>Famille :</b>",content$Famille,"<br>",
                     "<b>Source :</b>",content$Source,"<br>"))
        })),
          fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                   column(9,align="center",style="align:center;display:inline-block;",HTML(
                  paste0("<ul style='text-align: center;list-style: inside;'>
                     <li> Aller sur le site proposant l'indicateur en cliquant sur le bouton à droite",
                     ifelse(!content$`Classement producteur Niveau 1`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 1`),""),
                     ifelse(!content$`Classement producteur Niveau 2`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 2`),""),
                     ifelse(!content$`Classement producteur Niveau 3`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 3`),""),
                   "</ul>"))),
                   column(3,align="center",style="text-align:center;  position: relative;",
                      actionButton("go","Accéder à la base",#icon=icon("th")
                                   onclick =paste0("window.open('",content$hyperlien,"', '_blank')")))
        )
         ,footer = tagList(
          actionButton("tag1",label = content_tags$tag1),
          actionButton("tag2",label = content_tags$tag2),
          actionButton("tag3",label = content_tags$tag3)
          
         )
      ,easyClose =T,size="l"))
    }
  })  
  observeEvent(c(input$tag1,input$tag2,input$tag3),{
    tags_clicked=c(input$tag1%%2,input$tag2%%2,input$tag3%%2)
    tags_clicked=which(tags_clicked==1)
    currently_selected=input$tag
    new_selection=c(currently_selected,tags_reac()[tags_clicked])
    if(length(tags_clicked)>0){
      removeUI(selector = ".shiny-input-container",multiple = F,session = session,immediate = F)
      insertUI(session = session,selector = ".tagbar",where = "beforeBegin",ui=selectInput(inputId = "tag",
                      label = "Recherche par tags",choices = tag_names,
                      selected = new_selection,multiple = T),multiple = F,immediate = F)
    }
  })
  observeEvent(input$tag,{
    text=input$DT_to_render_search#on veut conserver le texte de recherche !
    recherche=input$tag
    to_plot(index)
    if (!is.null(recherche)){
      # print(recherche)
      # which_to_keep=(rowSums(tags[,recherche,with=F])==length(recherche))
      # index_to_keep=tags[which_to_keep]$index
      # tag_names
      which_to_keep=rowSums(data.table(tag_pred$tag1%in%recherche,tag_pred$tag2%in%recherche,tag_pred$tag3%in%recherche))==length(recherche)
      index_to_keep=tag_pred[which_to_keep]$index
      
      to_plot(to_plot()[index%in%index_to_keep])
      output$DT_to_render=renderDataTable(to_plot()[,vars_to_show,with=F],options = 
                              list(searchHighlight = TRUE,
                                    dom="ftipr", 
                                    initComplete = custom_DT,
                                    search=list(search=text)),
                              selection = 'single',rownames=F)
    }
  })
}