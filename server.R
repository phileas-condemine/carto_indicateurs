function(input,output,session){
  
  
  
  toggleModal(session, "startupModal", toggle = "open")
  
  to_plot=reactiveVal(index)
  tags_reac=reactiveVal()
  # onclick()
  output$DT_to_render=renderDT(to_plot()[,input$vars_to_show,with=F], 
          filter="top",
          extensions = 'Buttons',
          options = list(searchHighlight = TRUE,
                            language = list(
                              info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                              paginate = list(previous = 'Précédent', `next` = 'Suivant')
                            ),
                            dom = "lftiprB",#"tirB"
                            # initComplete = custom_DT,
                         scrollY=T,
                            pageLength = 30
                            ,buttons = c('copy', 'csv', 'excel')
                            ,columnDefs = list(list(
                              targets = 0:(length(input$vars_to_show)-1),
                              className = 'dt-center',
                              render = JS(
                                "function(data, type, row, meta) {",
                                "return type === 'display' && data.length > 50 ?",
                                "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                "}")
                            ))
            ),class = "display",selection = 'single',rownames=F
          )
  # callModule(module = my_value_boxes,id = "valueBoxes",reactive(to_plot()),reactive(NULL))

  observeEvent(input$DT_to_render_rows_all,{
     callModule(module = my_value_boxes,id="valueBoxes",to_plot,reactive(input$DT_to_render_rows_all))
  })
  # 

  onclick("doc_click",{
    showModal(modalDialog(title="Portail des indicateurs",easyClose = T,
      includeMarkdown("readme.md")
    ))
  })
  
    # search_content=js$searchcontent()
  observeEvent(input$DT_to_render_cell_clicked,{
    if(length(input$DT_to_render_cell_clicked)>0){
    clicked=input$DT_to_render_cell_clicked
    row_clicked=clicked$row
    content=to_plot()[row_clicked]
    content_tags=tag_pred[index==content$index]
    tags_reac(c(content_tags$tag1,content_tags$tag2,content_tags$tag3))
    showModal(modalDialog(
        fluidRow(style="color:#0253a3;text-align:center; margin-top:-15px;font-size:large;background-image: linear-gradient(to bottom,#f5f5f5 0,#e8e8e8 100%);",content$Indicateur),
        fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                 column(6,{
          HTML(paste("<b>Base :</b>",content$Base,"<br>",
                     "<b>Producteur de la base :</b>",content$Producteur,"<br>",
                     "<b>Fréquence d'actualisation:</b>",content$`Fréquence d'actualisation`,"<br>"))
        }),column(6,{
          HTML(paste("<b>Famille :</b>",content$Famille,"<br>",
                     "<b>Source :</b>",content$Source,"<br>"))
        })),
          fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                   column(12,align="left",style="align:left;display:inline-block;",HTML(
                  paste0("<h2>Instruction pour retrouver l'indicateur sur le site du producteur</h2>",
                      "<ul style='text-align: left;list-style: inside;'>
                     <li> <a href =",content$hyperlien," target='_blank'>Aller sur le site du producteur de l'indicateur en cliquant ici </a>",
                     ifelse(!content$`Classement producteur Niveau 1`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 1`),""),
                     ifelse(!content$`Classement producteur Niveau 2`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 2`),""),
                     ifelse(!content$`Classement producteur Niveau 3`=="",paste0("<li> puis aller dans  ", content$`Classement producteur Niveau 3`),""),
                   "</ul>"))))
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
      showNotification(sprintf(ifelse(length(tags_reac()[tags_clicked])==1,
                                      "Le tag %s vient d'être ajouté","Les tags %s viennent d'être ajoutés"),
                               paste(tags_reac()[tags_clicked],collapse=", ")),type = "message",id="add_tag")
    }
  })
  
  observeEvent(c(input$tag
                 # ,input$DT_to_render_search
                 # ,input$DT_to_render_filter
                 ),{
    # text=input$DT_to_render_search#on veut conserver le texte de recherche !
    recherche=input$tag
    to_plot(index)
    if (!is.null(recherche)){
      # print(recherche)
      # which_to_keep=(rowSums(tags_applied[,recherche,with=F])==length(recherche))
      # index_to_keep=tags_applied[which_to_keep]$index
      # tag_names
      which_to_keep=rowSums(data.table(tag_pred$tag1%in%recherche,tag_pred$tag2%in%recherche,tag_pred$tag3%in%recherche))==length(recherche)
      index_to_keep=tag_pred[which_to_keep]$index
      
      to_plot(to_plot()[index%in%index_to_keep])
      output$DT_to_render=renderDT(to_plot()[,input$vars_to_show,with=F],
                                          filter="top",
                                          extensions = 'Buttons',
                                          options = list(searchHighlight = TRUE,
                                                                   language = list(
                                                                     info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                                                                     paginate = list(previous = 'Précédent', `next` = 'Suivant')
                                                                   ),
                                                                   dom = "lftiprB",#"tirB"
                                                                   # initComplete = custom_DT,
                                                         scrollY=T,
                                                                   pageLength = 30
                                                                   ,buttons = c('copy', 'csv', 'excel')
                                                                   ,columnDefs = list(list(
                                                                     targets = 0:(length(input$vars_to_show)-1),
                                                                     className = 'dt-center',
                                                                     render = JS(
                                                                       "function(data, type, row, meta) {",
                                                                       "return type === 'display' && data.length > 50 ?",
                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                                                       "}")
                                                                   ))
                                          ),class = "display",selection = 'single',rownames=F
      )
    }
  })
}