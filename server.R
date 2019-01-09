function(input,output,session){
  
  
  
  toggleModal(session, "startupModal", toggle = "open")
  tags_reac=reactiveVal()
  
  to_plot=reactive({
    recherche=input$tag
    print("ma recherche")
    print(recherche)
    if (!is.null(recherche)){
      which_to_keep=rowSums(data.table(tag_pred$tag1%in%recherche,
                                       tag_pred$tag2%in%recherche,
                                       tag_pred$tag3%in%recherche))==length(recherche)
      index_to_keep=tag_pred[which_to_keep]$index
    } else {
      print("pas de recherche")
      index_to_keep=tag_pred$index
    }
    my_data=index[index%in%index_to_keep]
    my_data=droplevels(my_data)
    # print(head(my_data,3))
    return(my_data)
  })

  
  observe({ 
    to_plot()
    # input$vars_to_show
    print("saving search terms")
    # when it updates, save the search strings so they're not lost
    isolate({
      
      # update global search and column search strings
      default_search <- input$DT_to_render_search
      default_search_columns <- c("", input$DT_to_render_search_columns)
      
      print("fix search terms")
      # update the search terms on the proxy table (see below)
      proxy %>% updateSearch(keywords =
                               list(global = default_search, columns = default_search_columns))
    })
  })  
  output$DT_to_render=renderDT(to_plot()[,input$vars_to_show,with=F]
                               ,
          filter=list(position = 'top'),
          extensions = c('Buttons'
                         ,'ColReorder'
                         , 'FixedHeader'),
          options = list(
             colReorder = TRUE,
             # COMMENT AFFICHER PAR DEFAUT ?
             # buttons = I('colvis'),
             searchHighlight = TRUE,
             stateSave = FALSE,
             searchCols = default_search_columns,
             search = list(regex = FALSE, 
                           caseInsensitive = FALSE, 
                           search = default_search),
             fixedHeader = TRUE,
             language = list(
                           info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
                           paginate = list(previous = 'Précédent', `next` = 'Suivant')),
             dom = "Bfrtip",# "Blftipr"
             # initComplete = custom_DT,
             scrollX=F,
             pageLength = 50,
             buttons = c('copy', 'csv', 'excel'),
             columnDefs = list(#list(targets=(which(!names(index)%in%
                               #             c("Indicateur","Source","Producteur"))-1)# on compte à partir de 0 en JS
                               #     ,visible=FALSE)
                list(
                   targets = "_all",#0:(ncol(to_plot())-1),
                   className = 'dt-center',
                   render = JS(
                     "function(data, type, row, meta) {",
                     "return type === 'display' && data.length > 80 ?",
                     "'<span title=\"' + data + '\">' + data.substr(0, 80) + '...</span>' : data;",
                     "}"
                     # ,"function(data, type, full) {",
                     # "<write a tool tip or alert> blabla < all your data is in full variable>",
                     # "}"
                     )
                 )
                # ,
                #https://datatables.net/forums/discussion/32240/how-to-implement-a-popup-tooltip-on-a-datatables-cell-that-displays-all-data
               )
            ),
          class = "display",selection = 'single',rownames=F
          )
  proxy <- dataTableProxy('DT_to_render')

  observeEvent(input$DT_to_render_rows_all,{
    print("module for valueboxes")
     callModule(module = my_value_boxes,id="valueBoxes",to_plot,reactive(input$DT_to_render_rows_all))
  })
  onclick("doc_click",{
    showModal(modalDialog(title="Portail des indicateurs",easyClose = T,
      includeMarkdown("readme.md")
    ))
  })
  onclick("valuebox_indicateurs",{
    showModal(modalDialog(title="Informations sur les indicateurs sélectionnés",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_indicateurs",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  onclick("valuebox_bases",{
    showModal(modalDialog(title="Informations sur les bases",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_bases",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  onclick("valuebox_producteurs",{
    showModal(modalDialog(title="Informations sur les producteurs des indicateurs filtrés",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_producteurs",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  onclick("valuebox_sources",{
    showModal(modalDialog(title="Informations sur les sources des indicateurs filtrés",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_sources",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  onclick("valuebox_prod_principal",{
    showModal(modalDialog(title="Informations sur le producteur principal",easyClose = T,size = "m",fade = T,
        tags$div(id="modal_prod_principal",
                 HTML(paste("Un bon endroit pour afficher des informations complémentaire sur le producteur principal")))
    ))
  })  
  onclick("valuebox_nb_tags",{
    showModal(modalDialog(title="Informations sur le nombre de tags représentés",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_nb_tags",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  
  observeEvent(input$DT_to_render_cell_clicked,{
    print("cell clicked")
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
    print("add tag")
    tags_clicked=c(input$tag1%%2,input$tag2%%2,input$tag3%%2)
    tags_clicked=which(tags_clicked==1)
    currently_selected=input$tag
    new_selection=c(currently_selected,tags_reac()[tags_clicked])
    if(length(tags_clicked)>0){
      removeUI(selector = ".shiny-input-container",multiple = F,session = session,immediate = T)
      insertUI(session = session,selector = "#tags_select_bar",where = "afterBegin",ui=selectInput(inputId = "tag",
                      label = "Recherche par tags",choices = tag_names,
                      selected = new_selection,multiple = T),multiple = F,immediate = F)
      
      showNotification(sprintf(ifelse(length(tags_reac()[tags_clicked])==1,
                                      "Le tag %s vient d'être ajouté","Les tags %s viennent d'être ajoutés"),
                               paste(tags_reac()[tags_clicked],collapse=", ")),type = "message",id="add_tag")
    }
  })
  
  addPopover(session,id = "tags_select_bar",title = "Filtrage par thématiques",placement="right",
             options= list(container = "body"),
             content = "Vous pouvez sélectionner plusieurs thématiques pour 
             filtrer les indicateurs.\nVous pouvez également utiliser la barre 
             de recherche située à droite pour chercher des mots clefs."
  )
  addPopover(session,id = "vars_select_bar",title = "Choix des variables", placement="right",
             options= list(container = "body"),
             content = "Ce menu vous permet de sélectionner les variables à afficher 
             dans le tableau central."
  )
  # addPopover(session,id = "vars_select_bar",title = "Choix des variables", placement="right",
  #            options= list(container = "body"),
  #            content = "Ce menu vous permet de sélectionner les variables à afficher 
  #            dans le tableau central."
  # )
  
  # dataTables_filter
  
  # addPopover(session,id = "DT_to_render",title = "Indicateurs", 
  #            options= list(),placement = "top",
  #            content = "Cliquez pour en savoir plus sur cet indicateur"
  # )

}


