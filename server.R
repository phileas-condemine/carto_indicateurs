function(input,output,session){
  
  displayed_notif_about_randomization=reactiveVal(F)
  
  tags_reac=reactiveVal()
  
  to_plot=reactive({
    recherche=input$tag
    if (!is.null(recherche)){
      which_to_keep=rowSums(data.table(tag_pred$tag1%in%recherche,
                                       tag_pred$tag2%in%recherche,
                                       tag_pred$tag3%in%recherche))==length(recherche)
      index_to_keep=tag_pred[which_to_keep]$index
    } else {
      index_to_keep=tag_pred$index
    }
    my_data=index[index%in%index_to_keep]
    my_data=droplevels(my_data)
    return(my_data)
  })
  
  
  if(to_mongo_db){
    observeEvent(input$tag,{
      val=paste(input$tag,collapse=" & ")
      print(val)
      db$insert(data.frame(id=id,time=Sys.time(),input="tag",valeur=val))
    })
    observeEvent(input$search_keywords,{
      val=paste(input$search_keywords,collapse=" & ")
      print(val)
      db$insert(data.frame(id=id,time=Sys.time(),input="search_keywords",valeur=val))
    })
    
  }
  
  
  
  output$DT_to_render=renderDT({
    print(input$search_keywords)
    if(length(input$tag)>0|length(input$search_keywords)>0){
      
      if(!displayed_notif_about_randomization()){
        showNotification(a("Deux requêtes identiques ne fourniront pas les résultats dans le même ordre.",
                           href="https://medium.com/@galatea.net/charte-%C3%A9thique-pour-les-algorithmes-publics-b0c5422a54c9"), 
                         duration = 15, closeButton = TRUE, type = "message")
        displayed_notif_about_randomization(T)
      }
      
      
      
      my_datatable=datatable(to_plot()[,input$vars_to_show,with=F],
       extensions = c('Buttons'
                      ,'ColReorder'
                      , 'FixedHeader',"Responsive"),
       options = list(
         lengthMenu = list(c(10, 20, 50, -1),
                           c(10, 20, 50, "Tout")),
         colReorder = TRUE,
         searchHighlight = TRUE,
         stateSave = FALSE,
         searchCols = default_search_columns,
         search = list(regex = FALSE,
                       caseInsensitive = TRUE,
                       search = iconv(paste(input$search_keywords,collapse=" "),to = "UTF-8")),
         fixedHeader = TRUE,
         language = list(
           info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
           paginate = list(previous = 'Précédent', `next` = 'Suivant'),
           lengthMenu='Afficher _MENU_ résultats'
           ),
         dom = "tBpl",# "Blftipr"
         initComplete = JS(readLines("www/custom_DT.js")),#custom_DT,
         scrollX=F,
         pageLength = 50,
         buttons = list(list(extend = "copy",
                             text = "Copier"),
                        list(extend = "csv",
                             text = "Format CSV"),
                        list(extend = "excel",
                             text = "Format Excel")),#c('copy', 'csv', 'excel'),
         
         columnDefs = list(
           list(
             targets = "_all",
             className = 'dt-center',
             render =
               # JS(readLines("www/render_customized.js", warn = FALSE))
               JS(
                 "function(data, type, row, meta) {",
                 sprintf("return type === 'display' && data.length > %s ?",input$nb_chars_cut),
                 sprintf("'<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;",input$nb_chars_cut),
                 "}"
               )
           )
           # ,
           #https://datatables.net/forums/discussion/32240/how-to-implement-a-popup-tooltip-on-a-datatables-cell-that-displays-all-data
         )
       ),class = "display hover",selection = 'none',rownames=F)
      callModule(module = my_value_boxes,id="valueBoxes",
                 to_plot,reactive(input$DT_to_render_rows_all))
      my_datatable
    }else {
      callModule(module = my_value_boxes,id="valueBoxes",
                 to_plot,reactive(NULL))
      
      
      
      NULL
    }
  })
  
  
  output$placeholder_DT=renderUI({
    req((length(input$tag)+length(input$search_keywords))==0)
    includeHTML("www/placeholder_datatable.html")
  })
  
  
  observeEvent((length(input$tag)+length(input$search_keywords))==0,{
    isolate({
    if((length(input$tag)==0)&(length(input$search_keywords)==0)&displayed_notif_about_randomization()){#On utilise displayed_notif_about_randomization pour vérifier qu'on n'est pas à la phase d'initialisation ie un tableau a déjà été affiché !
      term_freq=full_text_split[,list(freq=.N),by="word"]
      term_freq=term_freq[!word%in%stopwords_vec]
      setorder(term_freq,-freq)
      removeUI(selector = "#search_keywords_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="search_keywords_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s) clef(s)",
                                       choices = term_freq$word,
                                       multiple=T,options = list(plugins= list('remove_button'),placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs.")
                        )))
      
      removeUI(selector = "#tag_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",
                                       label = "Recherche par tags",
                                       choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                       multiple=T,
                                       options = list(plugins= list('remove_button'),
                                                      placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list))))
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                        )))


    }})
  })
  
  observeEvent(c(input$DT_to_render_rows_all),{
    isolate({
      
      
      sub_index=to_plot()[input$DT_to_render_rows_all]$index
      if(length(input$search_keywords)>0|length(input$tag)>0){
        term_freq=full_text_split[index%in%sub_index,list(freq=.N),by="word"]
      } else {
        term_freq=full_text_split[,list(freq=.N),by="word"]
      }
      term_freq=term_freq[!word%in%stopwords_vec]
      setorder(term_freq,-freq)
      currently_selected_keywords=input$search_keywords
      removeUI(selector = "#search_keywords_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="search_keywords_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s) clef(s)",
                                       selected = currently_selected_keywords,
                                       choices = term_freq$word,
                                       multiple=T,options = list(plugins= list('remove_button'),placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs.")
                        )))
      
      sub_tags=tag_pred[index%in%sub_index,
                        c("tag1","tag2","tag3")]%>%
        unlist()%>%
        unique()
      
      currently_selected_tags=input$tag
      if(length(input$search_keywords)>0|length(input$tag)>0){
        sub_tags_class_list=lapply(tags_class_list,function(x)x[x%in%sub_tags])
      } else {
        sub_tags_class_list=tags_class_list
      }
      removeUI(selector = "#tag_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",options=list(plugins= list('remove_button'),placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list)))),
                                       label = "Recherche par tags",
                                       selected = currently_selected_tags,
                                       choices = sub_tags_class_list,#selectize = F,size = length(tag_names)+5,
                                       multiple=T)%>%shinyInput_label_embed(
                                         icon("question-circle") %>%
                                           bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                                       )))
      
    })
    
  })
  
  
  onclick("doc_click",{
    showModal(modalDialog(title="Portail des indicateurs",easyClose = T,
                          includeMarkdown("readme.md")
    ))
  })
  onclick("valuebox_indicateurs",{
    nb_indicateurs=nrow(to_plot())
    output$quick_plot=renderPlotly({
      req(input$Choix_var_quick_stat)
      stat=to_plot()[,list(count=.N),by=eval(input$Choix_var_quick_stat)]
      setnames(stat,input$Choix_var_quick_stat,"ventilation")
      plot_ly(data=stat,x=~ventilation,y=~count)
    })
    
    vars_interessants_stats=names(to_plot())
    vars_interessants_stats=vars_interessants_stats[vars_interessants_stats%in%c("Base","Source","Producteurs","Echelle géo. nationale","Echelle géo. Rég","Echelle géo dep",
                                                                                 "Autre échelle de restitution","Profondeur historique","Fréquence d'actualisation",
                                                                                 "Date version base","Type d'accès","Producteur de la base")]
    vars_interessants_stats=sample(vars_interessants_stats)
    showModal(modalDialog(title="Informations sur les indicateurs sélectionnés",easyClose = T,size = "m",fade = T,
                          selectInput("Choix_var_quick_stat",sprintf("Comment se répartissent les %s indicateurs sélectionnés ?",nb_indicateurs),choices = vars_interessants_stats),
                          plotlyOutput("quick_plot"),footer=NULL
                          
    ))
  })
  onclick("valuebox_bases",{
    
    output$bases_plot=renderPlotly({
      stat=to_plot()[,list(count=.N),by="Base"]
      plot_ly(data=stat,labels=~Base,values=~count,type = "pie",showlegend = FALSE)
    })
    
    showModal(modalDialog(title="Répartition des indicateurs dans les bases",easyClose = T,size = "m",fade = T,
                          plotlyOutput("bases_plot"),footer=NULL
                          
    ))
  })
  onclick("valuebox_producteurs",{
    output$bases_plot=renderForceNetwork({
      sub_index=to_plot()$index
      sub_index=index$index
      sub_cooc=cooc_producteurs[index%in%sub_index,list(weight=.N),by=c("variable.x","variable.y")]
      graph=graph.data.frame(sub_cooc)
      graph2=networkD3::igraph_to_networkD3(graph)
      graph2$nodes$group=1
      graph2$nodes$name=as.character(graph2$nodes$name)
      node_size=sub_cooc[variable.x==variable.y,c("variable.x","weight")]
      graph2$nodes=merge(graph2$nodes,node_size,by.x="name",by.y="variable.x")
      graph2$nodes$weight=sqrt(graph2$nodes$weight)
      forceNetwork(Links = graph2$links, Nodes = graph2$nodes,Value="value",
                   Source = 'source', Target = 'target',
                   linkWidth = networkD3::JS("function(d) { return Math.log(d.value); }"),
                   Nodesize = "weight", 
                   NodeID = 'name',Group='group',zoom = TRUE,fontSize=20,opacity = 1)    
      
    })
    showModal(modalDialog(title="Collaborations entre les producteurs",easyClose = T,size = "m",fade = T,
    
                          forceNetworkOutput("bases_plot",width = "auto",height = "400px"),footer=NULL
                          
                          ))
  })
  onclick("valuebox_sources",{
    showModal(modalDialog(title="Informations sur les sources des indicateurs filtrés",easyClose = T,size = "m",fade = T,
                          tags$div(id="modal_sources",
                                   HTML(paste("Un bon endroit pour afficher des informations complémentaire")))
    ))
  })
  # onclick("valuebox_prod_principal",{
  #   
  #   showModal(modalDialog(title="Informations sur le producteur principal",easyClose = T,size = "m",fade = T,
  #                         tags$div(id="modal_prod_principal",
  #                                  HTML(paste("Un bon endroit pour afficher des informations complémentaire sur le producteur principal"))),
  #                         
  #   ))
  # })
  onclick("valuebox_nb_tags",{
    sequences <- read.csv(
           system.file("examples/visit-sequences.csv",package="sunburstR")
           ,header = FALSE
           ,stringsAsFactors = FALSE
       )[1:100,]
    indicateurs_index=to_plot()$index
    # indicateurs_index=index[1:1000,]$index
    tags_count=tag_pred[index%in%indicateurs_index,
                c("tag1","tag2","tag3")]%>%
      unlist()%>%table%>%data.frame
    names(tags_count) <- c("tags","count")
    theme_tag=tags_class_list%>%stack%>%
      mutate_if(is.factor,as.character)%>%
      mutate_all(function(x)gsub(" ","_",x))
    tags_count=merge(tags_count,theme_tag,by.x="tags",by.y="values",all.x=T)
    tags_count=tags_count%>%
      mutate_if(is.factor,as.character)%>%
      mutate(ind=paste(ind,tags,sep="-"))%>%
      select(ind,count)
    output$tags_sunburst=renderSunburst({
      sunburst(rbind(tags_count),legend = F
      )
    })
    showModal(modalDialog(title="Informations sur les tags représentés",easyClose = T,size = "m",fade = T,
                          sunburstOutput("tags_sunburst"),footer=NULL
    ))
  })
  
  
  
  ##### click sur les boxes du placeholder_datatable
  
  onclick(id = "tags_box",{
    showModal(modalDialog(title="Classement des tags par thématiques",easyClose = T,size = "m",fade = T,
                          tags$img(src="liste_des_tags.png")
    ))
  })
  onclick(id = "infos_producteurs_box",{
    ns <- session$ns #https://shiny.rstudio.com/articles/modules.html
    showModal(modalDialog(title="Producteurs des indicateurs",easyClose = T,size = "l",fade = T,
                          s2,
                          # slickROutput(ns("slicker_carousel"),height = "300"),
                          s1,
                          footer=NULL
    ))
  })
  
  
  
  observeEvent(input$DT_to_render_cell_clicked,{
    req(input$DT_to_render_cell_clicked)
    if(length(input$DT_to_render_cell_clicked)>0){
      clicked=input$DT_to_render_cell_clicked
      row_clicked=clicked$row
      content=to_plot()[row_clicked]
      content_tags=tag_pred[index==content$index]
      tags_reac(c(content_tags$tag1,content_tags$tag2,content_tags$tag3))
      
      if(to_mongo_db){
        val=content$Indicateur
        print(val)
        db$insert(data.frame(id=id,time=Sys.time(),input="click_indicateur",valeur=val))
      }
      
      
      
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
        # ,footer = tagList(
        # actionButton("tag1",label = content_tags$tag1),
        # actionButton("tag2",label = content_tags$tag2),
        # actionButton("tag3",label = content_tags$tag3)
        ,fluidRow(column(6),infoBox(title="Site du producteur",value="Pour retrouver l'indicateur, les instructions ci-dessus",
                 href = sprintf("javascript:void(window.open('%s', '_blank'))", 
                                content$hyperlien),
                 icon = icon("door-open"),width = 6))
        ,footer=NULL
        
        
        ,easyClose =T,size="l"))
    }
  })
  # observeEvent(c(input$tag1,input$tag2,input$tag3),{
  #   # Ajout de filtres par tagg depuis le menu de l'indicateur
  #   tags_clicked=c(input$tag1%%2,input$tag2%%2,input$tag3%%2)
  #   tags_clicked=which(tags_clicked==1)
  #   currently_selected=input$tag
  #   new_selection=c(currently_selected,tags_reac()[tags_clicked])
  #   if(length(tags_clicked)>0){
  #     updateSelectizeInput(inputId = "tag",session = session,selected=new_selection)
  #     # removeUI(selector = ".shiny-input-container",multiple = F,session = session,immediate = T)
  #     # insertUI(session = session,selector = "#tags_select_bar",where = "afterBegin",
  #     #          ui=selectInput(inputId = "tag",
  #     #                 label = "Recherche par tags",choices = tag_names,
  #     #                 selected = new_selection,multiple = T),immediate = F)
  # 
  #     showNotification(sprintf(ifelse(length(tags_reac()[tags_clicked])==1,
  #                                     "Le tag %s vient d'être ajouté","Les tags %s viennent d'être ajoutés"),
  #                              paste(tags_reac()[tags_clicked],collapse=", ")),type = "message",id="add_tag")
  #   }
  # })
  
  addPopover(session,id = "tags_select_bar",title = "Filtrage par thématiques",placement="right",
             options= list(container = "body"),
             content = "Vous pouvez sélectionner plusieurs thématiques pour
             filtrer les indicateurs.\nVous pouvez également utiliser la barre
             de recherche située à droite pour chercher des mots clefs"
  )
  addPopover(session,id = "vars_select_bar",title = "Choix des variables", placement="right",
             options= list(container = "body"),
             content = "Ce menu vous permet de sélectionner les variables à afficher
             dans le tableau central"
  )
  
  addPopover(session,id = "DataTables_Table_0_filter",title = "Recherche par mots clés",
             placement="left",# options= list(container = "body"),
             content = 'Filtrer les indicateurs par mots clés,
             par exemple en écrivant "DREES"'
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
  
  # https://stackoverflow.com/questions/41351199/how-to-code-a-sidebar-collapse-in-shiny-to-show-only-icons
  runjs({'
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
    '})
  
  
  
  onclick('switchState', runjs({'
    var title = document.querySelector(".logo")
    if (title.style.visibility == "hidden") {
    title.style.visibility = "visible";
    /*} else {
    title.style.visibility = "hidden";*/
    }
    '}))
  
  output$slicker_carousel=renderSlickR(s1)
  # output$slicker_carousel=renderSlickR(slickr_carousel)
  
  
}
