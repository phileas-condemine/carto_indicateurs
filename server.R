function(input,output,session){

  
  
  displayed_notif_about_randomization=reactiveVal(F)

  tags_reac=reactiveVal()

  to_plot=reactive({
    recherche=input$tag
    # recherche=names(tag_pred)[c(3,5,10)]
    if (!is.null(recherche)){

      which_to_keep=rowSums(tag_pred[,recherche,with=F])==length(recherche)
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
      print("val to mongodb")
      print(val)
      db$insert(data.frame(id=id,time=Sys.time(),input="tag",valeur=val))
    })
    observeEvent(input$search_keywords,{
      val=paste(input$search_keywords,collapse=" & ")
      print("search keywords val")
      print(val)
      db$insert(data.frame(id=id,time=Sys.time(),input="search_keywords",valeur=val))
    })
    observeEvent(input$clipbtn,{
      val=url_to_bookmark()
      print("share url")
      print(val)
      db$insert(data.frame(id=id,time=Sys.time(),input="share_url",valeur=val))
    })
    
  }
  

  output$get_url_button=renderUI({
    if(length(input$tag)>0|length(input$search_keywords)>0){
      rclipButton("clipbtn", "Partager",
                  url_to_bookmark(), icon = icon("clipboard"))
    } else{
      NULL
    }
    })

  url_to_bookmark = reactive({
    print("update clip button")
    tags_picked=which(tags_class_vec%in%input$tag)
    tags_picked=paste(tags_picked,collapse="_")
    words_picked=term_freq_global[word%in%input$search_keywords]$hash
    words_picked=paste(words_picked,collapse="_")
    print("url")
    url=paste0("?q=",ifelse(length(input$tag)>0,paste0("tags%in%",tags_picked),""),
               ifelse(length(input$tag)>0&length(input$search_keywords)>0,"&",""),
               ifelse(length(input$search_keywords)>0,paste0("words%in%",words_picked),""))
    port=session$clientData$url_port
    port=ifelse(nchar(port)>0,paste0(":",port),"")
    protocol=session$clientData$url_protocol
    protocol=ifelse(nchar(protocol)>0,paste0(protocol,"//"),"")
    
    url=paste0(protocol,session$clientData$url_hostname,
               port,session$clientData$url_pathname,url)
    url
  })

  
  onclick("clipbtn",{
    showNotification(tags$p("L'adresse (URL) a été copiée dans le presse-papier avec succès !"),
                     duration = 5, closeButton = TRUE, type = "message")
  })

  output$DT_to_render=renderDT({
    # print("search keywords in renderDT")
    # print(input$search_keywords)
    if(length(input$tag)>0|length(input$search_keywords)>0){

      if(!displayed_notif_about_randomization()){
        showNotification(a("Deux requêtes identiques ne fourniront pas les résultats dans le même ordre.",
                           href="https://medium.com/@galatea.net/charte-%C3%A9thique-pour-les-algorithmes-publics-b0c5422a54c9",target="_blank", rel="noopener noreferrer"),
                         duration = 15, closeButton = TRUE, type = "message")
        displayed_notif_about_randomization(T)
      }


      keywords=paste(input$search_keywords,collapse=" ")%>%
        stringi::stri_trans_general("Latin-ASCII")%>%iconv(to="UTF-8")#"Latin-ASCII"#"ASCII//TRANSLIT"
      print(keywords)
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
                       search = keywords),
         fixedHeader = TRUE,
         language = list(
           info = 'Résultats _START_ à _END_ sur une liste de _TOTAL_.',
           paginate = list(previous = 'Précédent', `next` = 'Suivant'),
           lengthMenu='Afficher _MENU_ résultats'
           ),
         dom = "itBpl",# "Blftipr"
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
      # print(head(my_datatable))
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
               ui = div(id="search_keywords_div",class="col-sm-5 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s) clef(s)",
                                       choices = setNames(term_freq$word,paste0(term_freq$word,' (',term_freq$freq,')')),
                                       multiple=T,
                                       options = list(closeAfterSelect = TRUE,
                                                      create = TRUE,plugins= list('remove_button'),
                                                      # render = I(JS(readLines("render_selectizeInput_keywords.js"))),
                                                      placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs.")
                        )))
      # updateSelectizeInput(session,inputId = "search_keywords",server=T,selected = currently_selected_keywords,
      #                      choices = term_freq%>%mutate(label=word)%>%rename(value=word),
      #                      options = list(closeAfterSelect = TRUE,
      #                                                create = TRUE,plugins= list('remove_button'),
      #                                                render = I(JS(readLines("render_selectizeInput_keywords.js"))),
      #                                                placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...'))

      removeUI(selector = "#tag_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",
                                       label = "Recherche par tags",
                                       choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                       multiple=T,
                                       options = list(closeAfterSelect = TRUE,plugins= list('remove_button'),
                                                      placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list))))
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                        )))


    }})
  })

  observeEvent(input$feedback_send,{
    text_to_send=input$feedback_content
    print(text_to_send)
    if(to_mongo_db)
      db$insert(data.frame(id=id,time=Sys.time(),input="feedback",valeur=text_to_send))
    
  })
  
  observeEvent(c(input$DT_to_render_rows_all),{
    isolate({


      sub_index=to_plot()
      if(length(input$DT_to_render_rows_all)>0){
        sub_index=sub_index[input$DT_to_render_rows_all]
        }
      sub_index=sub_index$index


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
               ui = div(id="search_keywords_div",class="col-sm-5 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s) clef(s)",
                                       selected = currently_selected_keywords,
                                       # choices = term_freq$word,
                                       choices = setNames(term_freq$word,paste0(term_freq$word,' (',term_freq$freq,')')),
                                       
                                       multiple=T,options = list(closeAfterSelect = TRUE,
                                                                 create = TRUE,plugins= list('remove_button'),
                                                                 # render = I(JS(readLines("render_selectizeInput_keywords.js"))),
                                                                 placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs.")
                        )))

      # sub_tags=tag_pred[index%in%sub_index,
      #                   c("tag1","tag2","tag3")]%>%
      #   unlist()%>%
      #   unique()
      sub_tags=tag_pred%>%
        filter(index%in%sub_index)%>%
        select(-index)%>%
        colSums()%>%{
          .[.>0]
        }%>%
        names()

      currently_selected_tags=input$tag
      if(length(input$search_keywords)>0|length(input$tag)>0){
        sub_tags_class_list=lapply(tags_class_list,function(x)x[x%in%sub_tags])
      } else {
        sub_tags_class_list=tags_class_list
      }
      removeUI(selector = "#tag_div",immediate = T,session=session)
      insertUI(selector = ".resultats",where = "afterBegin",immediate = T,session = session,
               ui = div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",options=list(closeAfterSelect = TRUE,plugins= list('remove_button'),placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list)))),
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
  wordcloud_rep <- repeatable(wordcloud)
  onclick("valuebox_indicateurs",{
    nb_indicateurs=ifelse(length(input$DT_to_render_rows_all)>0,
                          length(input$DT_to_render_rows_all),
                          nrow(to_plot()))
      print("indics")
      print(input$DT_to_render_rows_all)
      indics=to_plot()
      if (length(input$DT_to_render_rows_all)>0){
        indics=indics[input$DT_to_render_rows_all]
        }#%>%dplyr::select(Indicateur)
      print(indics)
      indics=indics%>%select(Indicateur)
      # indics=index%>%select(Indicateur)
      term_count <- indics %>%
        unnest_tokens(word, Indicateur)
      term_count <- term_count %>%
        anti_join(data.frame(word=stopwords_vec,stringsAsFactors =F))
      term_count <- term_count %>%
        count(word, sort = TRUE)
      print("wordcloud")
      print(term_count%>%head(10))
      output$wordcloud_static <- renderPlot({
        wordcloud_rep(term_count$word, term_count$n, scale=c(4,0.5),
                      min.freq = term_count$n[100], max.words=100,
                      colors=brewer.pal(8, "Dark2"))
      })
      # output$wordcloud=renderWordcloud2({
      # wordcloud2(term_count%>%head(500))
      #   })


    showModal(modalDialog(title=NULL,size="l",easyClose = T,fade = T,
                          # wordcloud2Output("wordcloud"),
                          plotOutput("wordcloud_static",width = "100%"),
                          
                          footer=NULL

    ))
  })
  onclick("valuebox_bases",{
    nb_indicateurs=ifelse(length(input$DT_to_render_rows_all)>0,
                          length(input$DT_to_render_rows_all),
                          nrow(to_plot()))
    output$quick_plot=renderPlotly({
      req(input$Choix_var_quick_stat)
      print(input$Choix_var_quick_stat)
      # var_for_stat=iconv(input$Choix_var_quick_stat,to = "UTF-8")
      var_for_stat=input$Choix_var_quick_stat
      print(var_for_stat)
      stat=to_plot()
      if(length(input$DT_to_render_rows_all)>0){
        stat=stat[input$DT_to_render_rows_all]
      }
      stat=stat[,list(count=.N),by=eval(var_for_stat)]
      setnames(stat,var_for_stat,"ventilation")
      stat[,frac:=count/.N]
      if(input$alpha_freq_order){
        stat[,ventilation:=reorder(factor(ventilation),-count)]
      }
      plot_ly(data=stat,x=~ventilation,y=~count,type="bar")%>%layout(xaxis=list(title=F))
    })
    vars_interessants_stats=names(to_plot())
    vars_interessants_stats=vars_interessants_stats[vars_interessants_stats%in%c("Base","Source","Producteurs","Echelle géo. nationale","Echelle géo. Rég","Echelle géo dep",
                                                                                 "Autre échelle de restitution","Profondeur historique","Fréquence d'actualisation",
                                                                                 "Date version base","Type d'accès","Producteur de la base")]
    vars_interessants_stats=sample(vars_interessants_stats)
    showModal(modalDialog(title=NULL,size="l",easyClose = T,fade = T,
                          fluidRow(selectInput("Choix_var_quick_stat",
                                      sprintf("Comment se répartissent les %s indicateurs sélectionnés selon différents critères ?",
                                              nb_indicateurs),choices = vars_interessants_stats
                          ),
                                    switchInput(inputId = "alpha_freq_order",label = "Ordre",
                                                value=T,onLabel="Fréquence",offLabel = "Alphabétique",width="100%")),
                          plotlyOutput("quick_plot"),footer=NULL

    ))
  })
  onclick("valuebox_producteurs",{
    nb_indicateurs=ifelse(length(input$DT_to_render_rows_all)>0,
                          length(input$DT_to_render_rows_all),
                          nrow(to_plot()))
    sub_index=to_plot()
    if(length(input$DT_to_render_rows_all)>0){
      sub_index=sub_index[input$DT_to_render_rows_all]
    }
    sub_index=sub_index$index
    output$network=renderForceNetwork({
      # sub_index=index$index
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
    showModal(modalDialog(title=sprintf("Collaboration des producteurs pour la création des %s indicateurs.",nb_indicateurs),size="l",easyClose = T,fade = T,

                          forceNetworkOutput("network",width = "auto",height = "400px"),footer=NULL

                          ))
  })
  onclick("valuebox_sources",{
    nb_indicateurs=ifelse(length(input$DT_to_render_rows_all)>0,
                          length(input$DT_to_render_rows_all),
                          nrow(to_plot()))
    sub_index=to_plot()
    if(length(input$DT_to_render_rows_all)>0){
      sub_index=sub_index[input$DT_to_render_rows_all]
    }
    sub_index=sub_index$index
    output$network=renderForceNetwork({
      # sub_index=index$index
      sub_cooc=cooc_source[index%in%sub_index,list(weight=.N),by=c("variable.x","variable.y")]
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
    showModal(modalDialog(title=sprintf("Rapprochement de sources pour la création des %s indicateurs.",nb_indicateurs),size="l",easyClose = T,fade = T,

                          forceNetworkOutput("network",width = "auto",height = "400px"),footer=NULL

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
    indicateurs_index=to_plot()
    if(length(input$DT_to_render_rows_all)>0){
      indicateurs_index=indicateurs_index[input$DT_to_render_rows_all]
    }
    indicateurs_index=indicateurs_index$index
    # indicateurs_index=index[1:1000,]$index
    # tags_count=tag_pred[index%in%indicateurs_index,
    #             c("tag1","tag2","tag3")]%>%
    #   unlist()%>%table%>%data.frame
    # names(tags_count) <- c("tags","count")

    tags_count=tag_pred[index%in%indicateurs_index
                ,tolower(tag_names),with=F]%>%
      colSums()

    tags_count=data.frame(tags=names(tags_count),count=c(tags_count),stringsAsFactors = F,row.names = NULL)

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
    showModal(modalDialog(title="Informations sur les tags représentés",size="l",easyClose = T,fade = T,
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
      # tags_reac(c(content_tags$tag1,content_tags$tag2,content_tags$tag3))
      content_tags=tag_pred[index==content$index]
      content_tags$index=NULL
      tags_reac(names(content_tags)[which(content_tags==1)])
      if(to_mongo_db){
        val=content$Indicateur
        print("to mongodb val")
        print(val)
        db$insert(data.frame(id=id,time=Sys.time(),input="click_indicateur",valeur=val))
      }



      showModal(modalDialog(
        fluidRow(style="color:#0253a3;text-align:center; margin-top:-15px;font-size:large;background-image: linear-gradient(to bottom,#f5f5f5 0,#e8e8e8 100%);",content$Indicateur),
        fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                 column(6,{
                   HTML(paste("<b>Base :</b>",content$Base,"<br>",
                              "<b>Producteur de la base :</b>",content$Producteur,"<br>",
                              "<b>Fréquence d'actualisation:</b>",content$Frequence_d_actualisation,"<br>"))
                 }),column(6,{
                   HTML(paste("<b>Famille :</b>",content$Famille,"<br>",
                              "<b>Source :</b>",content$Source,"<br>"))
                 })),
        fluidRow(style="border-width:1px;border-style:ridge;border-color:#f5f5f5;padding-top:10px;padding-bottom:10px;",
                 column(12,align="left",style="align:left;display:inline-block;",HTML(
                   paste0("<h2>Instruction pour retrouver l'indicateur sur le site du producteur</h2>",
                          "<ul style='text-align: left;list-style: inside;'>
                     <li> <a href =",content$Acceder_a_la_base," target='_blank' rel='noopener noreferrer'>Aller sur le site du producteur de l'indicateur en cliquant ici </a>",
                          ifelse(!content$`Classement_producteur_Niveau_1__`=="",paste0("<li> puis aller dans  ", content$`Classement_producteur_Niveau_1__`),""),
                          ifelse(!content$`Classement_producteur_Niveau_2`=="",paste0("<li> puis aller dans  ", content$`Classement_producteur_Niveau_2`),""),
                          ifelse(!content$`Classement_producteur_Niveau_3__`=="",paste0("<li> puis aller dans  ", content$`Classement_producteur_Niveau_3__`),""),
                          "</ul>"))))
        ,fluidRow(infoBox(title="Site du producteur",value="Pour retrouver l'indicateur, suivez les instructions ci-dessus.",
                 href = sprintf('javascript:void(window.open("%s", "_blank", rel="noopener noreferrer"))',
                                content$Acceder_a_la_base),
                 icon = icon("door-open"),width = 12))
        ,footer=NULL


        ,easyClose =T,size="l"))
    }
  })

  observe({
    # IL FAUDRAIT QUE CA SE DECLENCHE LORSQUE LES SELECTIZEINPUT SONT DEJA READY
    # ?q=tags%in%4_27&words%in%41_151_2479
    print(session$clientData$url_search)
    url=session$clientData$url_search
    # url="?q=tags%in%4_27&words%in%41_151_2479"
    # url="?q=tags%in%4_27"
    # url="?q=words%in%41_151_2479"
    
    words_h=stringr::str_extract(url,"(words).+$")
    if(!is.na(words_h)){
      words_h=gsub("words%in%","",words_h)
      words_h=strsplit(words_h,"_")[[1]]
      words=term_freq_global[hash%in%words_h]$word
      updateSelectInput(session,"search_keywords",selected=words)
      
    } else {
      words=NULL
    }
    tags=gsub("(words).+$","",url)%>%stringr::str_extract("(tags).+$")
    print(tags)
    if(!is.na(tags)){
      tags=gsub("&$","",tags)
      tags=gsub("tags%in%","",tags)
      tags=strsplit(tags,"_")[[1]]%>%as.numeric
      tags=tags_class_vec[tags]
      updateSelectInput(session,"tag",selected=tags)
    } else{
      tags=NULL
    }
  })
  

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

  # addPopover(session,id = "DataTables_Table_0_filter",title = "Recherche par mots clés",
  #            placement="left",# options= list(container = "body"),
  #            content = 'Filtrer les indicateurs par mots clés,
  #            par exemple en écrivant "DREES"'
  # )
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
