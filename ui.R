dashboardPage(
  tags$header(class = "main-header", span(class = "logo",style="background: #1263b3;", "Indicateurs de Santé"),
              tags$nav(class = "navbar navbar-static-top",style="background: #0253a3;",
                       role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                       a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                         role = "button", span(class = "sr-only", "Toggle navigation")),
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   tags$li(id="logo_ministere",
                                           a(tags$i(class="fa icon_ministere text-success vert_center"),"Solidarités Santé",href="http://solidarites-sante.gouv.fr/",
                                             target="_blank",  rel="noopener noreferrer")),
                                   tags$li(id="logo_drees",
                                           a(tags$i(class="fa icon_drees text-success vert_center"),"Développeur",href="http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/",
                                             target="_blank",  rel="noopener noreferrer")),
                                   # https://resizeimage.net/
                                   
                                   tags$li(id="doc_click",
                                           a(tags$i(class="fa icon_carto text-success vert_center"),"A propos",
                                             target="_blank",  rel="noopener noreferrer"),style='cursor:pointer;'),
                                   
                                   tags$li(id="Github",
                                           a(tags$i(class="fa icon_github text-success vert_center"),"Code Source",href="https://github.com/phileas-condemine/carto_indicateurs",
                                             target="_blank",  rel="noopener noreferrer"))
                           )))
              ,includeCSS("www/my_styles.css")
              ,includeHTML('www/cookie_handler.html')
              ,includeScript("www/hide_when_loading.js")
              ,useShinyjs()  # Set up shinyjs
              
              
              
  ),
  
  dashboardSidebar(collapsed = F,
                   sidebarMenu(id="sidebarmenu",
                               menuItem(text = "Les indicateurs",icon = shiny::icon("search"),tabName="catalogue"),
                               
                               # menuItem("Accueil",icon = shiny::icon("home"),tabName="Accueil"),
                               
                               menuItem(text = "Paramétrage",icon = shiny::icon("gear"),
                                        selectInput(inputId="vars_to_show",label="Variables à afficher",
                                                    selected = init_vars_to_show,choices=names(index),multiple=T),
                                        sliderInput(inputId="nb_chars_cut",label = "Troncage du texte de l'indicateur",
                                                    min = 0,max=max(nchar(index$Indicateur)),value = 80,
                                                    step = 10,round = T,ticks = F,animate = F))),
                   includeHTML("www/logos.html")
                   # ,tags$img(src="Logo_Drees.jpg")
  ),
  dashboardBody(
    
    tabItems(
      # tabItem(tabName = "Accueil",
      #         includeHTML("www/accueil.html"),
      #         s2,
      #         slickROutput("slicker_carousel",height = "300"),
      #         includeHTML("www/footer_accueil.html")
      # ),
      
      tabItem(tabName = "catalogue",
              
              fluidRow(my_value_boxesUI("valueBoxes")),
              div(class="resultats",style="padding-top:50px;padding-bottom:50px;",#style="width:95%;margin-left:20px; margin-right:20px",
                  fluidRow(
                    div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",
                                       label = "Recherche par tags",#selected = ,
                                       choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                       multiple=T,options = list(closeAfterSelect = TRUE,plugins= list('remove_button'),placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list))))
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                        )),
                    div(id="search_keywords_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s) clef(s)",
                                       # choices = term_freq_global$word,
                                       choices = setNames(term_freq_global$word,paste0(term_freq_global$word,' (',term_freq_global$freq,')')),
                                       
                                       multiple=T,
                                       options = list(closeAfterSelect = TRUE,
                                                      create = TRUE,plugins= list('remove_button'),
                                                      # render = I(JS(readLines("render_selectizeInput_keywords.js"))),
                                                      placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs. Les mots-clefs sont triés par fréquence.")
                        ))),
                  fluidRow(
                    div(id="placeholder_datatable",htmlOutput("placeholder_DT")),
                    div(id="carto_datatable",dataTableOutput("DT_to_render")))),
              includeHTML("www/footer_catalogue.html"))
    )
  )
)
