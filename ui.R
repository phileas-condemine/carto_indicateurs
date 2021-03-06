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
              ,includeScript("www/check_browser.js")
              ,tags$script(JS(readLines("www/capture_hover.js")))
              ,tags$script(JS(readLines("www/get_ip.js")))
              ,useShinyjs()  # Set up shinyjs
              ,rclipboardSetup()

              
              
              
  ),
  
  dashboardSidebar(collapsed = F,
                   sidebarMenu(id="sidebarmenu",
                               menuItem(text = "Les indicateurs",icon = shiny::icon("search"),tabName="catalogue"),
                               
                               menuItem("Accueil",icon = shiny::icon("home"),tabName="Accueil"),
                               menuItem("Les indicateurs en chiffres",icon = shiny::icon("tachometer-alt"),tabName="Stats_indicateurs"),
                               menuItem(text = "Paramétrage",icon = shiny::icon("gear"),
                                        selectInput(inputId="vars_to_show",label="Variables à afficher",
                                                    selected = init_vars_to_show,choices=names(index),multiple=T),
                                        sliderInput(inputId="nb_chars_cut",label = "Troncage du texte de l'indicateur",
                                                    min = 0,max=max(nchar(index$Indicateur)),value = 80,
                                                    step = 10,round = T,ticks = F,animate = F)),
                               menuItem(text="Commentaire/remarque",icon=icon("question"),
                                        textAreaInput("feedback_content","Nous contacter",placeholder="Bonjour\nJ'ai trouvé dans le site une incohérence\nJ'ai une suggestion...\nMerci\nSignature ou anonyme",height = "200px"),
                                        textInput("adresse_mail","Adresse e-mail",placeholder = "ex drees-infos@sante.gouv.fr"),
                                        textInput("name_sender","NOM et Prénom",placeholder ="AUBERT Jean-Marc"),
                                        actionButton("feedback_send","Nous contacter",icon=icon("feather"))
                               ))
                   # ,includeHTML("www/logos.html")
                   # ,tags$img(src="Logo_Drees.jpg")
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "Accueil",
              includeHTML("www/accueil.html"),
              s2,
              div(id="placeholder_datatable",htmlOutput("placeholder_DT")),
              slickROutput("slicker_carousel",height = "300"),
              includeHTML("www/footer_accueil.html")
      ),
      tabItem(tabName = "Stats_indicateurs",
              fluidRow(my_value_boxesUI("valueBoxes"))
      ),
      tabItem(tabName = "catalogue",
              
                   fluidRow(
                    div(style="margin-left: 15px;margin-bottom: 30px;margin-top: 20px;",
                      h3("L'ensemble des indicateurs de santé recensés dans un même site")
                    )),             
              div(class="resultats",style="padding-top:20px;padding-bottom:20px;",#style="width:95%;margin-left:20px; margin-right:20px",
                  

                  
                  fluidRow(
                    div(id="two_inputs",
                    div(id="tag_div",class="col-sm-6 inbody_selector",
                        selectizeInput(inputId="tag",
                                       label = "Recherche par thématique",#selected = ,
                                       choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                       multiple=T,options = list(closeAfterSelect = TRUE,plugins= list('remove_button'),placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list))))
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                        )),
                    div(id="search_keywords_div",class="col-sm-5 inbody_selector",
                        selectizeInput(inputId="search_keywords",
                                       label = "Recherche par mot(s)-clef(s)",
                                       # choices = term_freq_global$word,
                                       choices = setNames(term_freq_global$word,paste0(term_freq_global$word,' (',term_freq_global$freq,')')),
                                       
                                       multiple=T,
                                       options = list(closeAfterSelect = TRUE,
                                                      create = T,
                                                      plugins= list('remove_button'),
                                                      # render = I(JS(readLines("render_selectizeInput_keywords.js"))),
                                                      placeholder = "Texte libre, exemple : aide médicale d'état")
                        )%>%shinyInput_label_embed(
                          icon("question-circle") %>%
                            bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs. Les mots-clefs sont triés par fréquence.")
                        ))),
                    div(id="get_URL_button",class="col-sm-1 inbody_selector",uiOutput("get_url_button"))),
                  fluidRow(
                    div(id="carto_datatable",DT::DTOutput("DT_to_render")))),
                    # div(id="carto_datatable",dataTableOutput("DT_to_render")))),
              includeHTML("www/footer_catalogue.html"))
      # ,tabItem(tabName = "feedback_tabItem",
      #   actionButton("feedback","Commentaires/remarques",icon=icon("question"))
      # )
    )
  )
)
