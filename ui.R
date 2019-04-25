dashboardPage(
  
  tags$header(class = "main-header", span(class = "logo",style="background: #1263b3;", "Indicateurs de Santé V0.5"),
              tags$nav(class = "navbar navbar-static-top",style="background: #0253a3;",
                       role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                       a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                         role = "button", span(class = "sr-only", "Toggle navigation")),
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   tags$li(id="logo_ministere",
                                           a(tags$i(class="fa icon_ministere text-success vert_center"),"Ministère",href="http://solidarites-sante.gouv.fr/",
                                             target="_blank")),
                                   tags$li(id="logo_drees",
                                           a(tags$i(class="fa icon_drees text-success vert_center"),"DREES",href="http://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/",
                                             target="_blank")),
                                   # https://resizeimage.net/
                                   
                                   tags$li(id="doc_click",
                                           a(tags$i(class="fa icon_carto text-success vert_center"),"Le projet",
                                             target="_blank")),
                                   
                                   tags$li(id="Github",
                                           a(tags$i(class="fa icon_github text-success vert_center"),"Code Source",href="https://github.com/phileas-condemine/carto_indicateurs",
                                             target="_blank"))
                           )))
              ,includeCSS("www/my_styles.css")
              ,includeHTML('www/cookie_handler.html')
              ,includeScript("www/hide_when_loading.js")
              
              
              
  ),
  
  dashboardSidebar(collapsed = F,
                   sidebarMenu(id="sidebarmenu",
                               menuItem(text = "Les indicateurs",icon = shiny::icon("search"),tabName="catalogue"),
                               
                               menuItem("Accueil",icon = shiny::icon("home"),tabName="Accueil"),
                               
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
      tabItem(tabName = "Accueil",
              includeHTML("www/accueil.html"),
              s2,
              slickROutput("slicker_carousel",height = "300"),
              includeHTML("www/footer_accueil.html")
      ),
      
      tabItem(tabName = "catalogue",
              
              my_value_boxesUI("valueBoxes"),
              div(class="resultats",#style="width:95%;margin-left:20px; margin-right:20px",
                  div(id="tag_div",class="col-sm-6 inbody_selector",
                      selectizeInput(inputId="tag",
                                     label = "Recherche par tags",#selected = ,
                                     choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                     multiple=T,options = list(placeholder = sprintf('Ajoutez un filtre en choisissant parmi les %s thématiques liées à la santé',length(unlist(tags_class_list))))
                      )%>%shinyInput_label_embed(
                        icon("question-circle") %>%
                          bs_embed_tooltip(title = "Choisissez une ou plusieurs thématique(s) de votre choix pour commencer à explorer le catalogue des indicateurs. Sinon vous pouvez également utiliser la recherche par mot-clef.")
                      )),
                  div(id="search_keywords_div",class="col-sm-6 inbody_selector",
                      selectizeInput(inputId="search_keywords",
                                     label = "Recherche par mot(s) clef(s)",
                                     choices = term_freq_global$word,
                                     multiple=T,options = list(placeholder = 'Entrez les mots-clefs de votre choix : ald, précarité, dépenses, handicap...')
                      )%>%shinyInput_label_embed(
                        icon("question-circle") %>%
                          bs_embed_tooltip(title = "Utilisez la barre de recherche semi-automatique pour sélectionner des mots-clefs pertinents pour explorer le catalogue des indicateurs. Les mots-clefs sont triés par fréquence.")
                      )),
                  div(id="carto_datatable",dataTableOutput("DT_to_render")),
                  conditionalPanel("(input.tag.length == 0) && (input.search_keywords.length == 0)",
                                   div(id="placeholder_datatable",
                                       tags$h3('Rechercher des indicateurs de santé'),
                                       tags$p("Utilisez l'une des barres de recherche ci-dessus pour rechercher des indicateurs de santé grâce à des mots clefs ou grâce à des catégories pré-définies (tags), pour en savoir plus rendez-vous dans l'onglet d'accueil")
                                   )
                                   # carousel
                                   )),
              includeHTML("www/footer_catalogue.html"))
    )
  )
)
