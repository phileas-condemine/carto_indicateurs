dashboardPage(

  tags$header(class = "main-header", span(class = "logo",style="background: #1263b3;", "Portail des indicateurs de Santé V0.5"),
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
              ))),
      includeCSS("www/my_styles.css"),
      includeHTML('www/cookie_handler.html')
      
      ),

  dashboardSidebar(collapsed = T,
                   sidebarMenu(id="sidebarmenu",
                    menuItem("Recherche et filtres",icon = shiny::icon("search"),
        # div(id="tags_select_bar",
        # tags$i(class="fa fa-gears"),
    #     selectizeInput(inputId="tag",
    #                 label = "Recherche par tags",#selected = ,
    #                  choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
    #                 multiple=T
    #     ),#https://stackoverflow.com/questions/49723322/how-to-bookmark-a-shiny-selectizeinput-with-dynamic-options
    #     # searchInput(inputId = "search_keywords",label = "Recherche par mot(s) clef(s)",
    #     #             placeholder = "Ecrivez puis appuyez sur Entrée",btnSearch= icon("search")),
    # # div(id="vars_select_bar",
    # selectizeInput(inputId="search_keywords",
    #                label = "Recherche par mot(s) clef(s)",#selected = ,
    #                choices = "",#vide par défaut
    #                #selectize = F,size = length(tag_names)+5,
    #                multiple=T
    # ),
        selectInput(inputId="vars_to_show",label="Variables à afficher",
                        selected = init_vars_to_show,choices=names(index),multiple=T
        ))),
    includeHTML("logos.html")


      ),
      
  dashboardBody(
    useShinyjs(),
    extendShinyjs(script = "jsCode.js"),
    navlistPanel(id="dashboard_body",selected = "catalogue",widths = c(4,12),
      tabPanel(title="Accueil",value = "accueil",
          includeHTML("accueil.html"),
          div(id="go_to_catalog")
          ,includeHTML("footer_accueil.html")
    ),
       
      tabPanel(title="Catalogue",value = "catalogue",
          my_value_boxesUI("valueBoxes"),
          div(class="resultats",#style="width:95%;margin-left:20px; margin-right:20px",
              div(id="tag_div",class="col-sm-6 inbody_selector",
                  selectizeInput(inputId="tag",
                                        label = "Recherche par tags",#selected = ,
                                        choices = tags_class_list,#selectize = F,size = length(tag_names)+5,
                                        multiple=T,options = list(placeholder = 'choix parmi les 32 tags')
                           )),
               div(id="search_keywords_div",class="col-sm-6 inbody_selector",
                   selectizeInput(inputId="search_keywords",
                              label = "Recherche par mot(s) clef(s)",
                              choices = term_freq_global$word,
                              multiple=T,options = list(placeholder = 'ald, précarité, dépenses...')
                           )),
          # withSpinner(
            div(id="placeholder_datatable",dataTableOutput("DT_to_render"))
            # ,size = 2)
          ),
          includeHTML("footer_catalogue.html"))
  )

)


)




