dashboardPage(
  # 
  tags$header(class = "main-header", span(class = "logo", "Portail des indicateurs de Santé V0.2"),
              tags$nav(class = "navbar navbar-static-top",
      role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
      a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
        role = "button", span(class = "sr-only", "Toggle navigation")),
      div(class = "navbar-custom-menu",
          tags$ul(class = "nav navbar-nav",
              tags$li(id="doc_click",
                      a(tags$i(class = "fa fa-book text-success"), "Le projet")),
              
              tags$li(id="Github",
                      a(tags$i(class = "fa fa-github text-success"),"Code Source", 
                        href="https://github.com/phileas-condemine/carto_indicateurs")
              )))
              )),
  # tags$header(class = "main-header"),
  # dashboardHeader(title = "Indicateurs de Santé V0.2",
  #                 # dropdownMenu(
  #                 #   type = "notifications", 
  #                 #   icon = icon("question-circle"),
  #                 #   badgeStatus = NULL,
  #                 #   headerText = "Référence",
  #                 #   notificationItem("Github", icon = icon("github"),
  #                 #                    #j'hésite avec icon("sunglasses") icon("console")
  #                 #                    href = "https://github.com/phileas-condemine/carto_indicateurs"),
  #                 #   tags$li(id="doc_click",a(tags$i(class = "fa fa-book text-success"), "Doc"))
  #                 #                     #j'hésite avec icon("sunglasses") icon("console"))
  #                 #   
  #                 #   )
  #                 div(class = "navbar-custom-menu", 
  #                     tags$ul(class = "nav navbar-nav",
  #                             tags$li(id="doc_click",a(tags$i(class = "fa fa-book text-success"), "Doc"))))
  #                 
  #                 
  #                 ),
  dashboardSidebar(#disable = TRUE
                   div(class="tagbar",
                       selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T),
                       selectInput(inputId="vars_to_show",label="Variables à afficher",selected = init_vars_to_show,choices=names(index),multiple=T)
                       # ,selectInput(inputId="vars_to_filtrer",label="Utiliser un filtre par variable",selected = NULL,choices=names(index),multiple=T)
                       #, conditionalPanel("!is.null(input.vars_to_filtrer)",lapply(input$vars_to_filtrer,function(var){
                       #   modalites=unique(to_plot()[[var]])
                       #   selectInput(inputId=paste0("filtre_",var),label=var,choices = modalites,selected = NULL,multiple = T)
                       # }))
                                          )
    ),
  dashboardBody(
    useShinyjs(),
    my_value_boxesUI("valueBoxes"),
    bsModal(id = 'startupModal', title = 'Application de cartographie des indicateurs de santé', 
            trigger = '',
            size = 'large', HTML("<i>Cette application s'adresse à tout utilisateur désireux d'indentifier des indicateurs de santé parmis les 18,885 indicateurs actuellement recensés.</i><br>
                                 Pour cela nous proposons deux critères d'exploration : 
                                 <ul>
                                 <li> Recherche par tags (parmis une liste de 34 tags)
                                 <li> Recherche par mots clefs sous la forme de texte libre
                                 </ul>
                                 Vous pouvez exporter les résultats de la recherche grâce aux boutons copy/CSV/Excel situés en bas du tableau.<br>
                                 <b> Attention cette application est en cours de développement, ne pas diffuser.")),
  # extendShinyjs(text = jsCode),
  # fluidRow(align="center",
          # div(class="tagbar",
          #     selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
          #     )
  # ),
  
div(class="resultats",style="width:95%;margin-left:20px; margin-right:20px",
      dataTableOutput("DT_to_render")
      )
  )
)




