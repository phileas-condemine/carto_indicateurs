dashboardPage(
  dashboardHeader(title = "Indicateurs de Santé",
                  dropdownMenu(
                    type = "notifications", 
                    icon = icon("question-circle"),
                    badgeStatus = NULL,
                    headerText = "Référence",
                    notificationItem("Github", icon = icon("github"),
                                     #j'hésite avec icon("sunglasses") icon("console")
                                     href = "https://github.com/phileas-condemine/carto_indicateurs"),
                    tags$li(id="doc_click",a(tags$i(class = "fa fa-book text-success"), "Doc"))
                                      #j'hésite avec icon("sunglasses") icon("console"))
                    
                    )
                  
                  ),
  dashboardSidebar(disable = TRUE
    # div(class="tagbar",
    #     selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
    # )
  ),
  dashboardBody(
    useShinyjs(),
    my_value_boxesUI("valueBoxes"),
    bsModal(id = 'startupModal', title = 'Introduction', trigger = '',
            size = 'large', HTML("Cette application s'adresse à tout utilisateur désireux d'indentifier des indicateurs de santé parmis les 18,885 indicateurs actuellement recensés. 
                                 Pour cela nous proposons deux critères d'exploration : ")),
  # extendShinyjs(text = jsCode),
  fluidRow(align="center",
          div(class="tagbar",
              selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
              )
  )
,
  
div(class="resultats",style="width:95%;margin-left:20px; margin-right:20px",
      dataTableOutput("DT_to_render")
      )
  )
)

