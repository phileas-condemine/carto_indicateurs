dashboardPage(
  dashboardHeader(title = "Indicateurs de Santé",
                  dropdownMenu(
                    type = "notifications", 
                    icon = icon("question-circle"),
                    badgeStatus = NULL,
                    headerText = "Référence",
                    notificationItem("Github", icon = icon("flash"),
                                     #j'hésite avec icon("sunglasses") icon("console")
                                     href = "https://github.com/phileas-condemine/carto_indicateurs")
                  )
                  ),
  dashboardSidebar(disable = TRUE
    # div(class="tagbar",
    #     selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
    # )
  ),
  dashboardBody(
    useShinyjs(),
    includeCSS("my_styles.css"),

  # extendShinyjs(text = jsCode),
  fluidRow(align="center",
          div(class="tagbar",
              selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
              ))
,
  
div(class="resultats",style="width:95%;margin-left:20px; margin-right:20px",
      dataTableOutput("DT_to_render")
      )
  )
)

