fluidPage(
  useShinyjs(),
  # extendShinyjs(text = jsCode),
  includeCSS("my_styles.css"),
  fluidRow(align="center",
          div(class="tagbar",
              selectInput(inputId="tag",label = "Recherche par tags",choices = tag_names,multiple=T)
              )
,
  
  fluidRow(div(class="resultats",
      dataTableOutput("DT_to_render")
      )
  ),
  div(class="footer",
    a(
      img(src="GitHub-Mark.png",width="50px",heigth="50px")
      ,href="https://github.com/phileas-condemine/carto_indicateurs"
      ,target="_blank"
    )
  )
  )
  

)
