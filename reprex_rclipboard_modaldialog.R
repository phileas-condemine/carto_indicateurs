library(rclipboard)
library(shiny)

# The UI
ui <- bootstrapPage(
  
  rclipboardSetup(),
  
  # Add a text input
  textInput("copytext", "Copy this:", "Zlika!"),
  
  actionButton("go_modal","Through modal dialog",icon=icon("link")),
  uiOutput("clip"),
  # A text input for testing the clipboard content.
  textInput("paste", "Paste here:")
  
)

# The server
server <- function(input, output) {
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "rclipButton Copy", input$copytext, icon("clipboard"))
  })
  output$clip2 <- renderUI({
    rclipButton("clipbtn", "rclipButton Copy", input$copytext, icon("clipboard"))
  })
  observeEvent(input$go_modal,{
    
    showModal(modalDialog(title="Click to get the text",
                          footer=NULL, easyClose = T,   # UI ouputs for the copy-to-clipboard buttons
      uiOutput("clip2")))
  })
}

shinyApp(ui = ui, server = server)
