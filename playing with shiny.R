ui <- shinyUI(bootstrapPage(
  shinyDirButton('files', 'File select', 'Please select a file', FALSE),
  verbatimTextOutput('rawInputValue'),
  verbatimTextOutput('filepaths')
))
server <- shinyServer(function(input, output) {
  root = c(dir = '~')
  shinyDirChoose(input, 'files', roots = root)
  output$rawInputValue <- renderPrint({str(input$files)})
  path <- renderPrint({parseDirPath(root, input$files)})
  output$filepaths <- path

  path <- reactive({
    path <- input$files
  })

  observe({
    print(unlist(path()))
  })
})

runApp(list(
  ui=ui,
  server=server
))

# Delete after you figure out the best way to do export_dataset
