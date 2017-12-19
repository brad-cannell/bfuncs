library(shiny)
library(miniUI)
library(purrr)
library(shinyFiles)
library(readr)
library(writexl)
library(feather)

data("mtcars")

export_dataset <- function() {

  # ===========================================================================
  # Grab data frames to select for export
  # 1. Create a logical vector that correspondes to objects in the global
  #    environment that are data frames.
  # 2. Create a list of data frame object names.
  # ===========================================================================
  df_index <- mget(ls(.GlobalEnv), .GlobalEnv) %>% # 1
    purrr::map_lgl(is.data.frame)

  data_frames <- names(df_index)[df_index == TRUE] # 2


  # ===========================================================================
  # User interface
  # Select the name of the data frame to export
  # List of choices comes from data_frames above
  # ===========================================================================
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Export Data Frame"),
    miniUI::miniContentPanel(

      # User selects data frame
      shiny::selectInput("df", "Select Data Frame:", data_frames),

      # User selects export format for data frame
      shiny::selectInput("format", "Select Export Format:", c("csv", "excel")),

      # User selects path to folder export target folder
      shinyFiles::shinyDirButton('folder', 'Folder select',
                                 'Please select a destination folder', FALSE),

      # ShinyFiles stuff not working at the moment. For now, just enter file
      # path manually.
      shiny::textInput("path", "Enter file path:", value = "~/"),

      # User enters name for exported file
      shiny::textInput("file_name", "Enter File Name:"),

      # For data checking
      verbatimTextOutput('folder')
      # verbatimTextOutput('export_preview')
    )
  )


  # ===========================================================================
  # Server function
  # The user selects the data frame that they want to export
  # That selection is passed below along with the format they want to export to
  # When the user clicks "done" the export is carried out
  # ===========================================================================
  server <- function(input, output) {

    # Receive the folder path from shinyDirButton
    # Paste together into a full file path to be passed to "write" function
    # below
    # ShinyFiles stuff not working at the moment. For now, just enter file
    # path manually.
    root <- c(dir = '~')
    shinyFiles::shinyDirChoose(input, 'folder', roots = root)
    path <- shiny::renderText({parseDirPath(root, input$folder)})
    output$folder <- path
    print(path)
    # path <- reactive({
    #
    # })

    # Execute when user clicks "done"
    shiny::observeEvent(input$done, {

      # Conditional logic
      # Export CSV or Excel depending on user selection
      if (input$format == "csv") {
        readr::write_csv(
          x    = get(input$df, envir = .GlobalEnv),
          path = "/Users/bradcannell/Desktop/mtcars.csv"
        )
      }
      shiny::stopApp()
    })
  }

  shiny::runGadget(ui, server,
    viewer = dialogViewer("Export Data Frame", height = 650)
  )
}

export_dataset()


# Left off trying to figure out how to convert the return value from the folder
# select button to a string that can be used to create a the file path used
# in write_csv
# Take a look at the RStudio webinar
https://www.rstudio.com/resources/webinars/how-to-start-with-shiny-part-2/

# Figure out how to let user select the path with a browse file pach button.
# Use modal dialogue box instead of viewer window.
# Make pretty by adding spaces https://github.com/rstudio/shiny/issues/1303
# Create a file path preview
