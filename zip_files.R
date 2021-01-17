library(shiny)
# ui.R
ui <- shinyUI(fluidPage(
  titlePanel('Downloading Data'),
  sidebarLayout(
    sidebarPanel(
      downloadButton('downloadData', 'Download')
    ),
    mainPanel()
  )
)
)
# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
    return(list(rock=rock, pressure=pressure, cars=cars))
  })
  
  output$downloadData <- downloadHandler(
    filename = 'pdfs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("rock.csv", "pressure.csv", "cars.csv")
      write.csv(datasetInput()$rock, file = "rock.csv", sep =",")
      write.csv(datasetInput()$pressure, file = "pressure.csv", sep =",")
      write.csv(datasetInput()$cars, file = "cars.csv", sep =",")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  )
  
}



shinyApp(ui = ui, server = server)

##### Read a Zip file #####
library(plyr)

# get all the zip files
zipF <- list.files(path = "/Users/manav/Desktop/apps_16012020/Amazing_App/as/", pattern = "*.zip", full.names = TRUE)

unzip("Users/manav/Desktop/apps_16012020/Amazing_App/as")
file_names <- list.files(pattern = "\\.csv$", full.names = TRUE)
list_files <- lapply(file_names, read.csv, sep = "|")
names(list_files) <- sub("\\.csv", "", basename(file_names))
list2env(list_files, .GlobalEnv)
