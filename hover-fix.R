
library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(gtools)
library(shinyBS)

#https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny RADIO FIX

##### THEME COLORS #####
theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)

##### DATA #####
responses<-read.csv("data-discovery-jan-20.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

# Assign columns readable names
names(responses) <- stri_trim(gsub("..Yes.No.|i\\.e\\..+or\\.|i\\.e\\..+|\\.{53}.+|\\.+", " ", names(responses)), side = "right")


##### FORM RESPONSES DIRECTORY #####
#dir.create("form")
outputDir <- "form"

##### FORM VARIABLES #####
fields <- c("Name", "Affiliation", "Data Source Name", "Credentials", "Skills", "Jobs", "Employers", "STW Relevant", 
            "Dataset Name", "Dataset Link", "Subject", "Organization", "Data Type", "Purpose", "Audience", "Population Coverage", 
            "Unit of Analysis", "Geographic Unit", "Time Coverage", "Collection Frequency", "When does the data become available?", "Can this data be trended?", 
            "Methodology Report Link", "Data Dictionary Link", "Data Quality Assessments", "Cost/Price", "Funding amount to support R&D", "Licensing or Training Required?", 
            "Accessibility", "Data Format", "Individuals Identifiable", "Gender", "Race/Ethnicity", "Veterans", "Active Military", "Persons Who Live on Tribal Lands", 
            "Fields of Study/Types of Training", "Types of Employment/Occupations", "Notes")

field_list <- c(fields, "submit_time")

##### FEEDBACK VARIABLES #####
fields.feedback <-c("Name.feedback", "Email.feedback", "Comment.feedback")
field_feedback_list <- c(fields.feedback, "submit_time")

##### FUNCTIONS FOR SAVING AND DISPLAYING FORM INPUTS (see saving checkboxs link) #####
saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      
      data[[x]] <- list(var)
    } else {
      data[[x]] <- var
    }
  }
  
  data$submit_time <- date()
  
  
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}


loadData <- function() {
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    data <- do.call(smartbind, data)
  }
  
  data
}


##### FEEDBACK RESPONSES DIRECTORY #####
#dir.create("feedback")
outputDir.feedback <- "feedback"

##### FUNCTIONS FOR SAVING AND DISPLAYING FEEDBACK INPUTS #####
saveData.feedback <- function(data) {
  data <- t(data)
  names(data) <- fields.feedback
  data$submit_time <- date()
  
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  write.csv(
    x = data,
    file = file.path(outputDir.feedback, fileName), 
    row.names = FALSE, 
    quote = FALSE
  )
}

##### UI #####
ui <- fluidPage(
  
  title = "Data Discovery",
  titlePanel( 
    fluidRow(
      column(3, img(height = 51.65, width = 243.3, src = "BII.jpg")),
      column(9, h1("Data Discovery - Skilled Technical Workforce", style = "font-weight: bold; font-size: 24pt; "))
    ) 
  ),
  
  mainPanel(
    tabsetPanel(
      id = 'dataset',

      tabPanel("Data Sources",
               #this puts the sidebar visible only for first tab 
               sidebarPanel(
                 checkboxGroupInput("show_vars", "Columns in Data Sources to show:", 
                                    choiceNames=stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"), 
                                    choiceValues = names(responses),
                                    selected=names(responses)) , 
                 width=3), 
               mainPanel( DT::dataTableOutput("mytable1")))
    )
  )
)



##### SERVER #####
server <- function(input, output, session) {
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE],  extensions = 'Buttons', filter = "top",
                  options = list(buttons = list(list(extend='csv',
                                                     filename = 'STW-Data-Discovery'),
                                                list(extend='excel',
                                                     filename = 'STW-Data-Discovery')), dom="BlfrtipS",iDisplayLength=-1,fixedColumns = TRUE))
  })
  

 

  
}

shinyApp(ui, server)


