#https://shiny.rstudio.com/articles/layout-guide.html LAYOUT
#https://shiny.rstudio.com/articles/persistent-data-storage.html PERSISTENT STORAGE
# https://stat.ethz.ch/pipermail/r-help/2017-June/447450.html This is for styling RMarkdown file
# https://gupsych.github.io/tquant/data-input.html saving checkbox group

library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(gtools)



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
      # all other data types
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
      column(9, h1("Data Discovery - Skilled Technical Workforce", style = "font-weight: bold; font-size: 30pt;"))
    )
  ),
  
  
  #this puts the side bar visible for all tabs
  #sidebarLayout(
  # sidebarPanel(
  
  
  #  checkboxGroupInput("show_vars", "Columns in Datasets to show:",choiceNames=gsub(names(responses),pattern="\\.",replacement=" "), choiceValues=names(responses),selected = names(responses)) 
  # ,width=2),
  
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel( "About", includeMarkdown("welcome-page.Rmd")),
      tabPanel("Data Sources",
               #this puts the sidebar visible only for first tab 
               sidebarPanel(
                 checkboxGroupInput("show_vars", "Columns in Data Sources to show:", 
                                    choiceNames=stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"), 
                                    choiceValues = names(responses),
                                    selected=names(responses)) , 
                 width=3), 
               mainPanel( DT::dataTableOutput("mytable1"))), 
      #static plots
      # tabPanel("Plot", fluidRow( column(12,
      #  splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2")), 
      #  fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph3"), plotOutput("plotgraph4"))))))), 
      #choose plots
      tabPanel( "Plot", 
                sidebarPanel(
                  selectInput("category", "Category", choices=c("Credentials","Jobs", "Employers", "Skills", "Organization Type" ))),
                #selectInput("category", "Category", choices=c("Credentials..Yes.No.", "Jobs..Yes.No.", "Employers..Yes.No.", "Skills..Yes.No.", "Organization.Type" ))), 
                plotOutput("Plot") ), 
      #we can make the Dictionary a Markdown file or HTML rather than typing it in R
      tabPanel("Dictionary", includeMarkdown("data-dictionary.Rmd")),
      tabPanel("Form", DT::dataTableOutput("form", width = 300), tags$hr(),
               fluidRow( column(4, textInput("Name", "Name", ""), 
                                textInput("Affiliation", "Affiliation", ""),
                                textInput("Data Source Name", "Data Source Name", ""),
                                radioButtons("Credentials", "Credentials", choices = list("Yes", "No")),
                                radioButtons("Skills", "Skills", choices = list("Yes", "No")),
                                radioButtons("Jobs", "Jobs", choices = list("Yes", "No")),
                                radioButtons("Employers", "Employers", choices = list("Yes", "No")),
                                textAreaInput("STW Relevant", "STW Relevant", ""),
                                textInput("Dataset Name", "Dataset Name", ""), 
                                textInput("Dataset Link", "Dataset Link", ""), 
                                checkboxGroupInput("Subject", 
                                                   "Subject", 
                                                   choices = list("Education/Training", "Licenses/Certifications", 
                                                                  "Jobs/Employment", "Industry"
                                                   )),
                                radioButtons("Organization", "Organization", choices = list("Non-Profit", "Federal", 
                                                                                            "For-Profit")),
                                checkboxGroupInput("Data Type", "Data Type", choices = list("Administrative", "Opportunity", 
                                                                                            "Procedural", "Designed (Survey)"))),
                         column(4,  textAreaInput("Purpose", "Purpose", ""), 
                                textInput("Audience", "Audience", ""), 
                                textInput("Population Coverage", "Population Coverage", ""),
                                textInput("Unit of Analysis", "Unit of Analysis", ""), 
                                checkboxGroupInput("Geographic Unit", "Geographic Unit", choices = list("National", "State", "City", "Zip Code", "Census Block", "Census Tract")), 
                                textInput("Time Coverage", "Time Coverage", ""),
                                radioButtons("Collection Frequency", "Collection Frequency", choices = list("Annual", "Monthly", "Biennial", "Daily", "Real-time", "Quarterly", "One-time")), 
                                textInput("When does the data become available?", "When does the data become available?", ""),
                                radioButtons("Can this data be trended", "Can this data be trended?", choices = list("Yes", "No")), 
                                textInput("Methodology Report Link", "Methodology Report Link", ""), 
                                textInput("Data Dictionary Link", "Data Dictionary Link", ""), 
                                textAreaInput("Data Quality Assessments", "Data Quality Assessments", ""), 
                                textInput("Cost/Price", "Cost/Price", ""), 
                                textInput("Funding amount to support R&D", "Funding amount to support R&D", ""), 
                                radioButtons("Licensing or Training Required?", "Licensing or Training Required?", choices = list("Yes", "No"))), 
                         column(4, 
                                checkboxGroupInput("Accessibility", "Accessibility", choices = list("API", "Download", "FTP", "Portal", "Webscraping")), 
                                checkboxGroupInput("Data Format", "Data Format", choices = list("CSV", "Excel", "TXT", "PDF", "JSON", "SAS", "R", "SPSS")), 
                                radioButtons("Individuals Identifiable", "Individuals Identifiable", choices = list("Yes", "No")), 
                                radioButtons("Gender", "Gender", choices = list("Yes", "No")), 
                                radioButtons("Race/Ethnicity", "Race/Ethnicity", choices = list("Yes", "No")), 
                                radioButtons("Persons with Disabilities", "Persons with Disabilities", choices = list("Yes", "No")), 
                                radioButtons("Veterans", "Veterans", choices = list("Yes", "No")), 
                                radioButtons("Active Military", "Active Military", choices = list("Yes", "No")), 
                                radioButtons("Persons Who Live on Tribals Lands", "Persons Who Live on Tribal Lands", choices = list("Yes", "No")), 
                                radioButtons("Fields of Study/Types of Training", "Fields of Study/Types of Training", choices = list("Yes", "No")), 
                                radioButtons("Types of Employment/Occupations", "Types of Employment/Occupations", choices = list("Yes", "No")))),
               textAreaInput("Notes", "Notes", ""),
               actionButton("submit", "Submit", style="border-color: #F17E1D; font-size: 20px; padding: 16px 16px;")), 
      tabPanel("Feedback", 
               textInput("Name.feedback", "Name", ""), textInput("Email.feedback", "Email", ""), 
               textAreaInput("Comment.feedback", "Comment", ""),
               actionButton("submit.feedback", "Submit", style="border-color: #F17E1D; font-size: 20px; padding: 16px 16px;"))
    )
  )
    )

#)

##### SERVER #####
server <- function(input, output, session) {
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE],  extensions = 'Buttons',
                  #colnames = stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"),
                  #colnames = c("Data Source Name" = "Data.Source.Name", paste(names(responses)[2:38])),
                  filter = "top",
                  # style = "bootstrap", 
                  # class = "table-hover",
                  
                  options = list(buttons = list(list(extend='csv',
                                                     filename = 'STW-Data-Discovery'),
                                                list(extend='excel',
                                                     filename = 'STW-Data-Discovery')),dom="BlfrtipS",iDisplayLength=-1,fixedColumns = TRUE))
  })
  #static plots
  #output$plotgraph1 <- renderPlot({
  
  # ggplot(responses, aes(x = Credentials..Yes.No.))+ 
  #  geom_bar()+ 
  # theme_minimal()}, height = 400,width = 400) 
  
  
  #  output$plotgraph2 <- renderPlot({
  
  #   ggplot(responses, aes(x = Skills..Yes.No.))+ 
  #    geom_bar()+
  #   theme_minimal()}, height = 400,width = 400)
  #output$plotgraph3 <- renderPlot({
  
  # ggplot(responses, aes(x = Jobs..Yes.No.))+ 
  #  geom_bar()+
  # theme_minimal()}, height = 400,width = 400)
  #output$plotgraph4 <- renderPlot({
  
  # ggplot(responses, aes(x = Employers..Yes.No.))+ 
  #  geom_bar() +
  # theme_minimal()}, height = 400,width = 400)
  #choose plots
  output$Plot <- renderPlot({
    
    ggplot(responses, aes(x =responses[ ,input$category], fill =responses[ ,input$category]))+ 
      scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4]))+
      geom_bar() +
      theme_minimal() +
      labs(title = input$category, y = "Number of Sources", x = "") +
      theme(
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18))}, height = 600, width = 800) 
  
  
  ##### FORM DATA #####
  
  formData <- reactive({
    data <- sapply(field_list, function(x) input[[x]])
    data
  })
  
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  output$form <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    
    loadData()
  })
  
  
  ##### FEEDBACK DATA #####
  formData.feedback <- reactive({
    data <- sapply(fields.feedback, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit.feedback, {
    saveData.feedback(formData.feedback())
  })
  
  
  
}

shinyApp(ui, server)


