#https://shiny.rstudio.com/articles/layout-guide.html LAYOUT
#https://shiny.rstudio.com/articles/persistent-data-storage.html PERSISTENT STORAGE

library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)

##### THEME COLORS #####
theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)

##### DATA #####
responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

##### FORM RESPONSES DIRECTORY #####
#dir.create("form")
outputDir <- "form"

##### FUNCTIONS FOR SAVING AND DISPLAYING FORM INPUTS #####
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  data <- do.call(rbind, data)
  data
}

##### FORM VARIABLES #####
fields <- c("Name", "Affiliation", "Data Source Name", "Credentials", "Skills", "Jobs", "Employers", "STW Relevant")

##### UI #####
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    
                    label {
                    
                    }
                    
                    "))
    ),
  
  
  
  title = "Data Discovery",
  titlePanel(
    fluidRow(
      column(3, img(height = 51.65, width = 243.3, src = "BII.jpg")),
      column(9, "Data Discovery - Skilled Technical Workforce") 
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
      tabPanel("Datasets",
               #this puts the sidebar visible only for first tab 
               sidebarPanel(
                 checkboxGroupInput("show_vars", "Columns in Datasets to show:", 
                                    choiceNames=stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"), 
                                    choiceValues = names(responses),
                                    selected=names(responses)) , 
                 width=2), 
               mainPanel( DT::dataTableOutput("mytable1"))), 
      #static plots
      # tabPanel("Plot", fluidRow( column(12,
      #  splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2")), 
      #  fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph3"), plotOutput("plotgraph4"))))))), 
      #choose plots
      tabPanel( "Plot", 
                sidebarPanel(
                  selectInput("category", "Category", choices=c("Credentials" = "Credentials..Yes.No.", "Jobs"="Jobs..Yes.No.", "Employers"="Employers..Yes.No.", "Skills"="Skills..Yes.No.", "Organization Type" ="Organization.Type" ))),
                #selectInput("category", "Category", choices=c("Credentials..Yes.No.", "Jobs..Yes.No.", "Employers..Yes.No.", "Skills..Yes.No.", "Organization.Type" ))), 
                plotOutput("Plot") ), 
      #we can make the Dictionary a Markdown file or HTML rather than typing it in R
      tabPanel("Dictionary", 
               tags$h1(tags$b("This is where the data dictionary goes!")), tags$b("test")),
      tabPanel("Form", DT::dataTableOutput("form", width = 300), tags$hr(),
               textInput("name", "Name", ""), 
               textInput("Affiliation", "Affiliation", ""),
               textInput("Data Source Name", "Data Source Name", ""),
               radioButtons("Credentials", "Credentials", choices = list("Yes" = "Yes", "No" = "No")),
               radioButtons("Skills", "Skills", choices = list("Yes" = "Yes", "No" = "No")),
               radioButtons("Jobs", "Jobs", choices = list("Yes" = "Yes", "No" = "No")),
               radioButtons("Employers", "Employers", choices = list("Yes" = "Yes", "No" = "No")),
               textAreaInput("STW Relevant", "STW Relevant", ""),
               actionButton("submit", "Submit"))
    )
  )
    )
#)

##### SERVER #####
server <- function(input, output, session) {
  
  responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
  # choose columns to display
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE],  extensions = 'Buttons',
                  colnames = stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"),
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
      labs(title = stri_trim(gsub(input$category,pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"), y = "Number of Sources", x = "")+
      theme(
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18))}, height = 600, width = 800) 
  
  
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$form <- DT::renderDataTable({
    input$submit
    loadData()
  }) 
  
}

shinyApp(ui, server)

