#https://shiny.rstudio.com/articles/layout-guide.html LAYOUT
# will need to figure out form saving
library(shiny)
library(DT)
library(ggplot2)  # for the diamonds dataset
responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
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
      column(3, img(height = 100, width = 150, src = "UVA centered.jpg")),
      column(9, "Data Discovery - Skilled Technical Workforce") 
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      
      
      checkboxGroupInput("show_vars", "Columns in Datasets to show:",choiceNames=gsub(names(responses),pattern="\\.",replacement=" "), choiceValues=names(responses),selected = names(responses)) 
      ,width=2),
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Datasets", DT::dataTableOutput("mytable1")), 
        tabPanel("Plot", fluidRow( column(12,
          splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2")), 
          fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph3"), plotOutput("plotgraph4"))))))), 
        tabPanel("Dictionary", 
                 tags$h1(tags$b("This is where the data dictionary goes!")), tags$b("test")),
        tabPanel("Form", 
                 textInput("name", "Name", ""))
      )
    )
  )
)

server <- function(input, output) {
  
  responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
  # choose columns to display
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE],  extensions = 'Buttons',
                  filter = "top",
                  options = list(buttons = list(list(extend='csv',
                                                     filename = 'STW-Data-Discovery'),
                                                list(extend='excel',
                                                     filename = 'STW-Data-Discovery')),dom="BlfrtipS",iDisplayLength=-1,fixedColumns = TRUE))
  })
  
  output$plotgraph1 <- renderPlot({
    
    ggplot(responses, aes(x = Credentials..Yes.No.))+ 
      geom_bar()+ 
      theme_minimal()}, height = 400,width = 400) 
  
  
  output$plotgraph2 <- renderPlot({
    
    ggplot(responses, aes(x = Skills..Yes.No.))+ 
      geom_bar()+
      theme_minimal()}, height = 400,width = 400)
  output$plotgraph3 <- renderPlot({
    
    ggplot(responses, aes(x = Jobs..Yes.No.))+ 
      geom_bar()+
      theme_minimal()}, height = 400,width = 400)
  output$plotgraph4 <- renderPlot({
    
    ggplot(responses, aes(x = Employers..Yes.No.))+ 
      geom_bar() +
      theme_minimal()}, height = 400,width = 400)
  
}

shinyApp(ui, server)