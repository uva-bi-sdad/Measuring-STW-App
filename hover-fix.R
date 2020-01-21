library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(gtools)
library(shinyBS)

##### THEME COLORS #####
theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)

##### DATA #####
responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

# Assign columns readable names
names(responses) <- stri_trim(gsub("\\.|\\.\\.Yes\\.No\\.", " ", names(responses)), side = "right")




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
      
      tabPanel("Datasets",
               #this puts the sidebar visible only for first tab 
               sidebarPanel(
                 checkboxGroupInput("show_vars", "Columns in Datasets to show:", 
                                    choiceNames=stri_trim(gsub(names(responses),pattern=("\\.|\\.\\.Yes\\.No\\."),replacement=" "), side = "right"), 
                                    choiceValues = names(responses),
                                    selected=names(responses)  ) , 
                 width=3
                 ), 
               
               mainPanel( DT::dataTableOutput("mytable1"))))))
      #we can make the Dictionary a Markdown file or HTML rather than typing it in R
 

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


  
  
}

shinyApp(ui, server)




