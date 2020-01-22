library(shiny)
library(DT)
library(stringi)
library(gtools)
library(ggplot2)

##### DATA #####
responses<-read.csv("data-discovery-jan-20.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

# Assign columns readable names
names(responses) <- stri_trim(gsub("..Yes.No.|i\\.e\\..+or\\.|i\\.e\\..+|\\.{53}.+|\\.+", " ", names(responses)), side = "right")

theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)


ui <- fluidPage(
  titlePanel('Test Select Graph'),
  mainPanel(column(2,
    uiOutput("filter_vars"),
    uiOutput("select_vars")
  ),
  column(10,
    uiOutput("plot")
  )
  )
)


server <- function(input, output, session) {
  output$filter_vars<-renderUI({
    radioButtons("rd","Select Option",choices = c("One Variable","Two Variables"),
                 selected = "One Variable")
  })
  
  output$select_vars <- renderUI({
    if(input$rd == "One Variable"){ fluidRow(
      selectInput("category1", "Variable", choices=c("Credentials","Jobs", "Employers", "Skills", "Organization Type", "Gender" )))
    }
    else if(input$rd == "Two Variables"){fluidRow(
      selectInput("category2", "Variable 1", choices=c("Credentials","Jobs", "Employers", "Skills", "Organization Type", "Gender" )),
      selectInput("category3", "Variable 2", choices=c("Credentials","Jobs", "Employers", "Skills", "Organization Type", "Gender" )))
    }
  })
  
  
  
  
  
  output$plot <- renderUI({

    if(input$rd=="One Variable"){
      
      output$plot1<-renderPlot({
        # Boxplots of mpg by number of gears 
        # observations (points) are overlayed and jittered
        ggplot(responses, aes(x =responses[ , input$category1], fill =responses[ , input$category1]))+ 
          scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4]))+
          geom_bar() +
          theme_minimal() +
          labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
          theme(
            legend.position = "none", 
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18), 
            axis.title.x = element_text(size = 18), 
            axis.title.y = element_text(size = 18))
      }, height = 600, width = 800)
      plotOutput("plot1")
    }
    
    
    else if(input$rd=="Two Variables"){
     
      output$plot2<-renderPlot({
        # Boxplots of mpg by number of gears 
        # observations (points) are overlayed and jittered
       
        ggplot(responses, aes(x =responses[ , input$category2], fill =responses[ , input$category3]))+ 
          scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4]))+
          geom_bar() +
          theme_minimal() +
          labs(title = paste("Data Sources Containing", input$category3, "Data by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3) ) +
          theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18), 
            axis.title.x = element_text(size = 18), 
            axis.title.y = element_text(size = 18))
      }, height = 600, width = 800)
      plotOutput("plot2")
    }
 
  })
  
}


  

shinyApp(ui = ui, server = server)

