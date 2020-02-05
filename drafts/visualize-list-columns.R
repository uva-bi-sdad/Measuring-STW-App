library(ggplot2)
library(stringr)
library(stringi)
library(tidyr)
library(dplyr)

test <- read.csv("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/data-discovery-feb-3.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")

test <- test %>% select(Data.Source.Name, Dataset.Name, "Subject")
test[colnames(test) == "Subject"] <- trimws(test[, "Subject"])


test[["Subject"]] <- as.list(str_split(test[["Subject"]], ", "))

vars <- unique(unlist(test$Data.Type))


test[, "Subject"]<- list(test[, "Subject"])


test2 <-test %>% unnest(Subject) %>% 
group_by(Data.Source.Name, Dataset.Name)
#%>% 
#spread(key=Data.Type, value=Data.Type)

for(i in 1:length(vars)){
  i <- vars[i]

test2[is.na(test2[, i]) == FALSE, i] <- "Yes"
test2[is.na(test2[, i]) == TRUE, i] <- "No"
}





ggplot(test2, aes(x = test2$Subject, fill = test2$Subject))+
  geom_bar()

ggplot(test2, aes(x =test2$Skills..Yes.No., fill = test2$Data.Type))+ 
 # scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4], theme_Palette[2]))+
  geom_bar(width = .66) +
  theme_minimal() +
  #labs(title = paste("Data Sources Containing", input$category3, "Data by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3) ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), 
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18))



check <- responses

check <- responses %>% select(`Data Source Name`, `Dataset Name`, Subject)

check[["Subject"]] <- trimws(check[["Subject"]])

`$`(check, "Subject") <- as.list(str_split(`$`(check, "Subject"), ", "))


check <- check %>% unnest("Subject") %>% group_by(`Data Source Name`, `Dataset Name`)



ggplot(check, aes(x = check[ , "Subject"] , fill = check[ , "Subject"] ))+
  geom_bar()






