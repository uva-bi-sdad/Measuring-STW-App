library(ggplot2)
library(stringr)
library(stringi)
library(tidyr)

test <- read.csv("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/data-discovery-jan-20.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
saveRDS(test, "test.rds")

ggplot(test, aes(x = unlist(test$Data.Format)))+
  geom_bar()
