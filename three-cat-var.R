
library(vcd)



mosaic(~Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c("#7D9ACA", "#31589B", "#31589B", theme_Palette[1], "#31589B", theme_Palette[1], theme_Palette[1], "#0B1931")))


mosaic(~Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c()))


mosaic(~Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c("#9fd1ca", "#6b9caa", "#6b9caa", "#426888", "#6b9caa", "#426888", "#426888", "#1b3766")))


#1b3766,#426888,#6b9caa,#9fd1ca
mosaic(~Skills + Jobs + Credentials, data = responses, main = paste("Data Containing ", "Skills", ", ", "Jobs", ", and ", "Credentials", sep = ""), 
       shade = T,  gp = gpar(fill = c("#72dbc7", "#58a0a6", "#58a0a6", "#3c6a86", "#58a0a6", "#3c6a86", "#3c6a86", "#1b3766")))

#1b3766,#3c6a86,#58a0a6,#72dbc7
cotabplot(~ Skills + Jobs | Credentials, data = responses, split_vertical = TRUE, gp = gpar(fill = c(theme_Palette[1], theme_Palette[5])))
