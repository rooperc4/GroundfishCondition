lwdata<-read.csv("//AKC0SS-N086/RACE_Users/chris.rooper/My Documents/Chris Work Stuff/Ecosystem Considerations contributions/2018 Contributions/L-W Residuals/AllSpeciesLWdata.csv",header=TRUE)


lwdata["YEAR"]<-round(lwdata["CRUISE"]/100,digits=1)

rmarkdown::render("EBS_GroundfishCondition.Rmd", "word_document")
rmarkdown::render("AI_GroundfishCondition.Rmd", "word_document")
