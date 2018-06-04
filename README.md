Condition indices calculated for groundfish species from length-weight residuals
================
Chris Rooper, Ebbet Siddon, Jennifer Boldt, Jerry Hoff
June 4, 2018

PURPOSE
-------

The purpose of this package is to provide a set of functions and a template for R Markdown documents for the computation of groundfish condition in Alaska for the ecosystem considerations chapter of the SAFE. The functions can also be used to calculate L-W residuals for other purposes.

DATA
----

The example data sets given here are for the EBS, AI and GOA through the 2017 bottom trawl survey years. However, other surveys that collect similar data (e.g. EcoFOCI surveys, RPA surveys and MACE-AT surveys) can use these functions as well. The r markdown script was prepared by E. Siddon and the functions and package compiled by C. Rooper. A more detailed description of the analyses and ecological relevance can be found in a draft manuscript (Boldt, Rooper and Hoff, in prep, that should be completed in late 2018).

The example data set included in this package is called lwdata and is specimen data (length and weight specimens) for all species where this data was collected on RACE bottom trawl surveys. The data was initially extracted from RACEBASE tables (including HAUL, SPECIMEN, SPECIES and STRATA tables). Other sources of data work fine as well, the key points are to have individual lengths and weights, a species identifier, a strata identifier (if you want some sort of by-area calculations) and a year. Here is what the RACEBASE data look like.

``` r
data("lwdata")
#create a year variable from the cruise data
lwdata["YEAR"]<-round(lwdata["CRUISE"]/100,digits=1)
pander::pandoc.table(lwdata[1:6,])
```

    ## 
    ## -----------------------------------------------------------------------------
    ##  REGION   CRUISE   VESSEL   HAUL       START_TIME       INPFC_AREA   STRATUM 
    ## -------- -------- -------- ------ -------------------- ------------ ---------
    ##   GOA     200501    143      1     5/21/2005 15:34:16    Shumagin      210   
    ## 
    ##   GOA     200501    143      1     5/21/2005 15:34:16    Shumagin      210   
    ## 
    ##   GOA     200501    143      1     5/21/2005 15:34:16    Shumagin      210   
    ## 
    ##   GOA     200501    143      1     5/21/2005 15:34:16    Shumagin      210   
    ## 
    ##   GOA     200501    143      2     5/21/2005 18:51:09    Shumagin      10    
    ## 
    ##   GOA     200501    143      2     5/21/2005 18:51:09    Shumagin      10    
    ## -----------------------------------------------------------------------------
    ## 
    ## Table: Table continues below
    ## 
    ##  
    ## --------------------------------------------------------------------------
    ##  STATIONID   BOTTOM_DEPTH   START_LATITUDE   START_LONGITUDE   SPECIMENID 
    ## ----------- -------------- ---------------- ----------------- ------------
    ##     3-6          219            52.56            -169.8            3      
    ## 
    ##     3-6          219            52.56            -169.8            8      
    ## 
    ##     3-6          219            52.56            -169.8            3      
    ## 
    ##     3-6          219            52.56            -169.8            5      
    ## 
    ##     3-8           82            52.64            -169.8            29     
    ## 
    ##     3-8           82            52.64            -169.8            30     
    ## --------------------------------------------------------------------------
    ## 
    ## Table: Table continues below
    ## 
    ##  
    ## ------------------------------------------------------------------------------
    ##  SPECIES_CODE          SPECIES_NAME                  COMMON_NAME          SEX 
    ## -------------- ----------------------------- --------------------------- -----
    ##     21921       Pleurogrammus monopterygius         Atka mackerel          1  
    ## 
    ##     21921       Pleurogrammus monopterygius         Atka mackerel          2  
    ## 
    ##     30050                                     rougheye and blackspotted    2  
    ##                                                    rockfish unid.             
    ## 
    ##     30060             Sebastes alutus            Pacific ocean perch       1  
    ## 
    ##     21921       Pleurogrammus monopterygius         Atka mackerel          2  
    ## 
    ##     21921       Pleurogrammus monopterygius         Atka mackerel          2  
    ## ------------------------------------------------------------------------------
    ## 
    ## Table: Table continues below
    ## 
    ##  
    ## ------------------------
    ##  LENGTH   WEIGHT   YEAR 
    ## -------- -------- ------
    ##   390      822     2005 
    ## 
    ##   380      660     2005 
    ## 
    ##   210      120     2005 
    ## 
    ##   320      500     2005 
    ## 
    ##   350      626     2005 
    ## 
    ##   410      740     2005 
    ## ------------------------

FUNCTIONS
---------

The key function for this analysis is the calculation of residuals from a log-log relationship between length and weight the lw.resids function inputs the length and weight data for individual fish and estimates the residuals.

``` r
lw.resids<-function(length,weight){
    loglength<-log(length)
    logwt<-log(weight)
    lw.res<-lm(logwt~loglength)
    lw.res<-lw.res$residuals
return(lw.res)}
```

EXAMPLE
-------

As an example of calculating the condition index, we take Pacific Cod in the eastern Bering Sea. The code is designed to loop through a number of species (e.g. see the EBS\_Groundfish\_COndition.Rmd file which computes the index for a group of species used in the Ecosystem Considerations SAFE Chapter). For this example, we are doing only a single species, but have left the architecture for multiple species intact. Also, please note that some of the steps such as the STRATUM definitions are specific to the RACEBASE EBS data and may not be needed for other applications.

The plotting by year is done in ggplot2 using gridExtra. This code can be modified as needed, but basically the mean L-W residuals (and variances) are calculated and put into a bar plot with year as the x-axis and mean residual on the y-axis. We have made the plots into a list object (myplot) so that they can easily be arranged onto a gridded figure.

``` r
myplot<-list() #Make the empty list
#myplot[]
lwdata_by_year<-array(dim=c(0,6))
colnames(lwdata_by_year)<-c("species","yrs","ymeans","yn","ysd","yse")
for(i in 1:length(EBS.species)){ #set up the loop to loop through species
    tempdata<-subset(EBS.lwdata,EBS.lwdata$SPECIES_CODE==EBS.species[i]) #subset the data to the species of interest
    tempdata["residuals"]<-lw.resids(tempdata$LENGTH,tempdata$WEIGHT) # Use the lw.resids function to calculate the residuals
    yrs=sort(unique(tempdata$YEAR)) #Sort by year
    ymeans=tapply(tempdata$residuals,tempdata$YEAR,mean) #Calculate mean by year
    yn=tapply(tempdata$residuals,tempdata$YEAR,length) #Count the number of observations by year
    ysd=tapply(tempdata$residuals,tempdata$YEAR,sd) #Calculate the sd of the mean by year
    yse=ysd/sqrt(yn) #Calculate the standard error
    data.summary<-data.frame(EBS.species[i],yrs,ymeans,yn,ysd,yse) #Put the mean, SE and year into a data frame for plotting
lwdata_by_year<-rbind(lwdata_by_year,data.summary)  
p<-ggplot(data.summary, aes(x = yrs, y = ymeans),cex=2) +  
  geom_bar(position = position_dodge(), stat="identity", fill="cornflowerblue",col="black") + 
  geom_errorbar(aes(ymin=ymeans-yse, ymax=ymeans+yse),width=0.30) +
 xlim(1996.5, 2017.5)+
 ggtitle(paste(EBS.speciesnames[i])) + 
    geom_hline(yintercept=0, color="black")+
  theme_bw() +
  theme(panel.grid.major = element_blank())+
    theme(axis.text.x = element_text(size=8))+
theme(axis.text.y = element_text(size=8))+
theme(axis.title.x = element_text(size=8))+
theme(axis.title.y = element_text(size=8))+
labs(title = paste(EBS.speciesnames[i]), y = "Length-weight residual", x = "Year")
#print(p)
pltName <- paste( EBS.speciesnames[i],"plot", sep = '' ) #Name the plot
myplot[[pltName]]<-p} #Add the plot to the list and loop
```

This line of code does the arranging of the plots on a grid. In this case we have a single species (Pacific cod), so the number of columns (ncol) is set to 1. For the Ecosystem Considerations contributions there are usually 2 columns (ncol=2). It also outputs the figure as a .png to the working directory and outputs the data to a .csv file.

    ## png 
    ##   2

![](Groundfish_Condition_files/figure-markdown_github/figure%202%20grid-1.png)

In this next bit of code, the same procedures are followed for calculating the residuals and producing plots. This time the plots are by strata and year. Plots and data are output at the end.

``` r
lwdata_by_strata<-array(dim=c(0,7))
colnames(lwdata_by_strata)<-c("species","strata","yrs","ymeans","yn","ysd","yse")
#By stratum graphs
for(i in 1:length(EBS.species)){
    tempdata<-subset(EBS.lwdata,EBS.lwdata$SPECIES_CODE==EBS.species[i])
    tempdata["residuals"]<-lw.resids(tempdata$LENGTH,tempdata$WEIGHT)

ymeans=aggregate(tempdata$residuals,by=list(tempdata$YEAR,tempdata$STRATUM),mean)
ysd=aggregate(tempdata$residuals,by=list(tempdata$YEAR,tempdata$STRATUM),sd)
yn=aggregate(tempdata$residuals,by=list(tempdata$YEAR,tempdata$STRATUM),length)
yse=ysd$x/sqrt(yn$x)
data.summary<-data.frame(species=EBS.species[i],strata=ymeans$Group.2,yrs=ymeans$Group.1,ymeans=ymeans$x,yn=yn$x,ysd=ysd$x,yse=yse)
lwdata_by_strata<-rbind(lwdata_by_strata,data.summary)

dat1 <- subset(data.summary,data.summary$ymeans>=0)
dat2 <- subset(data.summary,data.summary$ymeans< 0)
p2<-ggplot() +
    geom_bar(data = dat1, aes(x=yrs, y=ymeans, fill=factor(strata)),stat = "identity",col="black") +
    geom_bar(data = dat2, aes(x=yrs, y=ymeans, fill=factor(strata)),stat = "identity",col="black")+
 scale_fill_brewer(palette = "Spectral")+
    geom_hline(yintercept=0, color="black")+
 xlim(1996.5, 2017.5)+
theme_bw() +
    theme(axis.text.x = element_text(size=8))+
theme(axis.text.y = element_text(size=8))+
theme(axis.title.x = element_text(size=8))+
theme(axis.title.y = element_text(size=8))+
  theme(panel.grid.major = element_blank())+theme(legend.position="right")+
 labs(title = paste(EBS.speciesnames[i]), y = "Length-weight residual", x = "Year", fill = "Stratum")
#print(p2)
pltName <- paste( EBS.speciesnames[i],"plot", sep = '' )
myplot[[pltName]]<-p2}

png("bystrata.png",width=6,height=7,units="in",res=300)
grid.arrange(grobs=myplot,ncol=1)
dev.off()
```

    ## png 
    ##   2

``` r
grid.arrange(grobs=myplot,ncol=1)
```

![](Groundfish_Condition_files/figure-markdown_github/figure%203%20set%20up-1.png)

``` r
write.csv(lwdata_by_strata,"lwdata_by_strata.csv",row.names=FALSE)
```

GROUNDFISH CONDITION INDICES FOR ALASKA ESR
-------------------------------------------

There are 3 r markdown scripts included in this package that can be used to produce the groundfish condition indices for the annual updates to the ecosystem contributions section of the SAFE documents. They are:

GOA\_GroundfishCondition.rmd

EBS\_GroundfishCondition.rmd

AI\_GroundfishCondition.rmd

These rmarkdown scripts are based on E. Siddon's conversion of the original R script and will create a word document and associated figures. It should only be necessary to update the length-weight data from RACEBASE and then the r markdown text (to reflect any new trends or results).
