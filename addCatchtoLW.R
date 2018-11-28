species1<-species.names
haul0<-array(dim=c(0,5))
colnames(haul0)<-c("HAUL","VESSEL","CRUISE","CATCH","SPECIES_CODE")
for(i in 1:length(species1)){
species2<-species1[i]
haul1<-cbind(haul$HAUL,haul$VESSEL,haul$CRUISE,haul[,as.character(species2)],species2)
colnames(haul1)<-c("HAUL","VESSEL","CRUISE","CATCH","SPECIES_CODE")
haul0<-rbind(haul0,haul1)}

lwdata<-merge(lwdata,haul0,by=c("HAUL","VESSEL","CRUISE","SPECIES_CODE"),all.x=TRUE)
lwdata<-subset(lwdata,lwdata$CATCH>0)




