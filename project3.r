
#installing required packages and adjusting some settings
wants <- c("ggplot2","dplyr","R.utils","plotly")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {library(pkg, character.only = TRUE)}
Address <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(Address)
getwd()



destfile1 <- "StormData.csv.bz2"
destfile2 <- "StormData.csv"
URLAddress <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists(destfile2)) {
    download.file(URLAddress,destfile1, mode='wb')
    bunzip2(destfile1, destfile2)
}
df <- read.csv(destfile2)
summary(df)
levels(df$EVTYPE)
Freq <- table(df$EVTYPE)
FreqDF <- data.frame(Freq)
head(arrange(FreqDF,-Freq),20)
colnames(df)



## Data Processing 
### Health Impact  
df.fatalities <- df %>% select(EVTYPE, FATALITIES) %>% group_by(EVTYPE) %>% summarise(total.freq = length(EVTYPE),total.fatalities = sum(FATALITIES)) %>% arrange(-total.fatalities)
head(df.fatalities)
df.injuries <- df %>% select(EVTYPE, INJURIES) %>% group_by(EVTYPE) %>% summarise(total.freq = length(EVTYPE),total.injuries = sum(INJURIES)) %>% arrange(-total.injuries)
head(df.injuries)
df.casualties <- df  %>% group_by(EVTYPE) %>% summarise(total.freq = length(EVTYPE),total.fatalities = sum(FATALITIES),total.injuries = sum(INJURIES)) %>% arrange(-total.fatalities)
head(df.casualties)


### Economic Impact  
#df.damage <- df %>% select(EVTYPE, PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
df.damage <- df
Symbol <- sort(unique(as.character(df.damage$PROPDMGEXP)))
Converter <- c(0,0,0,1,10,10,10,10,10,10,10,10,10,10^9,10^2,10^2,10^3,10^6,10^6)
convert.Converter <- data.frame(Symbol, Converter)
df.damage$Prop.Converter <- convert.Converter$Converter[match(df.damage$PROPDMGEXP, convert.Converter$Symbol)]
df.damage$Crop.Converter <- convert.Converter$Converter[match(df.damage$CROPDMGEXP, convert.Converter$Symbol)]
df.damage <- df.damage %>% mutate(PROPDMG = PROPDMG*Prop.Converter) %>% mutate(CROPDMG = CROPDMG*Crop.Converter) %>% mutate(TOTAL.DMG = PROPDMG+CROPDMG)
df.damage.total <- df.damage %>% group_by(EVTYPE) %>% summarize(total.freq = length(EVTYPE),TOTAL.DMG.EVTYPE = sum(TOTAL.DMG))%>% arrange(-TOTAL.DMG.EVTYPE) 
head(df.damage.total)


ggplot(df.fatalities[1:10,], aes(x=reorder(EVTYPE, -total.fatalities), y=total.fatalities))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Total Fatalities") +labs(x="EVENT TYPE", y="Total Fatalities")
ggplot(df.injuries[1:10,], aes(x=reorder(EVTYPE, -total.injuries), y=total.injuries))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Total Injuries") +labs(x="EVENT TYPE", y="Total Injuries")
ggplot(df.damage.total[1:10,], aes(x=reorder(EVTYPE, -TOTAL.DMG.EVTYPE), y=TOTAL.DMG.EVTYPE))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Economic Impact") +labs(x="EVENT TYPE", y="Total Economic Impact ($USD)")

df.damage.total.filtered <- filter(df.damage.total, TOTAL.DMG.EVTYPE >1000000000)    
head(df.damage.total.filtered)
#for odering of x in the plot
df.damage.total.filtered$EVTYPE <- factor(df.damage.total.filtered$EVTYPE, levels = df.damage.total.filtered$EVTYPE[order(-df.damage.total.filtered$total.freq)])
P1 <- ggplot(df.damage.total.filtered , aes(factor(EVTYPE), log(TOTAL.DMG.EVTYPE) , size = total.freq ))+geom_point()+
  theme(axis.text.x = element_text( angle = 90,size = 4))+
  labs(title = "Total Damage of each Event",x = "Event Type",y="Damage log($)")+
  guides(fill = guide_legend(title = "LEFT", title.position = "top"))
P1

df.casualties.filtered <- filter(df.casualties, total.fatalities >100)    
#for odering of x in the plot
df.casualties.filtered$EVTYPE <- factor(df.casualties.filtered$EVTYPE, levels = df.casualties.filtered$EVTYPE[order(-df.casualties.filtered$total.freq)])
P2 <- plot_ly(df.casualties.filtered, x = ~EVTYPE, y = ~log(total.fatalities), z = ~log2(total.injuries), size = ~(total.freq)) %>% add_markers() %>%
  layout(font = list(    size = 8),
    legend = list(x = 0.1, y = 0.9),
    scene = list(xaxis = list(title = 'Event Type'),
                      yaxis = list(title = 'log(fatalities)'),
                      zaxis = list(title = 'log(Injuries)')))
P2

ggplot(df.damage.total.filtered , aes(EVTYPE, log(total.freq), size = TOTAL.DMG.EVTYPE  ))+geom_point()
ggplot(df.damage.total.filtered , aes(EVTYPE, total.freq, size = TOTAL.DMG.EVTYPE  ))+geom_point()
min(df.damage.total.filtered$TOTAL.DMG.EVTYPE)
