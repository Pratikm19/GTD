---
title: Global Terrorism Analysis - A bird eye view to one of man made catastrophic
  event.
author: Harshvardhan Singh, Pratik Mehta, Shoyeb Ansari,Ashutosh More @ Sankhya Analytics
  LLP UK
date: "April 10, 2017"
output:
  html_document:
    highlight: tango
    number_sections: yes
    theme: journal
    self_contained: no
always_allow_html: yes
---

```{r setup, include=FALSE}
options(width=200, gvis.plot.tag="chart")
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

<H1>Global Terrorism Database<H1>
<H2>Data Source</H2>
<ul>
<li>Open Source Dataset by University of Maryland.</li>
<li>Maintained by Study of Terrorism and Responses to Terrorism (START)</li>
<li>Comprises of Database of Terrorist Attacks from 1970 to 2015</li>
</ul>
<H2>Characteristics of Dataset</H2>
<ul>
<li>Contains Information on over 150,000 Terrorist Attacks</li>
<li>Includes comprehensive data on more than 75,000 bombings, 17,000 assassinations and 9000 kidnapping since 1970</li>
<li>Source : https://www.start.umd.edu/gtd/</li>
</ul>

Using read.csv function we load all the data which we are going to analyze.

```
```{r results='asis', tidy=TRUE}
op <- options(gvis.plot.tag="chart")

TerroristGDPDataContinent=read.csv("D:\\Ashutosh\\Personal\\study\\R-Language\\Project-Work\\Final-Published\\GitHub\\Ashutosh\\TerroristGDPDataContinent.csv")

dim(TerroristGDPDataContinent)

library(dplyr)
library(plyr)
library(plotly)
library(googleVis)
library(RJSONIO)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library("networkD3")          

colnames(TerroristGDPDataContinent)


```


<H1>Perform NA Analysis on your data</H1>
Using mice package we did NA analysis, and found out which attributes for  which observations there are NA values.
Mainly we analyzed, wheather columns like nkill, nwound, propvalue are having NA values, as these columns are mainly used in our PCA calculation, which in turn used for coming up with GTI index based on which we have done clustering of countries in High, Medium and Low risk.


```{r}
#install.packages("mice")
library(mice)
NA_PATTERN=md.pattern(TerroristGDPDataContinent)
write.csv(NA_PATTERN, "d://temp//NA_PATTERN.csv")
NA_PATTERN=read.csv("d://temp//NA_PATTERN.csv")
```

Since our data is predefined and having a fixed assumption as far its attributes are concerned, we can not apply any of prescribed NA treatment techiniques.
So what we did is to minimize NA values in these key columns like nkill and nwound mainly, we crosschecked as per its definitions that nkill and nwound is sum of all nkillus, nkillter and general kill and nwoundus, nwoundter and general wound respectively.
We created a duplicate columns for these two columns and then applied following formulas on these two new columns.
From the output of below command you can clearly see, there is reduction in NA values in these two columns after applying given formula.


Following columns are created for reducing NA's whose explanation is given above, these new columns are used for doing all the analysis
```{r}

TerroristGDPDataContinent$nkill1=TerroristGDPDataContinent$nkill

TerroristGDPDataContinent$nkill1=ifelse((is.na(TerroristGDPDataContinent$nkill1) &  (TerroristGDPDataContinent$nkillter > 0 | TerroristGDPDataContinent$nkillus > 0)) , 
                                           ifelse(is.na(TerroristGDPDataContinent$nkillter), 0, 
                                                  TerroristGDPDataContinent$nkillter) + 
                                          ifelse(is.na(TerroristGDPDataContinent$nkillus), 0, TerroristGDPDataContinent$nkillus),
                                        TerroristGDPDataContinent$nkill1
                                        )


TerroristGDPDataContinent$nwound1=TerroristGDPDataContinent$nwound
TerroristGDPDataContinent$nwound1=ifelse((is.na(TerroristGDPDataContinent$nwound1) &  (TerroristGDPDataContinent$nwoundte > 0 | TerroristGDPDataContinent$nwoundus > 0)) , 
                                        ifelse(is.na(TerroristGDPDataContinent$nwoundte), 0, 
                                               TerroristGDPDataContinent$nwoundte) + 
                                          ifelse(is.na(TerroristGDPDataContinent$nwoundus), 0, TerroristGDPDataContinent$nkillus),
                                        TerroristGDPDataContinent$nwound1
)


```


```{r}
summary(TerroristGDPDataContinent$nkill1)
summary(TerroristGDPDataContinent$nkill)

summary(TerroristGDPDataContinent$nwound1)
summary(TerroristGDPDataContinent$nwound)
```


<H1>Overview of Global Terrorism Data</H1>
<h2>Incident Time : Year, Month, Day of Attack</h2>
<h3>Year Wise Analysis and its visualization</h3>
```{r}
yearWiseData=ddply(TerroristGDPDataContinent,.(iyear),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)),
Total_Success=sum(na.omit(success)))
yearWiseData=yearWiseData[with(yearWiseData, order(-Total_Kill)), ]

plot_ly(yearWiseData) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Total Incidents in a Year") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kill, name = "Total Killed in a Year", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wound, name = "Total Wounded in a Year") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Total Successful attacks in a Year") %>%
  layout(title = "Terrorism Incidents Over Time",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Count"))

```
<h3>Month Wise Analysis and its visualization</h3>

```{r}

monthWiseData=ddply(TerroristGDPDataContinent,.(imonth),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)),
Total_Success=sum(na.omit(success)))

monthWiseData=monthWiseData[with(monthWiseData, order(-Total_Kill)), ]

plot_ly(monthWiseData) %>%
  add_lines(alpha = 1, x = ~imonth, y = ~Total_Incidents, name = "Total Incidents in a Year") %>%
  add_lines(alpha = 1,x = ~imonth, y = ~Total_Kill, name = "Total Killed in a Year", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~imonth, y = ~Total_Wound, name = "Total Wounded in a Year") %>%
  add_lines(alpha = .50, x = ~imonth, y = ~Total_Success, name = "Total Successful attacks in a Year") %>%
  layout(title = "Terrorism Incidents Over Time",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Count"))


```
<h2>Weapon Type :Use of Explosives, Fire Arms, Gasoline Attack, Rifles, Suicide Bombings</h2>
```{r}
weaponWiseData=ddply(TerroristGDPDataContinent,.(weaptype1_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)),Total_Success=sum(na.omit(success)))
weaponWiseData=weaponWiseData[with(weaponWiseData, order(-Total_Kill)), ]

plot_ly(weaponWiseData) %>%
   add_bars(alpha = 2, x = ~weaptype1_txt, y = ~Total_Kill, name = "Victims of Armed Assault") %>%
   layout(title = "Weapon Wise Victims",
          xaxis = list(title = "Weapons"),
          yaxis = list(title = "Victims"))
 
 
```


<h2>Mode Of Communication, Target type, Terrorist Organization.</h2>
```{r}
claimModeWiseData=ddply(TerroristGDPDataContinent,.(claimmode_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
claimModeWiseData=subset(claimModeWiseData, claimmode_txt != ".")
claimModeWiseData=claimModeWiseData[with(claimModeWiseData, order(-Total_Kill)), ]


plot_ly(claimModeWiseData) %>%
   add_bars(alpha = 2, x = ~claimmode_txt, y = ~Total_Kill, name = "Terrorists Mode of communications") %>%
   layout(title = "Terrorists Mode of communications",
          xaxis = list(title = "Claim Mode"),
          yaxis = list(title = "Victims"))


targetWiseData=ddply(TerroristGDPDataContinent,.(targtype1_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
targetWiseData=targetWiseData[with(targetWiseData, order(-Total_Kill)), ]

plot_ly(targetWiseData) %>%
   add_bars(alpha = 2, x = ~targtype1_txt, y = ~Total_Kill, name = "Top 30 Terrorists Targets") %>%
   layout(title = "Top 30 Terrorists Targets",
          xaxis = list(title = "Targets"),
          yaxis = list(title = "Victims"))


groupWiseData=ddply(subset(TerroristGDPDataContinent, gname != "."),.(gname),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))

groupWiseData=groupWiseData[with(groupWiseData, order(-Total_Kill)), ]
top10_groupWiseData = head(groupWiseData, 10)

plot_ly(top10_groupWiseData) %>%
   add_bars(alpha = 2, x = ~gname, y = ~Total_Kill, name = "Top 10 Terrorists Groups") %>%
   layout(title = "Top 10 Terrorists Groups",
          xaxis = list(title = "Group"),
          yaxis = list(title = "Victims"))

```

<h2>Effected Continents, Countries, Region, Cities, Nationalities </h2>
colnames(TerroristGDPDataContinent)
```{r}
continentWiseData=ddply(TerroristGDPDataContinent,.(Continent),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
continentWiseData=continentWiseData[with(continentWiseData, order(-Total_Kill)), ]

plot_ly(continentWiseData, labels = ~Continent, values = ~Total_Kill, type = 'pie') %>%
  layout(title = 'Continent Wise Killings',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


regionWiseData=ddply(TerroristGDPDataContinent,.(region_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
regionWiseData=regionWiseData[with(regionWiseData, order(-Total_Kill)), ]

plot_ly(regionWiseData, labels = ~region_txt, values = ~Total_Kill, type = 'pie') %>%
  layout(title = 'Region Wise Killings',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


cityWiseData=ddply(TerroristGDPDataContinent,.(city),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
cityWiseData=subset(cityWiseData, city!="Unknown")
cityWiseData=cityWiseData[with(cityWiseData, order(-Total_Kill)), ]

top10_cityWiseData = head(cityWiseData, 10)

plot_ly(top10_cityWiseData) %>%
   add_bars(alpha = 1, x = ~city, y = ~Total_Kill, name = "Top 10 Attacked Cities of World") %>%
   layout(title = "Top 10 Attacked Cities of World",
          xaxis = list(title = "Cities"),
          yaxis = list(title = "Victims"))


nationalityWiseData=ddply(TerroristGDPDataContinent,.(natlty1_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
nationalityWiseData=nationalityWiseData[with(nationalityWiseData, order(-Total_Kill)), ]

top10_nationalityWiseData = head(nationalityWiseData, 10)

plot_ly(top10_nationalityWiseData) %>%
   add_bars(alpha = 2, x = ~natlty1_txt, y = ~Total_Kill, name = "Top 20 Attacked Nations of World") %>%
   layout(title = "Top 20 Attacked Nationalities of World",
          xaxis = list(title = "Nationalities"),
          yaxis = list(title = "Victims"))


```

<h2>Success Rate of Terrorists</h2>
```{r}
successWiseData=ddply(TerroristGDPDataContinent,.(success),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))

plot_ly(successWiseData, labels = ~success, values = ~Total_Kill, type = 'pie') %>%
  layout(title = 'Success rate of Terrorists (1-Success, 0-Failure)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



```

<H2>Number of people killed/wounded in these Terrorists Attack</h2>
```{r}
success_TerroristGDPDataContinent=subset(TerroristGDPDataContinent, success==1)

successWiseDataKilled=ddply(success_TerroristGDPDataContinent,.(success),summarise,Total_Kill=sum(na.omit(nkill1)))
successWiseDataKilled$Category="People Killed"
successWiseDataWounded=ddply(success_TerroristGDPDataContinent,.(success),summarise,Total_Kill=sum(na.omit(nwound1)))
successWiseDataWounded$Category="People Wounded"
successWiseDataWounded_Killed = rbind(successWiseDataWounded,successWiseDataKilled)

plot_ly(successWiseDataWounded_Killed, labels=~Category, values = ~c(Total_Kill), type = 'pie') %>%
  layout(title = 'Number of people killed/wounded in these Terrorists Attack',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


```


<H1>Attack Summary of deadliest attack in one of most attacked country</H1>
```{r}
top5_nationalityWiseData = head(nationalityWiseData, 1)

top5_wordCloud_NationalityWise=merge(TerroristGDPDataContinent, top5_nationalityWiseData, all.y=TRUE)

top100_wordCloud_NationalityWise=top5_wordCloud_NationalityWise[with(top5_wordCloud_NationalityWise, order(-nkill)), ]

top100_wordCloud_NationalityWise=head(top100_wordCloud_NationalityWise, 100)

corp=Corpus(VectorSource(top100_wordCloud_NationalityWise$summary))
corp=tm_map(corp, tolower)
corp=tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
#corp<- tm_map(corp,PlainTextDocument)
corp<-tm_map(corp,removeWords, stopwords("english"))

wordcloud(corp,max.words=1000,random.order=FALSE,colors=brewer.pal(8, "Dark2"))
```
colnames(TerroristGDPDataContinent)

<H1>Attack Summary showing property damage of deadliest attack in one of most attacked country</H1>
```{r}
top5_nationalityWiseData = head(nationalityWiseData, 5)

top5_wordCloud_NationalityWise=merge(TerroristGDPDataContinent, top5_nationalityWiseData, all.y=TRUE)

top100_wordCloud_NationalityWise=top5_wordCloud_NationalityWise[with(top5_wordCloud_NationalityWise, order(-nkill)), ]

top100_wordCloud_NationalityWise=head(top100_wordCloud_NationalityWise, 20000)

corp=Corpus(VectorSource(top100_wordCloud_NationalityWise$propcomment))
corp=tm_map(corp, tolower)
corp=tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
#corp<- tm_map(corp,PlainTextDocument)
corp<-tm_map(corp,removeWords, stopwords("english"))

wordcloud(corp,max.words=1000,random.order=FALSE,colors=brewer.pal(8, "Dark2"))
```



<H1>Visualization - Using googleVis interactive charts</H1>
<H2>Continent Wise Understanding of the Data</H2>

```{r}
Asia<-subset(TerroristGDPDataContinent,Continent=="AS")
regionwisedata_Asia<-ddply(Asia,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill1)))
M<-gvisMotionChart(regionwisedata_Asia,idvar='region_txt',timevar='iyear')
M

NorthAmerica<-subset(TerroristGDPDataContinent,is.na(TerroristGDPDataContinent$Continent))
regionwisedata_NA<-ddply(NorthAmerica,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill)))
M<-gvisMotionChart(regionwisedata_NA,idvar='region_txt',timevar='iyear')
M

Africa<-subset(TerroristGDPDataContinent,Continent=="AF")
          regionwisedata_AF<-ddply(Africa,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill1)))
          M<-gvisMotionChart(regionwisedata_AF,idvar='region_txt',timevar='iyear')
          M

Europe<-subset(TerroristGDPDataContinent,Continent=="EU")
          regionwisedata_EU<-ddply(Europe,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill1)))
          M<-gvisMotionChart(regionwisedata_EU,idvar='region_txt',timevar='iyear')
          M

SOUTHAM<-subset(TerroristGDPDataContinent,Continent=="SA")
          regionwisedata_SA<-ddply(SOUTHAM,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill1)))
          M<-gvisMotionChart(regionwisedata_SA,idvar='region_txt',timevar='iyear')
          M

ANZ<-subset(TerroristGDPDataContinent,Continent=="OC")
regionwisedata_SA<-ddply(ANZ,.(iyear,Continent,region_txt),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill1)))
M<-gvisMotionChart(regionwisedata_SA,idvar='region_txt',timevar='iyear')
M

```

<H2>Network Visualization - Top 100 nodes showing who is targetting who</H2>
```{r}
groupNationalityWiseData=ddply(TerroristGDPDataContinent,.(gname, natlty1_txt),summarise,Total_Incidents=length(na.omit(eventid)))
groupNationalityWiseData=subset(groupNationalityWiseData, gname!="Unknown")
groupNationalityWiseData=groupNationalityWiseData[with(groupNationalityWiseData, order(-Total_Incidents)), ]
Top_100_groupNationalityWiseData=head(groupNationalityWiseData, 100)
Top_100_groupNationalityWiseData_Network= subset(Top_100_groupNationalityWiseData, select=c(gname, natlty1_txt, Total_Incidents))
simpleNetwork(Top_100_groupNationalityWiseData_Network,  height = 1000, width = 1000, fontSize = 14,  fontFamily = "sans-serif", linkDistance = 90, zoom=TRUE)
```

<H2>Network Visualization - Top 100 nodes showing which group is using which weapons</H2>

```{r}
groupWeaponWiseData=ddply(TerroristGDPDataContinent,.(gname, weaptype1_txt),summarise,Total_Incidents=length(na.omit(eventid)))
groupWeaponWiseData=subset(groupWeaponWiseData, gname!="Unknown")
groupWeaponWiseData=groupWeaponWiseData[with(groupWeaponWiseData, order(-Total_Incidents)), ]
Top_100_groupWeaponWiseData=head(groupWeaponWiseData, 100)
Top_100_groupWeaponWiseData_Network= subset(Top_100_groupWeaponWiseData, select=c(gname, weaptype1_txt, Total_Incidents))
simpleNetwork(Top_100_groupWeaponWiseData_Network,  height = 1000, width = 1000, fontSize = 14,  fontFamily = "sans-serif", linkDistance = 120, zoom=TRUE)
```

<H2>Network Visualization - Top 100 nodes showing which group is targetting what</H2>

```{r}
groupTargetWiseData=ddply(TerroristGDPDataContinent,.(gname, targtype1_txt),summarise,Total_Incidents=length(na.omit(eventid)))
groupTargetWiseData=subset(groupTargetWiseData, gname!="Unknown")
groupTargetWiseData=groupTargetWiseData[with(groupTargetWiseData, order(-Total_Incidents)), ]
Top_100_groupTargetWiseData=head(groupTargetWiseData, 100)
Top_100_groupTargetWiseData_Network= subset(Top_100_groupTargetWiseData, select=c(gname, targtype1_txt, Total_Incidents))
simpleNetwork(Top_100_groupTargetWiseData_Network,  height = 1000, width = 1000, fontSize = 14,  fontFamily = "sans-serif", linkDistance = 120, zoom=TRUE)
```

<H1>Principal Component Analysis on the data to arrive at GTI, for Clustering of countries in High, Medium and Low</H1>
<h2>Assumptions and Pre-Requisites:</h2>
Grouping is done based on Country Text present in data. PCA is done on all those grouped records, in which we have considered Total of nkill, Total of nwound, Total incidents and Total property damage.
<h2>R Commands and its Output</h2>
```{r}
countryWiseData=ddply(TerroristGDPDataContinent,.(country_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)),  Total_Propvalue=sum(na.omit(propvalue)))
colnames(countryWiseData)
countryWiseDataPCA=subset(countryWiseData, select=c(Total_Incidents, Total_Kill, Total_Wound, Total_Propvalue))
pcGTD = princomp(formula=~ ., data = countryWiseDataPCA, cor=TRUE)
pcGTD
pcGTD$loadings
summary(pcGTD)
```

<h2>Visualization of PCA output for Damage variables:</h2>
```{r}
plot(pcGTD)
biplot(pcGTD)
```

<H1>Inference<H1>
As per above analysis, inference from this is that:
<b>Component 1</b> – total_incidents, total_nkill, total_nwound
<b>Component 2</b> - propvalue

<H1>Final Conclusion and further analysis:</H1>
Since we found that propvalue is having lot of NA for about 80% of records, we will be excluding the same and doing PCA analysis again without it.

```{r}
summary(TerroristGDPDataContinent$propvalue)
countryWiseData=ddply(TerroristGDPDataContinent,.(country_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill1)), Total_Wound=sum(na.omit(nwound1)))
colnames(countryWiseData)
countryWiseDataPCA=subset(countryWiseData, select=c(Total_Incidents, Total_Kill, Total_Wound))
pcGTD = princomp(formula=~ ., data = countryWiseDataPCA, cor=TRUE)
pcGTD
pcGTD$loadings
summary(pcGTD)
plot(pcGTD)
biplot(pcGTD)

```

<H1>Inference</H1>
As per above analysis, inference from this is that:
Component 1 – total_incidents, total_nkill, total_nwound
It is considering all the variance of all these variables.
So we will be using score of this component for GTI and using which we will be forming the clusters.

<H2>Merging PCA score as GTI score with original country wise data set</H2>
```{r}
summary(countryWiseData)
countryWiseData$Damage_Score=pcGTD$scores[,1]
```
<h2>Showing Top 20 countries in descending order of its GTI score </h2>
```{r}
countryWiseData=countryWiseData[with(countryWiseData, order(-Damage_Score)), ]
head(countryWiseData, 20)
```

<h2>Showing Bottom 20 countries in as per its GTI score </h2>
```{r}

tail(countryWiseData, 20)

```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

