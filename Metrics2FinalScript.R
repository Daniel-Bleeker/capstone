# Note all packages must be installed for code to run properly. 
install.packages("car")
library(tidyverse)
library(readxl)
library(plm)
library(stargazer)
library(lmtest)
library(car)
### Importing and preprocesing data to be used
pwt100 <- read_excel("C:/Users/revsf/Downloads/FinalProjectDataSheetsUsed.xlsx", 
                     sheet = "FinalData")
table<-pwt100 %>% filter(year %in% c(1998:2017))
tableUse<-table %>% select(country, year, `PopGr%`, PerCapitaGrowth, LaggedPerCapitaRGdp, LaggedGDPGr, Open, csh_i, csh_g, Tel, TelSq, TelLag)
colnames(tableUse)[7]<-"open"
colnames(tableUse)[8]<-"ShFixInv"
colnames(tableUse)[9]<-"ShGovCons"

celldata<- read_excel("C:/Users/revsf/Downloads/FinalProjectDataSheetsUsed.xlsx", 
                      sheet = "Cellphone")
celldata<-celldata %>% select(Country, Year, Value)
colnames(celldata)[3]<-"Cellphone"
celldata<- celldata %>% mutate(id=paste0(Country,Year))
table2Use<-tableUse %>% mutate(id=paste0(country, year))
celldatafinal<-merge(table2Use, celldata, by.x="id", by.y="id")
celldatafinal<-celldatafinal %>% select(country, year, `PopGr%`, PerCapitaGrowth, LaggedPerCapitaRGdp, LaggedGDPGr, open, ShFixInv, ShGovCons, Cellphone)
celldatafinal<-celldatafinal %>% mutate(cellsq=Cellphone^2)

###Summary Statistics
##General Telecommunication
CorTab<-tableUse %>% select(year, `PopGr%`, PerCapitaGrowth, LaggedPerCapitaRGdp, LaggedGDPGr, open, ShFixInv, ShGovCons, Tel)
# General Summary for telecommunications data
summary(CorTab)
#Correlation Table
cor(CorTab)

#Plot of Real Per Capita GDP Growth Over Time
ggplot(tableUse, aes(year, PerCapitaGrowth))+geom_point()+geom_smooth()+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#Plot of ShFixInv by Nation
ggplot(tableUse, aes(country, ShFixInv))+geom_point()+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Cellphone data
CorTab2<-celldatafinal %>% select(year, `PopGr%`, PerCapitaGrowth, LaggedPerCapitaRGdp, LaggedGDPGr, open, ShFixInv, ShGovCons, Cellphone)
summary(CorTab2)
#Correlation Table
cor(CorTab2)

## Country Statistics
# Telecommunication
CountrySummary<-table %>% group_by(country) %>%
  summarize(PerCapitaGDPGrowth=mean(PerCapitaGrowth), PerCapitaGDP=mean(PerCaipta),Population=mean(pop), 
            PopGrowthRate=mean(`PopGr%`), Openness=mean(Open), InvestmentShare=mean(csh_i), GovernmentConsumptionShare=mean(csh_g), 
            Telecommunication=mean(Tel))
OECD<-table %>% summarize(PerCapitaGDPGrowth=mean(PerCapitaGrowth), 
                          PerCapitaGDP=mean(PerCaipta),Population=mean(pop), 
                          PopGrowthRate=mean(`PopGr%`), Openness=mean(open), InvestmentShare=mean(csh_i), 
                          GovernmentConsumptionShare=mean(csh_g), Telecommunication=mean(Tel))
country<-"OECD"
row<-cbind(country, OECD)
CountrySummaryTel<-rbind(CountrySummary, row)

#Cellphone
CellSummary<-celldatafinal %>% group_by(country) %>% 
  summarize(PerCapitaGDPGrowth=mean(LaggedGDPGr), PerCapitaGDP=mean(LaggedPerCapitaRGdp), 
            PopGrowthRate=mean(`PopGr%`), Openness=mean(open), InvestmentShare=mean(ShFixInv), GovernmentConsumptionShare=mean(ShGovCons),
            CellData=mean(Cellphone))
CellOECD<-celldatafinal %>% summarize(PerCapitaGDPGrowth=mean(LaggedGDPGr), PerCapitaGDP=mean(LaggedPerCapitaRGdp), 
                                      PopGrowthRate=mean(`PopGr%`), Openness=mean(open), InvestmentShare=mean(ShFixInv), GovernmentConsumptionShare=mean(ShGovCons),
                                      CellData=mean(Cellphone))
country<-"OECD"
Cellrow<-cbind(country, CellOECD)
CountrySummaryCell<-rbind(CellSummary, Cellrow)



### Analysis & Findings
##Telecom
tableUse2<-tableUse %>% filter(year %in% 1998:2017) #Australia is missing 2018 So it is removed to make the model "balanced"
TelecomData2Use<-pdata.frame(tableUse2,index=c("country","year"))
pvar(TelecomData2Use)
#Model Creation
ModelTelecom<-plm(PerCapitaGrowth~country+year+LaggedGDPGr+PopGr.+LaggedPerCapitaRGdp+open+ShFixInv+ShGovCons+Tel, data=TelecomData2Use,model="within")
#Reporting Effects
stargazer(ModelTelecom, type="html", out="C:/Users/revsf/Downloads/TelecomModel.html", flip = TRUE) # Table 7
FixedEffects<-summary(fixef(ModelTelecom))
stargazer(FixedEffects, type="html", out="C:/Users/revsf/Downloads/FixedEffectstTelecom.html", flip = TRUE ) # Table 8
coeftest(ModelTelecom, vcov. = vcovHC, type = "HC1")

##Cellphone
celldatafinal<-celldatafinal %>% filter(year %in% 1998:2017) #Australia is missing 2018 So it is removed to make the model "balanced"
CellData2Use<-pdata.frame(celldatafinal,index=c("country","year"))
pvar(CellData2Use)
#Model Creation
ModelCell<-plm(PerCapitaGrowth~country+year+LaggedGDPGr+PopGr.+LaggedPerCapitaRGdp+open+ShFixInv+ShGovCons+Cellphone, CellData2Use, model="within")
stargazer(ModelCell, type="html", out="C:/Users/revsf/Downloads/CellphoneModel.html") # Table 9
FixedEffects<-summary(fixef(ModelCell)) 
stargazer(FixedEffects, type="html", out="C:/Users/revsf/Downloads/FixedEffects4Cellphone.html")  # Table 10
coeftest(ModelCell, vcov. = vcovHC, type = "HC1") 

#### Finding Residuals 
### Telecommunications Data
## Expected Error for Country
table2Use<-table2Use %>% arrange(country)
residuals<-data.frame(ModelTelecom$residuals, table2Use$country)
colnames(residuals)<-c("residuals", "country")
#Table 11 part 1
ErrorSummaryTelecomCountry<-residuals %>% group_by(country) %>% 
  summarize(ExpectedError=mean(residuals))
### Expected Value of Error for Year
residuals<-data.frame(ModelTelecom$residuals, table2Use$year)
colnames(residuals)<-c("residuals", "year")
#Table 11 part 2
ErrorSummaryTelecomYear<-residuals %>% group_by(year) %>% summarize(ExpectedError=mean(residuals))


### Cell Data
## Expected Error for Country
pvar(TelecomData2Use)
table4Resid<-table2Use %>% arrange(country) %>% filter(year %in% 2010:2017)
residuals<-data.frame(ModelCell$residuals, table4Resid$country)
colnames(residuals)<-c("residuals", "country")
# Graph 12 part 1
ErrorSummaryCellCountry<-residuals %>% group_by(country) %>% summarize(ExpectedError=mean(residuals))
## Expected Error for Year
residuals<-data.frame(ModelCell$residuals, table4Resid$year)
colnames(residuals)<-c("residuals", "year")
# Graph 12 part 2
ErrorSummaryCellYear<-residuals %>% group_by(year) %>% summarize(ExpectedError=mean(residuals))


####Vif Test
## Telecommunication
# First Create Pooled Model
TelPooled<-plm(PerCapitaGrowth~country+year+LaggedGDPGr+PopGr.+LaggedPerCapitaRGdp+open+ShFixInv+ShGovCons+Tel, data=TelecomData2Use, model="pooling")
# Run Vif test   
vif(TelPooled)   #Table 13
## Cellphone
# Create Pooled Model
CellPooled<-plm(PerCapitaGrowth~country+year+LaggedGDPGr+PopGr.+LaggedPerCapitaRGdp+open+ShFixInv+ShGovCons+Cellphone, data=CellData2Use, model="pooling")
#Run Vif Test
vif(CellPooled)  # Table 14

### Hausman Test
## Telecommunications
# Create Random effects model 
ModelRandomTelecom<-plm(PerCapitaGrowth~country+year+LaggedGDPGr+PopGr.+LaggedPerCapitaRGdp+open+ShFixInv+ShGovCons+Tel, data=TelecomData2Use,model="twoway")
phtest(ModelRandomTelecom, ModelTelecom)
