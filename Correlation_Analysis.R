############Libraries

library(readxl)
library(dplyr)
library(MVA)
library (cluster)
library(mice)
library(ggplot2)
library(tidyr)
library(EnvStats) 
library(gridExtra)


#############################Load and inspect data

urb_all <- read.csv("C:/Users/danie/OneDrive/Dokumente/HSLU Master/Module/Geospatial Data Analysis for Smart Communities/Group Project/FAO_all/Population_E_All_Data_(Normalized).csv")
agri_all <- read.csv("C:/Users/danie/OneDrive/Dokumente/HSLU Master/Module/Geospatial Data Analysis for Smart Communities/Group Project/FAO_all/Inputs_LandUse_E_All_Data_(Normalized).csv")
head(urb_all)
head(agri_all)

#Select Distinct
colnames(agri_all)
distinct_area <- distinct(agri_all,Area, .keep_all = FALSE)
distinct_area

nrows
colnames(urb_all)
distinct_area_urb <- distinct(urb_all,Area, .keep_all = FALSE)
distinct_area_urb  #Same areas


#First select all Europe Countries
agri_filter <- agri_all %>%
  filter(Area == "Albania" | Area =="Andorra" | Area == "Armenia" | Area == "Austria" | Area == "Azerbaijan" | Area == "Belarus" | Area == "Belgium" | Area == "Belgium-Luxembourg" | Area == "Bosnia and Herzegovina" | Area == "Bulgaria" | 
           Area == "Croatia" | Area == "Czechia"  | Area == "Czechoslovakia" | Area == "Cyprus" | Area == "Denmark" | Area == "Estonia" | Area =="Finland" | Area == "France" | Area == "Georgia" | Area == "Germany" | Area == "Greece" | 
           Area == "Hungary" |  Area == "Iceland" | Area == "Ireland" | Area == "Italy" | Area == "Latvia" | Area == "Liechtenstein" | Area == "Lithuania" | Area == "Luxembourg" | Area == "Luxembourg" | Area == "Malta" |Area == "Monaco" | 
           Area == "Montenegro" | Area == "North Macedonia" | Area == "Netherlands" | Area == "Norway" | Area == "Poland" | Area == "Portugal" | Area == "Republic of Moldova" | Area == "Romania" | Area == "Russian Federation" |
           Area == "San Marino" | Area == "Serbia" | Area == "Serbia and Montenegro" | Area == "Slovakia" | Area =="Slovenia" | Area == "Spain" | Area == "Sweden" | Area == "Switzerland" | Area == "Turkey" |Area == "Ukraine" |
           Area == "United Kingdom of Great Britain and Northern Ireland" | Area == "USSR" | Area == "Yugoslav SFR" | Area == "Europe" | Area == "Eastern Europe" | Area == "Northern Europe" | Area == "Southern Europe" | Area == "Western Europe" |
           Area == "European Union (27)") 

#Israel and Kazakhstan not included
#Kosovo no data 


##################Data Preparation

#Subset data on 2007 and 2016. We can therefore exclude Belgium-Luxembourg, Czechoslovakia, Serbia and Montenegro, Yugoslav SFR, USSR 
agri_filter2 <-  agri_all %>%  filter(Area == "Albania" | Area =="Andorra" | Area == "Armenia" | Area == "Austria" | Area == "Azerbaijan" | Area == "Belarus" | Area == "Belgium" | Area == "Bosnia and Herzegovina" | Area == "Bulgaria" | 
                                        Area == "Croatia" | Area == "Czechia"  |  Area == "Cyprus" | Area == "Denmark" | Area == "Estonia" |   Area == "Faroe Islands" | Area =="Finland" | Area == "France" | Area == "Georgia" | Area == "Germany" | Area == "Greece" | 
                                        Area == "Hungary" |  Area == "Iceland" | Area == "Ireland" | Area == "Italy" | Area == "Latvia" | Area == "Liechtenstein" | Area == "Lithuania" |  Area == "Luxembourg" |Area == "Malta"# |Area == "Monaco" | 
                                      | Area == "Montenegro" | Area == "North Macedonia" | Area == "Netherlands" | Area == "Norway" | Area == "Poland" | Area == "Portugal" | Area == "Republic of Moldova" | Area == "Romania" | Area == "Russian Federation" |
                                        Area == "San Marino" | Area == "Serbia" | Area == "Slovakia" | Area =="Slovenia" | Area == "Spain" | Area == "Sweden" | Area == "Switzerland" | Area == "Turkey" |Area == "Ukraine" |
                                        Area == "United Kingdom of Great Britain and Northern Ireland" | Area == "Europe" | Area == "Eastern Europe" | Area == "Northern Europe" | Area == "Southern Europe" | Area == "Western Europe" | Area == "European Union (27)") 


urb_filter2 <-  urb_all %>%  filter(Area == "Albania" | Area =="Andorra" | Area == "Armenia" | Area == "Austria" | Area == "Azerbaijan" | Area == "Belarus" | Area == "Belgium" | Area == "Bosnia and Herzegovina" | Area == "Bulgaria" | 
                                      Area == "Croatia" | Area == "Czechia"  |  Area == "Cyprus" | Area == "Denmark" | Area == "Estonia" |  Area == "Faroe Islands" | Area =="Finland" | Area == "France" | Area == "Georgia" | Area == "Germany" | Area == "Greece" | 
                                      Area == "Hungary" |  Area == "Iceland" | Area == "Ireland" | Area == "Italy" | Area == "Latvia" | Area == "Liechtenstein" | Area == "Lithuania" |  Area == "Luxembourg" | Area == "Malta" #|Area == "Monaco"  
                                    |  Area == "Montenegro" | Area == "North Macedonia" | Area == "Netherlands" | Area == "Norway" | Area == "Poland" | Area == "Portugal" | Area == "Republic of Moldova" | Area == "Romania" | Area == "Russian Federation" |
                                      Area == "San Marino" | Area == "Serbia" | Area == "Slovakia" | Area =="Slovenia" | Area == "Spain" | Area == "Sweden" | Area == "Switzerland" | Area == "Turkey" |Area == "Ukraine" |
                                      Area == "United Kingdom of Great Britain and Northern Ireland" | Area == "Europe" | Area == "Eastern Europe" | Area == "Northern Europe" | Area == "Southern Europe" | Area == "Western Europe" | Area == "European Union (27)") 

#Monaco excluded because it doesnt have agricultural area


#Agri Analyse
colnames(agri_filter2)
distinct_item  <- distinct(agri_filter2, Item, .keep_all = FALSE)
distinct_item

#Urb Analyse
colnames(urb_filter2)
distinct_Item_urb <- distinct(urb_filter2,Item, .keep_all = FALSE)
distinct_Item_urb 

distinct_Element_urb <- distinct(urb_filter2,Element, .keep_all = FALSE)
distinct_Element_urb 

distinct_Unit_urb <- distinct(urb_filter2,Unit, .keep_all = FALSE)
distinct_Unit_urb 


#Check some items to know what to pick and check deviations:
agri_ger   %>% filter(Year == "2016")  %>%   filter(Item == "Country area") #Total area of the country = 35758 * 1000 ha (same as de statis)
#https://web.archive.org/web/20211030054049/https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/02-bundeslaender.html
agri_ger   %>% filter(Year == "2016")  %>%   filter(Item == "Land area") #Land area exluding water of the country = 34936 * 1000 ha (Wikipedia: 34922 ha) --> 2.34% water.
agri_ger   %>% filter(Year == "2016")  %>%   filter(Item == "Agricultural land") #Land area of the country used for agriculture = 16659 * 1000 ha (same as in Statista) / 16715 * 1000 ha in ec.europa --> close enough
#https://www.statista.com/statistics/1249603/agricultural-land-germany/
#All others Check Item Metadata xslx


#Filter data to year and deselect not needed columns: 
agri_filter3 <- agri_filter2 %>%
  filter(Item == "Land area" | Item == "Agricultural land")  %>%
  filter(Year > 2005 ) %>%
  filter(Year < 2017 )


agri_filter3  <- agri_filter3%>%
  dplyr::select(-Area.Code, - Item.Code, -Element, -Element.Code, -Year.Code, -Flag)

distinct_Unit <- distinct(agri_filter3, Unit, .keep_all = FALSE)
distinct_Unit

for_filter3 <- agri_filter2 %>%
  filter(Item == "Land area" | Item == "Forest land")  %>%
  filter(Element == "Area")  %>%
  filter(Year > 2005 ) %>%
  filter(Year < 2017 )

#Forest Analyse
distinct_Element_for <- distinct(for_filter3,Element, .keep_all = FALSE)
distinct_Element_for 

distinct_Unit_for <- distinct(for_filter3,Unit, .keep_all = FALSE)
distinct_Unit_for 

distinct_item_for <- distinct(for_filter3,Item, .keep_all = FALSE)
distinct_item_for 


#Filter data to year and deselect not needed columns: 
for_filter3  <- for_filter3%>%
  dplyr::select(- Area.Code, -Item.Code,-Element, -Element.Code,-Year.Code, -Flag)


urb_filter3 <- urb_filter2 %>%
  filter(Year > 2005 ) %>%
  filter(Year < 2017 )

urb_filter3  <- urb_filter3%>%
  dplyr::select(- Area.Code, -Item, -Item.Code,  -Element.Code, -Year.Code, -Flag, -Note)


#Check for missing values:
md.pattern(agri_filter3)


####Agriculture Land:
#Change Value in 2 new columns, by making it to pivot wide
agri_filter_final <- pivot_wider(agri_filter3, 
                                 names_from = "Item",
                                 values_from = "Value")

#Create Total_Land as vector
Total_Land <- agri_filter_final$`Land area`

#Delete it as own row variable and drop the nas
agri_filter_final  <- agri_filter_final%>%
  dplyr::select(-`Land area`)

agri_filter_final<- drop_na(agri_filter_final)
Total_Land<- Total_Land[!is.na(Total_Land)]


#Now give the vector back
agri_filter_final$Total_Land <- Total_Land
#Make percentage value
agri_filter_final$Agricultural_Land_Percentage<- agri_filter_final$`Agricultural land`/agri_filter_final$Total_Land



#####Forest Land:
for_filter_final <- pivot_wider(for_filter3, 
                                names_from = "Item",
                                values_from = "Value")

#Delete it as own row variable and drop the nas
for_filter_final  <- for_filter_final%>%
  dplyr::select(-`Land area`)

for_filter_final<- drop_na(for_filter_final)


#Now give the vector back
for_filter_final$Total_Land <- Total_Land
#Make percentage value
for_filter_final$Forest_Land_Percentage<- for_filter_final$`Forest land`/for_filter_final$Total_Land


####Urbanisation:
urb_filter_final <- pivot_wider(urb_filter3, 
                                names_from = "Element",
                                values_from = "Value")

#Create Total Population as vector
Total_Population <- urb_filter_final$`Total Population - Both sexes`


#Delete it as own row variable and drop the nas
urb_filter_final <- urb_filter_final%>%
  dplyr::select(-`Total Population - Both sexes`, -`Total Population - Male`, -`Total Population - Female`,  -`Rural population`)

urb_filter_final<- drop_na(urb_filter_final)
Total_Population<- Total_Population[!is.na(Total_Population)]


#Now give the vector back
urb_filter_final$Total_Population <- Total_Population
#Make percentage value
urb_filter_final$Urban_Population_Percentage<- urb_filter_final$`Urban population`/urb_filter_final$Total_Population



#Join together
joined_all <- left_join(agri_filter_final,
                        urb_filter_final,
                        by = c("Year", "Area"))

joined_all_for <- left_join(joined_all,
                            for_filter_final,
                            by = c("Year", "Area"))



#Check ob Daten vollst?ndig sind:
joined_all_group <- joined_all %>% 
  filter(Year == max(Year))  %>% 
  group_by(Area, .drop = FALSE) 

joined_all_group

nrow(joined_all) / 53
#--> Daten sind vollst?ndig


###############Correlation and Linear Model: Agri vs Urb 
joined_all_sliced<- joined_all[1:528,] 
#sliced because Western, Eastern, Southern, Northern and European Union is not needed for the model assumptions test

cor(y= joined_all_sliced$`Agricultural land`,x=joined_all_sliced$`Urban population`) #strong positive correlation

hist(joined_all_sliced$`Urban population`) #right skrewed: use log transformation
hist(log(joined_all_sliced$`Urban population`))
joined_all_sliced$urb_new<- log(joined_all_sliced$`Urban population`)

hist(joined_all_sliced$`Agricultural land`)  #right skrewed: use log transformation
hist(log(joined_all_sliced$`Agricultural land`))
joined_all_sliced$agri_new <- log(joined_all_sliced$`Agricultural land`)


#lm
lm_fit <- lm(agri_new~urb_new, data =joined_all_sliced)
summary(lm_fit)


###############Check for prerequisites for linear model and Correlation

######Correlation:
#Interval scaled variables: Urban Population and Agriculturael area are numerical, ratio scaled scaled variables. --> This is the case

#########Linar Model assumptions:
#1.	The errors follow a normal distribution  
#2.	The errors expected value is zero 
#3.	The errors are homoscedastic (constant variance) 
#4.	The errors are independent: Observations must be independent!

resid <-  lm_fit$residuals
plot(lm_fit)
acf(resid)
pacf(resid)

#######################Graphical Analysis
plot(joined_all_sliced$`Agricultural land` ~ joined_all_sliced$`Urban population`)
abline(lm_fit1)

library(ggplot2)
ggplot(data = joined_all_sliced, mapping = aes(y= joined_all_sliced$`Agricultural land`, x = joined_all_sliced$`Urban population`))+
  geom_point() +
  geom_smooth()

ggplot(data = joined_all_sliced, mapping = aes(y= joined_all_sliced$`Agricultural land`, x = joined_all_sliced$`Urban population`))+
  geom_point() +
  geom_smooth(method="lm")

#######################GAM 
library(mgcv)
gam1 <- gam(agri_new~s(urb_new), data = joined_all_sliced)
summary(gam1)
#Anova
anova(lm_fit, gam1)            



#Exclude russia it is an outlier
joined_all_sliced2 <- joined_all_sliced %>%  filter(Area == "Albania" | Area =="Andorra" | Area == "Armenia" | Area == "Austria" | Area == "Azerbaijan" | Area == "Belarus" | Area == "Belgium" | Area == "Bosnia and Herzegovina" | Area == "Bulgaria" | 
                                                      Area == "Croatia" | Area == "Czechia"  |  Area == "Cyprus" | Area == "Denmark" | Area == "Estonia" |   Area == "Faroe Islands" | Area =="Finland" | Area == "France" | Area == "Georgia" | Area == "Germany" | Area == "Greece" | 
                                                      Area == "Hungary" |  Area == "Iceland" | Area == "Ireland" | Area == "Italy" | Area == "Latvia" | Area == "Liechtenstein" | Area == "Lithuania" |  Area == "Luxembourg" |Area == "Malta"# |Area == "Monaco" | 
                                                    | Area == "Montenegro" | Area == "North Macedonia" | Area == "Netherlands" | Area == "Norway" | Area == "Poland" | Area == "Portugal" | Area == "Republic of Moldova" | Area == "Romania"|
                                                      Area == "San Marino" | Area == "Serbia" | Area == "Slovakia" | Area =="Slovenia" | Area == "Spain" | Area == "Sweden" | Area == "Switzerland" | Area == "Turkey" |Area == "Ukraine" |
                                                      Area == "United Kingdom of Great Britain and Northern Ireland") 


cor(y= joined_all_sliced2$`Agricultural land`,x=joined_all_sliced2$`Urban population`) #strong positive correlation

hist(joined_all_sliced2$`Urban population`) #right skrewed: use log transformation
hist(log(joined_all_sliced2$`Urban population`))
joined_all_sliced2$urb_new<- log(joined_all_sliced2$`Urban population`)

hist(joined_all_sliced2$`Agricultural land`)  #right skrewed: use log transformation
hist(log(joined_all_sliced2$`Agricultural land`))
joined_all_sliced2$agri_new <- log(joined_all_sliced2$`Agricultural land`)


#lm
lm_fit2 <- lm(agri_new~urb_new, data =joined_all_sliced2)
summary(lm_fit2)


resid2 <-  lm_fit2$residuals
plot(lm_fit2)

acf(resid2)
pacf(resid2)


#######################Graphical Analysis
plot(joined_all_sliced2$`Agricultural land` ~ joined_all_sliced2$`Urban population`)
abline(lm_fit2)

library(ggplot2)
ggplot(data = joined_all_sliced2, mapping = aes(y= joined_all_sliced2$`Agricultural land`, x = joined_all_sliced2$`Urban population`))+
  geom_point() +
  geom_smooth()

ggplot(data = joined_all_sliced2, mapping = aes(y= joined_all_sliced2$`Agricultural land`, x = joined_all_sliced2$`Urban population`))+
  geom_point() +
  geom_smooth(method="lm")

#######################GAM 
library(mgcv)
gam2 <- gam(agri_new~s(urb_new), data = joined_all_sliced2)
summary(gam2)
#Anova
anova(lm_fit2, gam2)            

###############Correlation and Linear Model: Agri vs Urb for each country


Countries_Correlation <- c("Albania", "Andorra",  "Armenia", "Austria", "Azerbaijan", "Belarus" , "Belgium" , "Bosnia and Herzegovina" , "Bulgaria" , "Croatia" , "Czechia" , "Cyprus" , "Denmark" , "Estonia" ,"Faroe Islands" ,"Finland", "France" , "Georgia" ,
                           "Germany" , "Greece" , "Hungary" , "Iceland" , "Ireland" , "Italy" , "Latvia" ,"Liechtenstein" , "Lithuania" , "Luxembourg"  , "Malta" , "Montenegro" , "North Macedonia" , "Netherlands" , "Norway",
                           "Poland" , "Portugal", "Republic of Moldova" , "Romania" , "Russian Federation" , "San Marino" , "Serbia" , "Slovakia" , "Slovenia" , "Spain" , "Sweden" , "Switzerland" , "Turkey", "Ukraine" ,
                           "United Kingdom of Great Britain and Northern Ireland" , "Europe" , "Eastern Europe" , "Northern Europe" , "Southern Europe" ,"Western Europe" , "European Union")


length(Countries_Correlation)
Countries_Fit <- c("Albania_Fit", "Andorra_Fit",  "Armenia_Fit", "Austria_Fit", "Azerbaijan_Fit", "Belarus_Fit" , "Belgium_Fit" , "Bosnia and Herzegovina_Fit" , "Bulgaria_Fit" , "Croatia_Fit" , "Czechia_Fit" , "Cyprus_Fit" , 
                   "Denmark_Fit" , "Estonia_Fit" , "Faroe Islands_Fit", "Finland_Fit", "France_Fit" , "Georgia_Fit" , "Germany_Fit" , "Greece_Fit" , "Hungary_Fit" , "Iceland_Fit" , "Ireland_Fit" , "Italy_Fit" , "Latvia_Fit" ,
                   "Liechtenstein_Fit" , "Lithuania_Fit" , "Luxembourg_Fit" , "Malta_Fit" ,  "Montenegro_Fit" , "North Macedonia_Fit" , "Netherlands_Fit" , "Norway_Fit",
                   "Poland_Fit" , "Portugal_Fit", "Republic of Moldova_Fit" , "Romania_Fit" , "Russian Federation_Fit" , "San Marino_Fit" , "Serbia_Fit" , "Slovakia_Fit" , "Slovenia_Fit" , "Spain_Fit" , "Sweden_Fit" , 
                   "Switzerland_Fit" , "Turkey_Fit", "Ukraine_Fit" , "United Kingdom of Great Britain and Northern Ireland_Fit" , "Europe_Fit" , "Eastern Europe_Fit" , "Northern Europe_Fit" ,
                   "Southern Europe_Fit" ,"Western Europe_Fit" , "European Union_Fit")


i <- 1
j <- 1


while (i <= nrow(joined_all)){ 
  
  slicer <- joined_all$Area[i]
  
  temp<- joined_all %>% 
    filter(Area == slicer) 
  
  temp_cor<- cor(y= temp$`Agricultural land`,x=temp$`Urban population`)
  temp_fit <- lm(log(temp$`Agricultural land`)~log(temp$`Urban population`))
  
  assign(Countries_Correlation[j], temp_cor)
  assign(Countries_Fit[j], temp_fit)
  
  i <- i + 11
  j <-  j +1
}

#Note: The standard deviation for Faroe Islands is 0, because the values are constant over the complete time period.


#Create a data.frame

country <-   c("Albania", "Andorra",  "Armenia", "Austria", "Azerbaijan", "Belarus" , "Belgium" , "Bosnia and Herzegovina" , "Bulgaria" , "Croatia" , "Czechia" , "Cyprus" , "Denmark" , "Estonia" , "Faroe Islands","Finland", "France" , "Georgia" ,
               "Germany" , "Greece" , "Hungary" , "Iceland" , "Ireland" , "Italy" , "Latvia" ,"Liechtenstein" , "Lithuania" , "Luxembourg"  , "Malta" , "Montenegro" , "North Macedonia" , "Netherlands" , "Norway",
               "Poland" , "Portugal", "Republic of Moldova" , "Romania" , "Russian Federation" , "San Marino" , "Serbia" ,  "Slovakia" , "Slovenia" , "Spain" , "Sweden" , "Switzerland" , "Turkey", "Ukraine" ,
               "United Kingdom of Great Britain and Northern Ireland")

#length(country)

country_cor <-  c(Albania, Andorra,  Armenia, Austria, Azerbaijan, Belarus , Belgium, `Bosnia and Herzegovina` , Bulgaria , Croatia , Czechia , Cyprus , Denmark , Estonia , `Faroe Islands`, Finland, France , Georgia ,
                  Germany , Greece , Hungary , Iceland , Ireland , Italy , Latvia ,Liechtenstein , Lithuania , Luxembourg  , Malta , Montenegro , `North Macedonia` , Netherlands , Norway,
                  Poland , Portugal, `Republic of Moldova` , Romania , `Russian Federation` , `San Marino` , Serbia , Slovakia , Slovenia , Spain , Sweden , Switzerland , Turkey, Ukraine ,
                  `United Kingdom of Great Britain and Northern Ireland` )
#length(country_cor)

europe_area <-  c ("Southern Europe" , "Southern Europe" , "Caucasus", "Western Europe" , "Caucasus", "Eastern Europe" ,"Western Europe", "Southern Europe", "Southern Europe", "Southern Europe", "Eastern Europe",
                   "Not Classified", "Northern Europe", "Northern Europe", "Northern Europe"  , "Northern Europe", "Western Europe", "Caucasus", "Western Europe", "Southern Europe", "Eastern Europe", "Northern Europe", "Northern Europe", "Southern Europe",
                   "Northern Europe", "Western Europe", "Northern Europe", "Western Europe", "Southern Europe", "Southern Europe", "Southern Europe", "Western Europe", "Northern Europe", "Eastern Europe",
                   "Southern Europe", "Eastern Europe", "Eastern Europe", "Eastern Europe", "Southern Europe", "Southern Europe", "Eastern Europe", "Southern Europe", "Southern Europe", "Northern Europe",
                   "Western Europe", "Not Classified", "Eastern Europe", "Northern Europe")

#length(europe_area)
europe_area_cor <- c(`Southern Europe`, `Southern Europe`, NA, `Western Europe` , NA, `Eastern Europe` ,`Western Europe` , `Southern Europe`, `Southern Europe`, `Southern Europe`, `Eastern Europe`,
                     NA, `Northern Europe`, `Northern Europe`, `Northern Europe`, `Northern Europe`, `Western Europe`, NA, `Western Europe`, `Southern Europe`, `Eastern Europe`, `Northern Europe`, `Northern Europe`, `Southern Europe`,
                     `Northern Europe`, `Western Europe`, `Northern Europe`, `Western Europe`, `Southern Europe`, `Southern Europe`, `Southern Europe`, `Western Europe`, `Northern Europe`, `Eastern Europe`,
                     `Southern Europe`, `Eastern Europe`, `Eastern Europe`, `Eastern Europe`, `Southern Europe`, `Southern Europe`, `Eastern Europe`, `Southern Europe`, `Southern Europe`, `Northern Europe`,
                     `Western Europe`, NA, `Eastern Europe`, `Northern Europe`)

#length(europe_area_cor)

`European Union`


european_union <-  c ("No" , "No" , "No", "European Union" , "No", "No" ,"European Union", "No", "European Union", "European Union", "European Union",
                      "European Union", "European Union",  "European Union", "No", "European Union",  "European Union", "No", "European Union", "European Union", "European Union", "No", "European Union", "European Union",
                      "European Union", "No", "European Union", "European Union", "European Union", "No", "No", "European Union", "No", "European Union",
                      "European Union", "No", "European Union", "No", "No", "No", "European Union", "European Union", "European Union", "European Union",
                      "No", "No", "No", "No")
#length(european_union)

european_union_cor <-  c (NA , NA , NA, `European Union` , NA, NA ,`European Union`, NA, `European Union`, `European Union`, `European Union`,
                          `European Union`, `European Union`, `European Union`, NA, `European Union`,  `European Union`, NA, `European Union`, `European Union`, `European Union`, NA, `European Union`, `European Union`,
                          `European Union`, NA, `European Union`, `European Union`, `European Union`, NA, NA, `European Union`, NA, `European Union`,
                          `European Union`, NA, `European Union`, NA, NA, NA, `European Union`, `European Union`, `European Union`, `European Union`,
                          NA, NA, NA, NA)

#length(european_union_cor)

df_corr<-  data.frame(country, country_cor, europe_area, europe_area_cor, european_union, european_union_cor)

write.csv(df_corr,"C:/Users/danie/OneDrive/Dokumente/HSLU Master/Module/Geospatial Data Analysis for Smart Communities/Group Project/corr_europe.csv", row.names = FALSE)





##############################Correlation and Linear Model: Agri vs Forest 


Countries_Correlation2 <- c("Albania2", "Andorra2",  "Armenia2", "Austria2", "Azerbaijan2", "Belarus2" , "Belgium2" , "Bosnia and Herzegovina2" , "Bulgaria2" , "Croatia2" , "Czechia2" , "Cyprus2" , "Denmark2" , "Estonia2" ,"Faroe Islands2" ,"Finland2", "France2" , "Georgia2" ,
                            "Germany2" , "Greece2" , "Hungary2" , "Iceland2" , "Ireland2" , "Italy2" , "Latvia2" ,"Liechtenstein2" , "Lithuania2" , "Luxembourg2"  , "Malta2" , "Montenegro2" , "North Macedonia2" , "Netherlands2" , "Norway2",
                            "Poland2" , "Portugal2", "Republic of Moldova2" , "Romania2" , "Russian Federation2" , "San Marino2" , "Serbia2" , "Slovakia2" , "Slovenia2" , "Spain2" , "Sweden2" , "Switzerland2" , "Turkey2", "Ukraine2" ,
                            "United Kingdom of Great Britain and Northern Ireland2" , "Europe2" , "Eastern Europe2" , "Northern Europe2" , "Southern Europe2" ,"Western Europe2" , "European Union2")


length(Countries_Correlation)
Countries_Fit2 <- c("Albania_Fit2", "Andorra_Fit2",  "Armenia_Fit2", "Austria_Fit2", "Azerbaijan_Fit2", "Belarus_Fit2" , "Belgium_Fit2" , "Bosnia and Herzegovina_Fit2" , "Bulgaria_Fit2" , "Croatia_Fit2" , "Czechia_Fit2" , "Cyprus_Fit2" , 
                    "Denmark_Fit2" , "Estonia_Fit2" , "Faroe Islands_Fit2", "Finland_Fit2", "France_Fit2" , "Georgia_Fit2" , "Germany_Fit2" , "Greece_Fit2" , "Hungary_Fit2" , "Iceland_Fit2" , "Ireland_Fit2" , "Italy_Fit2" , "Latvia_Fit2" ,
                    "Liechtenstein_Fit2" , "Lithuania_Fit2" , "Luxembourg_Fit2" , "Malta_Fit2" ,  "Montenegro_Fit2" , "North Macedonia_Fit2" , "Netherlands_Fit2" , "Norway_Fit2",
                    "Poland_Fit2" , "Portugal_Fit2", "Republic of Moldova_Fit2" , "Romania_Fit2" , "Russian Federation_Fit2" , "San Marino_Fit2" , "Serbia_Fit2" , "Slovakia_Fit2" , "Slovenia_Fit2" , "Spain_Fit2" , "Sweden_Fit2" , 
                    "Switzerland_Fit2" , "Turkey_Fit2", "Ukraine_Fit2" , "United Kingdom of Great Britain and Northern Ireland_Fit2" , "Europe_Fit2" , "Eastern Europe_Fit2" , "Northern Europe_Fit2" ,
                    "Southern Europe_Fit2" ,"Western Europe_Fit2" , "European Union_Fit2")


Countries_Correlation_all <- c("Albania_all", "Andorra_all",  "Armenia_all", "Austria_all", "Azerbaijan_all", "Belarus_all" , "Belgium_all" , "Bosnia and Herzegovina_all" , "Bulgaria_all" , "Croatia_all" , "Czechia_all" , "Cyprus_all" , "Denmark_all" , "Estonia_all" ,"Faroe Islands_all" ,"Finland_all", "France_all" , "Georgia_all" ,
                               "Germany_all" , "Greece_all" , "Hungary_all" , "Iceland_all" , "Ireland_all" , "Italy_all" , "Latvia_all" ,"Liechtenstein_all" , "Lithuania_all" , "Luxembourg_all"  , "Malta_all" , "Montenegro_all" , "North Macedonia_all" , "Netherlands_all" , "Norway_all",
                               "Poland_all" , "Portugal_all", "Republic of Moldova_all" , "Romania_all" , "Russian Federation_all" , "San Marino_all" , "Serbia_all" , "Slovakia_all" , "Slovenia_all" , "Spain_all" , "Sweden_all" , "Switzerland_all" , "Turkey_all", "Ukraine_all" ,
                               "United Kingdom of Great Britain and Northern Ireland_all" , "Europe_all" , "Eastern Europe_all" , "Northern Europe_all" , "Southern Europe_all" ,"Western Europe_all" , "European Union_all")


k <- 1
m <- 1



while (k <= nrow(joined_all_for)){ 
  
  slicer2 <- joined_all_for$Area[k]
  
  temp2<- joined_all_for %>% 
    filter(Area == slicer2) 
  
  temp_cor2<- cor(y= temp2$`Agricultural land`,x = temp2$`Forest land` )
  temp_fit2 <- lm(log(temp2$`Agricultural land`)~log(temp2$`Urban population`) + log(temp2$`Forest land`))
  
  
  # create the dataframe with 3 columns
  temp_data <- data.frame(Urban=temp2$`Urban population`,
                          Forest= temp2$`Forest land`,
                          Agri=temp2$`Agricultural land`)
  
  # correlation in entire dataframe
  temp_cor_all<- cor(temp_data)
  
  assign(Countries_Correlation_all[m], temp_cor_all)
  assign(Countries_Correlation2[m], temp_cor2)
  assign(Countries_Fit2[m], temp_fit2)
  
  k <- k + 11
  m <-  m +1
}


#Create a data.frame


#length(country)

country_cor2  <- c(`Albania2`, `Andorra2`,  `Armenia2`, `Austria2`, `Azerbaijan2`, `Belarus2` , `Belgium2` , `Bosnia and Herzegovina2` , `Bulgaria2` , `Croatia2` , `Czechia2` , `Cyprus2` , `Denmark2` , `Estonia2` ,`Faroe Islands2` ,`Finland2`, `France2` , `Georgia2` ,
                   `Germany2` , `Greece2` , `Hungary2` , `Iceland2` , `Ireland2` , `Italy2` , `Latvia2` ,`Liechtenstein2` , `Lithuania2` , `Luxembourg2`  , `Malta2` , `Montenegro2` , `North Macedonia2` , `Netherlands2` , `Norway2`,
                   `Poland2` , `Portugal2`, `Republic of Moldova2` , `Romania2` , `Russian Federation2` , `San Marino2` , `Serbia2` , `Slovakia2` , `Slovenia2` , `Spain2` , `Sweden2` , `Switzerland2` , `Turkey2`, `Ukraine2` ,
                   `United Kingdom of Great Britain and Northern Ireland2`)

#length(country_cor2)



#length(europe_area)
europe_area_cor2 <- c(`Southern Europe2`, `Southern Europe2`, NA, `Western Europe2` , NA, `Eastern Europe2` ,`Western Europe2` , `Southern Europe2`, `Southern Europe2`, `Southern Europe2`, `Eastern Europe2`,
                      NA, `Northern Europe2`, `Northern Europe2`, `Northern Europe2`, `Northern Europe2`, `Western Europe2`, NA, `Western Europe2`, `Southern Europe2`, `Eastern Europe2`, `Northern Europe2`, `Northern Europe2`, `Southern Europe2`,
                      `Northern Europe2`, `Western Europe2`, `Northern Europe2`, `Western Europe2`, `Southern Europe2`, `Southern Europe2`, `Southern Europe2`, `Western Europe2`, `Northern Europe2`, `Eastern Europe2`,
                      `Southern Europe2`, `Eastern Europe2`, `Eastern Europe2`, `Eastern Europe2`, `Southern Europe2`, `Southern Europe2`, `Eastern Europe2`, `Southern Europe2`, `Southern Europe2`, `Northern Europe2`,
                      `Western Europe2`, NA, `Eastern Europe2`, `Northern Europe2`)

#length(europe_area_cor2)



#length(european_union)

european_union_cor2 <-  c (NA , NA , NA, `European Union2` , NA, NA ,`European Union2`, NA, `European Union2`, `European Union2`, `European Union2`,
                           `European Union2`, `European Union2`, `European Union2`, NA, `European Union2`,  `European Union2`, NA, `European Union2`, `European Union2`, `European Union2`, NA, `European Union2`, `European Union2`,
                           `European Union2`, NA, `European Union2`, `European Union2`, `European Union2`, NA, NA, `European Union2`, NA, `European Union2`,
                           `European Union2`, NA, `European Union2`, NA, NA, NA, `European Union2`, `European Union2`, `European Union2`, `European Union2`,
                           NA, NA, NA, NA)

#length(european_union_cor2)

df_corr_for <-  data.frame(country, country_cor2, europe_area, europe_area_cor2, european_union, european_union_cor2)

write.csv(df_corr_for,"C:/Users/danie/OneDrive/Dokumente/HSLU Master/Module/Geospatial Data Analysis for Smart Communities/Group Project/corr_europe_with_for.csv", row.names = FALSE)


###################################Visualisations#################################

#############Germany##########################
sliced_plots<- joined_all_for %>% 
  filter(Area == "Germany") 

#plot(sliced_plots$`Urban population`, sliced_plots$`Agricultural land`)
#plot(sliced_plots$Year, sliced_plots$`Agricultural land`)
#plot(sliced_plots$Year, sliced_plots$`Urban population`)


##GG Plot:
#########Plotted against each other
sliced_plots %>%  ggplot(mapping = aes(x = `Urban population`, y = `Agricultural land`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Urban Population  plotted against Agricultural Land") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


sliced_plots %>%  ggplot(mapping = aes(x = `Forest land`, y = `Agricultural land`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Urban Population plotted against Agricultural Land") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


########Agri
Agri_ger1 <- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Agricultural land`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Agricultural Land by year") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 

Agri_ger2<- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Agricultural_Land_Percentage`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Agricultural Land by year in %") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


grid.arrange(Agri_ger1, Agri_ger2, ncol=2)


#########Urban
Urb_ger1 <- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Urban population`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Urban Population by year") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


Urb_ger2 <- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Urban_Population_Percentage`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Urban Population by year in %") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


grid.arrange(Urb_ger1, Urb_ger2, ncol=2)

#########Forest
For_ger1 <- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Forest land`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Forest Land by year") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 


For_ger2<- sliced_plots %>%  ggplot(mapping = aes(x = `Year`, y = `Forest_Land_Percentage`)) +
  geom_line (stat = "identity", size = 1, color = "#4e79a7") +
  # geom_hline(yintercept = 50, linetype = "dashed", col = "darkgrey") +
  #geom_hline(yintercept = 100, linetype = "dashed", col = "darkgrey") +
  ggtitle("Germany: Forest Land by year in %") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme_minimal() 

grid.arrange(For_ger1, For_ger2, ncol=2)