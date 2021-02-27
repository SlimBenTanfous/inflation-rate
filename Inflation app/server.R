#Charger les packages
require(shinydashboard)
require(ggplot2)
library(dplyr)
require(highcharter) 
library(readxl)
library(tidyr)
if (!require("highcharter")) install.packages('highcharter')
library(highcharter)
if (!require("DT")) install.packages('DT')
library(DT)


#Importation des donnees et data management
inflation <- read_excel("inflation.xls",na = "no data")
colnames(inflation)[2:44]<-as.character(c(1980:2022))


year<-c(1980:2022) 
year<-as.character(year)

inf<-inflation%>% gather(year,key = "Year",value="Inflation")
inf<-na.omit(inf) 

names(inf)<-c("region","year","inflation")
inf$year<-as.numeric(inf$year)
inf$inflation<-as.numeric(inf$inflation)

#Creation de sous datas pour les pays de reference qu'on a choisi
Tn<-filter(inf,region=="Tunisia")
India<-filter(inf,region=="India")
China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
UK<-filter(inf,region=="United Kingdom")




##########    Travail Bonus : Taux de chomage des pays de reference      ##############

#Importation des donnees et data management
chomage<- read_excel("chomage.xls",na = "")
chomage<- chomage[,-2]

yearcho<-c(1991:2017) 
yearcho<-as.character(yearcho)

cho<-chomage%>% gather(yearcho,key = "Yearcho",value="Chomage")
cho<-na.omit(cho) 

names(cho)<-c("Region","Year","Chomage")
cho$Year<-as.numeric(cho$Year)
cho$Chomage<-as.numeric(cho$Chomage)

#Creation de sous datas pour les pays de reference qu'on a choisi
Tncho<-filter(cho,Region=="Tunisie")
Indiacho<-filter(cho,Region=="Inde")
Chinacho<-filter(cho,Region=="Chine, RAS de Hong Kong")
Japancho<-filter(cho,Region=="Japon")
Maroccho<-filter(cho,Region=="Maroc")








server <- function(input, output) { 
  
  
  output$hcontainer <- renderHighchart     ({
    
    df<-inf %>% filter(region==input$country) #Selection des donnees choisit par l'utilisateur
   
    # tracer les series temporelles des paysselectionnees
    hchart(df, "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",shared = TRUE, borderWidth = 2) %>%
    hc_title(text="Taux d'inflation selon les annees",align="center") %>%
    hc_subtitle(text="Source de donnees: International Monetary Fund",align="center") %>%
    hc_add_theme(hc_theme_elementary()) 
                                          })
  
  #Tracer les series temporelles du tax d'inflation des pays de references
  output$hc2<-renderHighchart             ({
    
      highchart() %>% 
      hc_xAxis(categories=unique(inf$year)) %>% 
      hc_add_series(name = "India", data = India$inflation) %>% 
      hc_add_series(name = "USA", data = US$inflation) %>%
      hc_add_series(name = "UK", data = UK$inflation) %>%
      hc_add_series(name = "China", data = China$inflation) %>%
      hc_add_series(name = "Germany", data = Ger$inflation) %>%
      hc_add_series(name="Japan",data=Japan$inflation) %>%
      hc_add_series(name="Tunisia",data=Tn$inflation) %>%
      hc_colors(c("yellow","blue","green","purple","darkpink","orange","red")) %>%
      hc_add_theme(hc_theme_elementary())
                          
                                         })
  
  #Tracer les series temporelles du taux d'inflation  des associations economiques
  output$hc3<-renderHighchart           ({
    
    union<-inf %>% filter(region==input$region)

      hchart(union,hcaes(x=year,y=inflation),type="area",color="#2B1F97") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Taux d'inflation pour les associations économiques",align="center") %>%
      hc_subtitle(text="Source de donnee: IMF",align="center") %>%
      hc_add_theme(hc_theme_elementary())
                                         
                                         })
  #Tracer les series temporelles du taux d'inflation dans le monde
  output$hc4<-renderHighchart({
    world<-inf %>% filter(region=="World")
    
      hchart(world,hcaes(x=year,y=inflation),type="area",color="#B915A3") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Taux d'inflation dans le monde",align="center") %>%
      hc_subtitle(text="Source de donnees: International Monetary Fund",align="center") %>%
      hc_add_theme(hc_theme_elementary())
    
                                       })
  #Afficher le jeu de donnees
  
  
  output$table<-renderDataTable({
    DT::datatable(inf)
  })
  

  ############## Bonus : Taux de chomage ################
  output$hc8<-renderHighchart             ({
    
    highchart() %>% 
      hc_xAxis(categories=unique(cho$Year)) %>% 
      hc_add_series(name = "India", data = Indiacho$Chomage) %>% 
      hc_add_series(name = "China", data = Chinacho$Chomage) %>%
      hc_add_series(name="Japan",data=Japancho$Chomage) %>%
      hc_add_series(name="Tunisia",data=Tncho$Chomage) %>%
      hc_add_series(name="Maroc",data=Maroccho$Chomage) %>%
      hc_colors(c("yellow","darkpink","orange","red","blue")) %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  
  
  
  
  
}

