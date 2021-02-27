#Charger les packages
library(shinydashboard)
require(shiny)
if (!require("highcharter")) install.packages('highcharter')
library(highcharter)
if (!require("DT")) install.packages('DT')
library(DT)

#definition des pays à selectionner pourl'input

country<-c("Tunisia","India","United States","Mexico","Canada","China, People's Republic of","Japan",
           "Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe",
          "ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")

#Elaboration du dashboard
dashboardPage(
  #Header de la page
  skin = "purple",
  dashboardHeader(title="Taux d'inflation" ),
  
  
  #definition de la barre de menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("à propos", tabName = "propos", icon = icon("th")),
      menuItem("Associations Internationales",tabName="unions",icon=icon("signal")),
      menuItem("Monde",tabName="monde",icon=icon("globe")),
      menuItem("Bonus:Taux de chomage",tabName="chomage",icon=icon("globe")),
      menuItem("Data",tabName="data",icon=icon(" fa-database",lib="font-awesome"))
      
              )
                 ),
  #definition du body
  dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myshinycss.css")),
    
    tabItems(
#tab menu dashboard  
tabItem(tabName = "dashboard",
    fluidRow(column(12,box(selectInput("country",label="Selectionner un pays",choices=country),width = 12)),
                #box pour les graphe de séries temporelles
                column(12,box(highchartOutput("hcontainer"),width="12")), 
                hr(),
                h4("Taux d'inflation relatif",align="center"),
                br(),
                column(12,box(highchartOutput("hc2"),width=12) )),
               h4("Dashboard élaborer par", strong("Slim Ben Tanfous")),
    a("Pour me contacter cliquer ici !",target="_blank",href="https://www.linkedin.com/in/slimbentanfous")
       ),
      
      
      
#tab menu à propos
tabItem(tabName="propos",h2("QU’EST-CE QUE L’INFLATION ?",style="text-align:center"),
              br(),
              br(),
              box(width=12,height="400px",
                  p(style="font-size:20px",strong("L'inflation"),"correspond à l'augmentation de la circulation de monnaie et à une hausse généralisée des prix des biens et services, qui est mesurée grâce à l'indice des prix à la consommation (IPC).
                     Plus les prix augmentent, plus le marché financier est instable et crée des inégalités sociales car la valeur des salaires diminue fortement et ne suffit plus à subvenir aux besoins des ménages face aux prix des biens.
                     Les deux principales causes de l'inflation sont donc l'inflation par les coûts et par la demande."),
                  
                  p(style="font-size:20px",strong("La deflation"), "Par opposition à l'inflation, la déflation désigne une baisse des prix constatée sur une longue période. Elle ne doit pas être confondue avec la désinflation qui correspond à la baisse du taux d'inflation.
                      La déflation apparaît lorsque la demande est faible par rapport à la quantité de biens et services disponibles dans l'économie.
                      En économie, la déflation peut correspondre à un ralentissement de l'activité économique. Selon les Keynésiens, la déflation est négative pour l'économie et doit être évitée, car elle pousse à reporter la consommation des ménages thésaurisent qui  leur argent par anticipation d'une baisse des prix. 
                     Les entreprises, quant à elles, diminuent leurs investissements et leur production, ce qui ralentit l'activité économique."))
              ),
#tab menu Associations Internationales 
tabItem(tabName = "unions",h3("Taux d'inflation concernant les associations economiques",align="center") ,
        fluidRow(column(12,box(selectInput("region",label="Selectionner une association economique",choices=unions),width = 12)),
                 box(highchartOutput("hc3"),width=12)) 
        ),
#tab menu monde      
tabItem(tabName = "monde",h3("Taux d'inflation dans le monde",align="center"),box(highchartOutput("hc4"),width=12)
        ),
tabItem(tabName = "chomage",h3("Taux de chomage ",align="center"),box(highchartOutput("hc8"),width=12)
),
tabItem(tabName = "data",h3("Jeu de donnees (apres data management)",align="center"),box(dataTableOutput("table"),width=12) )


 
)#fin tab items
)#fin body
)#fin dashboard
