
rm(list = ls())
if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(shinysky)) { devtools::install_github("AnalytixWare/ShinySky"); require(shinysky)}
if(!require(shinyBS)) { install.packages("shinyBS"); require(shinyBS)}
if(!require(shinyjs)) { install.packages("shinyjs"); require(shinyjs)}
if(!require(shinyBS)) { install.packages("shinyBS"); require(shinyBS)}
if(!require(shinythemes)) { install.packages("shinythemes"); require(shinythemes)}
if(!require(shiny)) { install.packages("shiny"); require(shiny)}
if(!require(ggvis)) { install.packages("ggvis"); require(ggvis)}
if(!require(dtplyr)) { install.packages("dtplyr"); require(dtplyr)}
if(!require(rhandsontable)) { install.packages("rhandsontable"); require(rhandsontable)}
if(!require(DT)) { install.packages("DT"); require(DT)}
if(!require(plotly)) { install.packages("plotly"); require(plotly)}

library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shiny)
library(shinysky)
library(shinythemes)
library(ggvis)
library(dtplyr)
library(rhandsontable)
library(DT)
library(plotly)

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Projet Shiny"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "home", icon = icon("home")),
                        menuItem("Statistiques générales", tabName = "statgen", icon = icon("calculator"),
                                 menuSubItem("Résumé et visualisation", tabName = "respl" ,icon = icon("pie-chart")),
                                 menuSubItem("ACP", tabName = "acp",icon = icon("bar-chart"))),
                        menuItem("Régression", tabName = "Mregression", icon = icon("line-chart")),
                        menuItem("Classification", tabName = "class", icon = icon("sitemap"))
                        
                      )
                    ),
                    dashboardBody(
                      fluidRow(
                        useShinyjs(),
                        hidden(
                          div(
                            id = "app-content",
                            
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"),
                            tags$head(tags$style(HTML('
                                                      .main-header .logo {
                                                      font-family:"Orbitron", sans-serif;
                                                      font-weight: bold;
                                                      font-size: 26px;
                                                      }
                                                      .content-wrapper,
                                                      .right-side {
                                                      background-color: white;
                                                      }
                                                      '))),
                            
                            tabItems(
                              
                              tabItem(tabName = "Mregression",
                                      tabBox(id = "MLFup",title ="Options",  width = 3,
                                             fluidRow(
                                               h5(tags$b("1) Choisissez votre Data:")),
                                               radioButtons(inputId = "MLFdtup", NULL, choices = c("File Upload"),selected = character(0)),
                                               
                                               conditionalPanel(
                                                 condition = "input.MLFdtup == 'File Upload'",
                                                 h5(tags$b("2) la variable à expliquer :")),
                                                 selectInput("choose_columns1",NULL,choices = NULL,width="140"),
                                                 h5(tags$b("3) la(les) variable(s) explicative(s) :")),
                                                 selectInput("choose_columns2",NULL,choices = NULL,multiple = TRUE,width="140"),
                                                 h5(tags$b("4) Modèle avec intercept :")),
                                                 radioButtons("intercept1",NULL, choices = c("oui","non"),inline = TRUE)
                                                 
                                               ),
                                               
                                               
                                               div(style = "display:inline-block",actionButton(inputId = "sbmitReg",
                                                                                               label = "Submit", styleclass = "danger")),
                                               
                                               
                                               bsModal("emptyReg", h2(tags$b("ERREUR!"),align = "center"), trigger ="", size ="large",h5(("Vérifier votre"), tags$b("DATA"),align = "center"),h5(("ou vérifier si vous avez séléctionné"), tags$b("une ou des variables explicatives"),align = "center")
                                               ),
                                               bsModal("error3D", h2(tags$b("ATTENTION!"),align = "center"), trigger ="", size ="large",h5(("Cette partie est utilsée pour voir votre régression si vous avez choisi"), tags$b("3 variables"),align = "center")
                                               ),
                                               bsModal("supplyMLF", h2(tags$b(""),align = "center"), trigger ="", size ="large", 
                                                        h5(tags$b("Cliquez sur File Upload et importez votre fichier CSV!"),align = "center"))
                                               
                                               
                                               
                                             )),
                                      tabBox(id ="MLFres",title = tagList(shiny::icon("line-chart"),"Régression"),width = 9, selected = "Methode d'utilisation",
                                             tabPanel("Data Upload", 
                                                      fluidRow(
                                                        column(width = 12,
                                                               
                                                               conditionalPanel(
                                                                 condition = "input.MLFdtup == 'File Upload'",
                                                                 
                                                                 fluidPage(
                                                                   column(width = 4,fileInput('file1','Choisissez un fichier CSV',
                                                                                              accept=c('text/csv', 
                                                                                                       'text/comma-separated-values,text/plain', 
                                                                                                       '.csv')),checkboxInput('header', 'Header', TRUE)),
                                                                   
                                                                   #use radio button control to decide the type of sepator in the file
                                                                   column(width = 4,radioButtons('sep', 'Separator',
                                                                                                 c(Comma=',',
                                                                                                   Semicolon=';',
                                                                                                   Tab='\t'),
                                                                                                 ',')),
                                                                   
                                                                   #use radio button control to decide the type of quotes in the file
                                                                   column(width = 4,radioButtons('quote', 'Quote',
                                                                                                 c(None='',
                                                                                                   'Double Quote'='"',
                                                                                                   'Single Quote'="'"),'"'))),
                                                                 DT::dataTableOutput("dataRegout")
                                                                 
                                                                 
                                                                 
                                                               )))),
                                             
                                             
                                             tabPanel("Resultats Statistiques",
                                                      column(width = 12,DT::dataTableOutput("regStat")),
                                                      column(width = 6,DT::dataTableOutput("regStat1")),
                                                      column(width = 12,DT::dataTableOutput("regStat2")),
                                                      column(width = 12,DT::dataTableOutput("regStat3")) 
                                                      
                                                      
                                             ),tabPanel("Regression Plot2D",
                                                        fluidRow(
                                                          
                                                          column(width = 3,         
                                                                 colourInput("col","Couleur de la droite :", "#D175A3",allowTransparent = TRUE)),
                                                          column(width = 3, 
                                                                 colourInput("ptcol","Couleur des points :", "#81BEF7",allowTransparent = TRUE)),
                                                          column(width = 3,
                                                                 textInput("mtitle","Titre :", "",width = NULL, placeholder = "taper le titre")),
                                                          column(width = 3, 
                                                                 numericInput("mtitlesize","Taille du titre :", "",width = NULL, value = 16))
                                                          
                                                        )
                                                        ,ggvisOutput("plot2") 
                                                        
                                             ),
                                             
                                             tabPanel("Regression Plot3D",
                                                      fluidRow(
                                                        column(width = 4,         
                                                               colourInput("col1","Couleur de la droite :", "#D175A3",allowTransparent = TRUE)),
                                                        column(width = 4, 
                                                               colourInput("ptcol1","Couleur des points :", "#81BEF7",allowTransparent = TRUE))
                                                        
                                                      )
                                                      ,plotlyOutput("plot3")
                                             ),tabPanel("Matrice de correlation",radioButtons("mix",NULL,choices =c("simple","mixed"),inline = T),
                                                        conditionalPanel(
                                                          condition = "input.mix == 'mixed'",
                                                          column(width = 4,
                                                                 selectInput("lower","lower",choices =c("circle", "square", "ellipse", "number", "shade",
                                                                                                        "color", "pie"),width="140")),
                                                          column(width = 4,
                                                                 selectInput("upper","upper",choices = c("circle", "square", "ellipse", "number", "shade",
                                                                                                         "color", "pie"),width="140"))),
                                                        conditionalPanel(
                                                          condition = "input.mix == 'simple'",
                                                          column(width = 4,
                                                                 selectInput("methode","méthode",choices =c("circle", "square", "ellipse", "number", "shade",
                                                                                                            "color", "pie"),width="140")),
                                                          column(width = 4,
                                                                 selectInput("type","type",choices = c("full", "lower", "upper"),width="140")),
                                                          column(width = 4,
                                                                 selectInput("order","ordre",choices = c("original",
                                                                                                         "AOE", "FPC", "hclust", "alphabet"),width="140")),
                                                          column(width = 4,conditionalPanel( condition = "input.order == 'hclust' && input.type == 'full'",sliderInput("order1","nombre de classes",min = 0,max=10,value=0,step=1)))
                                                          
                                                          
                                                        ),
                                                        fluidPage(column(width = 10, selectInput("corval","Choisissez les variables de la matrice de corrélation",choices = NULL,multiple = TRUE)
                                                        ))
                                                        ,
                                                        
                                                        fluidPage(column(width = 10,plotOutput("correlation")))),
                                             
                                             tabPanel("Methode d'utilisation", 
                                                        
                                                        fluidRow(
                                                          tags$blockquote(tags$h5(tags$p(tags$span(style="color:#5858FA",tags$b("Etape 1")),tags$br(),
                                                          "Cliquez sur ",tags$b("File Upload."),tags$br(),"Une liste d'options va vous apparaitre, Choisissez votre fichier CSV qui contient la base que vous voulez travailler avec
                                                                 et mettez les paramètres que vous désiriez."),
                                                          tags$p(tags$span(style="color:#5858FA",tags$b("Etape 2")),tags$br(),
                                                          "En cliquant sur",tags$span(style="color:red",tags$button("submit")),", vous allez avoir vos résultats partant
                                                          du panneau",tags$b("Résultats Statistiques.")),
                                                          tags$p(tags$span(style="color:#5858FA",tags$b("Etape 3")),tags$br(),
                                                                 "Dans les panneaux des plots 2D et 3D, vous pouvez changer les couleurs des graphes en ajoutant le titre. Pour la",tags$b("matrice de corrélation"),
                                                                 ", vous avez le choix de séléctionner l'option que vous voulez pour avoir une idée plus profondes sur 
                                                                  la corrélation des variables"),
                                                          tags$p(tags$code("Chaque fois vous faites une modification,cliquez sur",tags$span(style="color:red",tags$button("submit")),"pour voir les changements."))
                                                          ))
                                                           )),tabPanel("Documentation", 
                                                                      
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               tags$b("pour savoir plus sur la régression, cliquez sur "),tags$b(tags$a(href="http://maths.cnam.fr/IMG/pdf/A-C-P-.pdf", tags$img(src = "Downloads.png", width = "25px", height = "25px")))
                                                                              ,tags$hr()
                                                                              
                                                                        ),
                                                                        column(width = 12, align="center",
                                                                               tags$p(tags$b("Tutoriel sur la régression avec R")),
                                                                              uiOutput("videoReg")
                                                                        )
                                                                      ))
                                      )
                              ),tabItem(tabName = "class",
                                        tabBox(id = "classdt",title =NULL,  width = 3,
                                      
                                               tabPanel("Classification", 
                                                        radioButtons("fildtp",NULL,choices = c("File Upload"),selected = character(0)),
                                                        selectInput("mthclass","Méthode de classification",choices = c("Kmeans","Hiérarchique","Complète","Moyenne","Médianne"),width="140"),
                                                        sliderInput("choik","Nombre max et min de classes", min = 2,max = 10, value = c(2, 6)),
                                                        
                                                        actionButton("stepTOW","Next", styleclass = "warning")
                                               ),
                                               tabPanel("Echantillonage", 
                                                        fluidPage( 
                                                        selectInput("mthech","Méthode déchantillonage",choices = c("Sans remise","Avec remise","Stratifiée"),width="140"),
                                                        
                                                        numericInput("nbech","Nombre d'echantillons",value = 2,min=2,max=10,step=1,width = "70"),
                                                        
                                                        sliderInput("tau","Taux d'échantillonage", value = 80,min = 10, max = 100,step = 10,ticks = F,pos="%"),
                                                        uiOutput("percent"),
                                                        div(style = "display:inline-block",actionButton("stepONEbkw","Previous", styleclass = "primary")),
                                                        div(style = "display:inline-block",actionButton(inputId = "sbmitclass",
                                                                                                        label = "Submit", styleclass = "danger"))),
                                               
                                               bsModal("emptyclass", h2(tags$b("ERREUR!"),align = "center"), trigger ="", size ="large",h5(("Vérifier votre"), tags$b("DATA"),align = "center"),h5(("ou"), tags$b("vos paramètres séléctionnés"),align = "center")
                                               ),
                                               bsModal("supplyclass",trigger ="", size ="large",h5(tags$b("Cliquer sur File Upload et importez votre fivhier CSV"),align = "center")
                                               ))
                                               
                                               
                                        ),
                                        tabBox(id ="class1",title = tagList(shiny::icon("sitemap"),"Classification"),width = 9, selected = "Methode d'utilisation",
                                               tabPanel("Data Upload",
                                                        conditionalPanel(
                                                            condition = "input.fildtp == 'File Upload'",
                                                              fluidPage(
                                                            column(width = 4,fileInput('file2','Choisissez un fichier CSV',
                                                                                       accept=c('text/csv', 
                                                                                                'text/comma-separated-values,text/plain', 
                                                                                                '.csv')),checkboxInput('header2', 'Header', TRUE)),
                                                            
                                                            column(width = 4,radioButtons('sep2', 'Separator',
                                                                                          c(Comma=',',
                                                                                            Semicolon=';',
                                                                                            Tab='\t'),
                                                                                          ',')),
                                                            
                                                            #use radio button control to decide the type of quotes in the file
                                                            column(width = 4,radioButtons('quote2', 'Quote',
                                                                                          c(None='',
                                                                                            'Double Quote'='"',
                                                                                            'Single Quote'="'"),'"')))),
                                                          DT::dataTableOutput("dataRegout1")
                                                          
                                                          
                                                          
                                                        
                                                        
                                               ),
                                               
                                               
                                               tabPanel("Resultats"
                                                        ,verbatimTextOutput("ss2")
                                                        ,verbatimTextOutput("ss3")
                                                        ,verbatimTextOutput("ss4")
                                                        ,verbatimTextOutput("ss1"),
                                                        h5(tags$b("   "))
                                                        ,DT::dataTableOutput("ss")
                                                        
                                                        
                                               ),tabPanel("Plot des classes",
                                                          conditionalPanel(condition = "input.mthclass != 'Kmeans'",
                                                                           radioButtons("hor","Dendogramme Horizontale",c("oui","non"),selected = "non"))
                                                          ,downloadButton('pltclass',"télécharger image"),plotOutput("plott2")  
                                               ),tabPanel("Les classes"
                                                          ,column(width = 4 ,h5(tags$b("La classe numéro")),numericInput("nbclass",NULL,value = 0,min=0,max = 0,width = "70")) ,
                                                          column(width = 8,verbatimTextOutput("size")),
                                                          DT::dataTableOutput("sss")
                                               ),tabPanel("Methode d'utilisation", 
                                                          
                                                          fluidRow(
                                                            tags$blockquote(tags$h5(tags$p(tags$span(style="color:#5858FA",tags$b("Etape 1")),tags$br(),
                                                                                           "Tout d'abord, importez votre fichier CSV dans le panneau",tags$b("Data Upload.")),
                                                                                    tags$p(tags$span(style="color:#5858FA",tags$b("Etape 2")),tags$br(),
                                                                                           "Dand le box qui se trouve à gauche, vous allez trouvez les options à choisir entre la",tags$b("Classification"),"et l'",tags$b("Echantillonage"),". Vous pouvez choisir les options
                                                                                           et vous pouvez également allez d'un panneau à un autre en cliquant sur ",tags$span(style="color:yellow",tags$button("Next")),"et",tags$span(style="color:blue",tags$button("Previous"))),
                                                                                    tags$p(tags$span(style="color:#5858FA",tags$b("Etape 3")),tags$br(),
                                                                                           "En cliquant sur",tags$span(style="color:red",tags$button("submit")),", vous allez avoir vos résultats partant
                                                                                           du panneau",tags$b("Résultats.")),
                                                                                    tags$p(tags$span(style="color:#5858FA",tags$b("Etape 4")),tags$br(),
                                                                                           "Dans les panneaux restants, vous trouvez des options à utiliser pour que vous puissiez comprendre mieux le résultat."),
                                                                                    tags$p(tags$code("Remarque : pour faire une classification simple, il suffit de mettre le maximum et le minimum du slider correspend au nombre de classe dans le même nombre.")),
                                                                                    
                                                                                    tags$p(tags$code("Chaque fois vous faites une modification, cliquez sur",tags$span(style="color:red",tags$button("submit")),"pour voir les les nouveaux résultats."))
                                                                                    )),tags$hr()
                                                          )),tabPanel("Documentation", 
                                                                      
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               tags$b("pour savoir plus sur la classification, cliquez sur "),tags$b(tags$a(href="https://lagunita.stanford.edu/c4x/HumanitiesScience/StatLearning/asset/classification.pdf", tags$img(src = "Downloads.png", width = "25px", height = "25px")))
                                                                               ,tags$hr()
                                                                               
                                                                        ),
                                                                        column(width = 12, align="center",
                                                                               tags$p(tags$b("Tutoriel sur la classification avec R")),
                                                                               uiOutput("videoClass")
                                                                        )
                                                                      ))
                                        )
                              ),
                              tabItem(tabName = "respl",
                                        tabBox(id = "respldt",title =NULL,  width = 3,
                                              
                                               tabPanel("File Upload",   
                                                        fileInput('file3','Choisissez un fichier CSV',
                                                                                       accept=c('text/csv', 
                                                                                                'text/comma-separated-values,text/plain', 
                                                                                                '.csv')),
                                                           checkboxInput('header3', 'Header', TRUE),
                                                            
                                                            radioButtons('sep3', 'Separator',
                                                                                          c(Comma=',',
                                                                                            Semicolon=';',
                                                                                            Tab='\t'),
                                                                                          ','),
                                                            radioButtons('quote3', 'Quote',
                                                                                          c(None='',
                                                                                            'Double Quote'='"',
                                                                                            'Single Quote'="'"),'"'),
                                                        
                                                          div(style = "display:inline-block",actionButton(inputId = "sbmitrespl",
                                                                                                          label = "Submit", styleclass = "danger")),
                                                        
                                                        bsModal("emptyrespl", h2(tags$b("ERREUR!"),align = "center"), trigger ="", size ="large",h5(("Vérifier votre"), tags$b("DATA"),align = "center"),h5(("ou"), tags$b("vos paramètres séléctionnés"),align = "center")
                                                        )
                                               
                                               
                                        )),
                                        tabBox(id ="resplpan",title = tagList(shiny::icon("pie-chart"),"Résumé et visualisation"),width = 9, selected = "Data Upload",
                                               tabPanel("Data Upload",
                                                        DT::dataTableOutput("respldata")
                                                         )
                                                        
                                               ,tabPanel("Resume"
                                                        ,verbatimTextOutput("summ")
                                                        
                                                ),tabPanel("Visualisation Data",
                                                           fluidPage(radioButtons("dtvis","choisissez le type de plot",choices = c("BoxPlot","ViolinPlot","BarPlot","Cammembert"),inline = T),
                                                           
                                                            
                                                           column(width = 4 , selectInput("quantivar","L'axe des x", choices = NULL,width="140")),
                                                           column(width = 4 , selectInput("qualivar","L'axe des y", choices = NULL,width="140"),downloadButton('foo',"télécharger Image"))),
                                                           
                                                           plotOutput("box")  
                                                         
                                                           
                                               ))
                                        
                              ),
                              tabItem(tabName = "acp",
                                      tabBox(id = "acpdt",title ="Paramètres ACP",  width = 3,
                                             
                                                               selectInput("quantivaracp","quanti.sup",choices = NULL,multiple = T,width="140"),
                                                               selectInput("qualivaracp","quali.sup",choices = NULL,multiple = T,width="140"),
                                                               numericInput("ncp","nombre de dimensions",value = 5,min=1,max=10,step = 1,width="70"),
                                                               
                                                               div(style = "display:inline-block",actionButton(inputId = "sbmitacp",
                                                                                                               label = "Submit", styleclass = "danger")),
                                                               bsModal("emptyacp", h2(tags$b("ERREUR!"),align = "center"), trigger ="", size ="large",h5(("Vérifier votre"), tags$b("DATA"),align = "center"),h5(("ou"), tags$b("vos paramètres séléctionnés"),align = "center")
                                                               ) 
                                                               
                                                        
                                                      
                                                      
                                                      
                                             ),
                                      tabBox(id ="acppan",title = tagList(shiny::icon("bar-chart"),"ACP"),width = 9, selected = "Data Upload",
                                             tabPanel("Data Upload",
                                                    fluidPage(  column(width = 4,fileInput('file4','Choisissez un fichier CSV',
                                                                                           accept=c('text/csv', 
                                                                                                    'text/comma-separated-values,text/plain', 
                                                                                                    '.csv')),checkboxInput('header4', 'Header', TRUE)),
                                                      
                                                      
                                                      column(width = 4,radioButtons('sep4', 'Separator',
                                                                   c(Comma=',',
                                                                     Semicolon=';',
                                                                     Tab='\t'),
                                                                   ',')),
                                                      column(width=4,radioButtons('quote4', 'Quote',
                                                                   c(None='',
                                                                     'Double Quote'='"',
                                                                     'Single Quote'="'"),'"'))
                                                      ),DT::dataTableOutput("acpdata")
                                             )
                                             
                                             ,tabPanel("Valeurs propres"
                                                       ,verbatimTextOutput("valp")
                                                       ,downloadButton('vlp',"télécharger image")
                                                       ,plotOutput("ebl")
                                                       
                                             ),tabPanel("Cercle de correlation",
                                                        downloadButton('crc',"télécharger image"),
                                                        plotOutput("cercle")  
                                                        
                                                        
                                             ),
                                             tabPanel("Courbe d'individus",
                                                      column(width = 3,selectInput("grp","Groupe",choices = NULL,width="140")),
                                                      column(width = 4,radioButtons("elip","Ajout des elipses",choices = c("oui","non"),selected = "non",inline = T)),
                                                      downloadButton('crbind',"télécharger image"),
                                                      plotOutput("crb")  
                                                      
                                                      
                                             ),tabPanel("Correlation",
                                                        verbatimTextOutput("cor22"),
                                                        plotOutput("cor2")  
                                                        
                                                        
                                             ),tabPanel(HTML(paste("Cos", tags$sup(2), sep = "")),
                                                        plotOutput("cos")  
                                                        
                                                        
                                             ),tabPanel("Documentation", 
                                                        
                                                        fluidRow(
                                                          column(width = 6,
                                                                 tags$b("pour savoir plus sur ACP, cliquez sur "),tags$b(tags$a(href="http://maths.cnam.fr/IMG/pdf/A-C-P-.pdf", tags$img(src = "Downloads.png", width = "25px", height = "25px")))
                                                                 ,tags$hr()
                                                                 ),
                                                          column(width = 12, align="center",
                                                                 tags$p("Tutoriel ACP avec R"),
                                                                 uiOutput("videoACP")
                                                          )
                                                        ))
                                             )
                                      
                              ),
                              tabItem(tabName = "home",
                                      
                                   fluidRow(   tabPanel(id = "about",title = "", width = 12, side = "right",
                                             
                                             sidebarPanel(width = 3,
                                               tags$div( tags$b(tags$img(src = "IMG.jpg", width = "125px", height = "170px")),
                                                         tags$p(tags$b(""))
                                                         ,tags$p(tags$img(src = "in.png", width = "25px", height = "25px"),tags$b(tags$a(href="https://www.linkedin.com/in/nakkachi-chiheb-b28807151/", tags$i("Chiheb Nakkachi"))))
                                                         
                                                         ,tags$p(tags$img(src = "etudiant.png", width = "25px", height = "25px"),tags$b(tags$a(href="http://www.essai.rnu.tn/accueil.htm", tags$i("Etudiant à l'ESSAIT")))),
                                                         tags$b(tags$img(src = "mail.png", width = "25px", height = "25px"),tags$span(style="color:blue", "nakkachichiheb@gmail.com")))
                                                         
                                               
                                             ),
                                             mainPanel(
                                               tags$div(
                                                 tags$span(style="color:#08088A",tags$h2("Présentation"))
                                               ),tags$div(
                                                 tags$blockquote(tags$h5(
                                                   tags$p( paste("Cette application permet de faire 3 fonctions ;"),
                                                 tags$b("Statistiques générales, Régression et Classification."),
                                                 paste("Elle est applicable à toutes les bases.")),
                                                 tags$p("Il suffit d'importer le fichier CSV et cliquer sur ",
                                                 tags$button(tags$span(style="color:red","Submit")),
                                                 " et vous allez trouver les résulats correspendants à chaque partie.")
                                               ))),tags$hr(),tags$div(
                                                 tags$span(style="color:#0101DF",tags$h3("Statistiques générales"))
                                               ),tags$div(
                                                 tags$span(style="color:#5858FA",tags$h4("Résumé et visualisation"))
                                               ),tags$div(
                                                 tags$blockquote(tags$h5(
                                                   tags$p("Vous pouvez utiliser cette partie pour avoir une idée générale sur votre data."),  
                                                   tags$p("Vous allez trouver un résumé sur les variables ainsi que des plots pour mieux comprendre la base.")
                                                 ))),tags$div(
                                                   tags$span(style="color:#5858FA",tags$h4("ACP"))
                                                 ),tags$div(
                                                   tags$blockquote(tags$h5(
                                                     tags$p("Cette partie vous permet d'avoir des résultats concernant l'analyse en composantes principales."),
                                                     tags$p("Elle informe sur les possibilités de réduction des dimensions que vous voulez utiliser.")
                                                ))),tags$hr(),tags$div(
                                                       tags$span(style="color:#0101DF",tags$h3("Régression"))
                                                     ),tags$div(
                                                       tags$blockquote(tags$h5(
                                                         tags$p("Cette partie vous permet d'obtenir une",tags$b("régression linéaire"),"selon vos choix de paramètres."),
                                                          tags$p("Elle permet de donner des résultats statistiques accompagnés de plots.")
                                                       ))),tags$hr(),tags$div(
                                                         tags$span(style="color:#0101DF",tags$h3("Classification"))
                                                       ),tags$div(
                                                         tags$blockquote(tags$h5(
                                                           tags$p("L'idée de cette fonction est de tester la stabilité de nombre classes dans une base."),
                                                           tags$p("En fait, on va calculer le nombre de classes de la base, puis on va tirer des échantillons de la même Data
                                                                 et calculer le nombre de classes."),tags$p("Ensuite, on va comparer les moyenne des indices de Rand entre les échatillons et la base de façon que l'indice le plus proche de 1 est celui qui correspend au 
                                                                 nombre optimal de classes."),
                                                           tags$p("Les résultats obtenus vont être sous forme de résultats statistiques avec des plots."),
                                                           tags$p(tags$code("Remarque : pour faire une classification simple, il suffit de mettre le maximum et le minimum du slider correspend
                                                                     au nombre de classe dans le même nombre."))                                                           
                                                           ))),tags$hr(),
                                               tags$div(
                                                 tags$blockquote(tags$h5(
                                                   tags$p("Quelques bases que vous pouvez utiliser pour tester l'application."),
                                                   tags$p(downloadButton('iris',"iris"),downloadButton('decathlon',"decathlon"),downloadButton('uscereal',"UScereal"))
                                                 ))),tags$hr()
                                               
                                                      
                                             ))
                              ))
                              
                              
                            ))))))