if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(devtools)) { install.packages("devtools"); require(devtools)}
if(!require(shinyBS)) { devtools::install_github("AnalytixWare/ShinySky"); require(shinyBS)}
if(!require(shinyjs)) { install.packages("shinyjs"); require(shinyjs)}
if(!require(shiny)) { install.packages("shiny"); require(shiny)}
if(!require(shinysky)) { install.packages("shinysky"); require(shinysky)}
if(!require(gdata)) { install.packages("gdata"); require(gdata)}
if(!require(data.table)) { install.packages("data.table"); require(data.table)}
if(!require(ggvis)) { install.packages("ggvis"); require(ggvis)}
if(!require(dtplyr)) { install.packages("dtplyr"); require(dtplyr)}
if(!require(rhandsontable)) { install.packages("rhandsontable"); require(rhandsontable)}
if(!require(nlstools)) { install.packages("nlstools"); require(nlstools)}
if(!require(drc)) { install.packages("drc"); require(drc)}
if(!require(DT)) { install.packages("DT"); require(DT)}
if(!require(plotly)) { install.packages("plotly"); require(plotly)}
if(!require(corrplot)) { install.packages("corrplot"); require(corrplot)}
if(!require(ggfortify)) { install.packages("ggfortify"); require(ggfortify)}
if(!require(ClustOfVar)) { install.packages("ClustOfVar"); require(ClustOfVar)}
if(!require(splitstackshape)) { install.packages("splitstackshape"); require(splitstackshape)}
if(!require(dendextend)) { install.packages("dendextend"); require(dendextend)}
if(!require(cluster)) { install.packages("cluster"); require(cluster)}
if(!require(ggdendro)) { install.packages("ggdendro"); require(ggdendro)}
if(!require(tidyr)) { install.packages("tidyr"); require(tidyr)}
if(!require(factoextra)) { install.packages("factoextra"); require(factoextra)}
if(!require(FactoMineR)) { install.packages("FactoMineR"); require(FactoMineR)}
if(!require(vembedr)) { install.packages("vembedr"); require(vembedr)}
if(!require(htmltools)) { install.packages("htmltools"); require(htmltools)}

library(gdata)
library(data.table)
library(ggvis)
library(dtplyr)
library(rhandsontable)
library(nlstools)
library(drc)
library(DT)
library(plotly)
library(corrplot)
library(ggfortify)
library(ClustOfVar)
library(splitstackshape)
library(dendextend)
library(cluster)
library(ggdendro)
library( tidyr)
library(factoextra)
library(FactoMineR)
library(vembedr)
library(htmltools)

shinyServer(function(input, output,session) {
  show("app-content")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN statistiques~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  datainrespl <- reactive({ 
    
    inFile <- input$file3
    if (is.null(inFile))
      return(NULL)
    Tabl=read.csv(inFile$datapath, header=input$header3, sep=input$sep3, 
                  quote=input$quote3)
    
    return(Tabl)
  })
  
  
  output$respldata = DT::renderDataTable({
    
    try(updateSelectInput(session, "quantivar","variable quantitative", choices = names(df())))
    try(updateSelectInput(session, "qualivar","variable qualitative", choices = names(df1())))
    
    try(datatable(datainrespl(),  class = 'cell-border stripe',caption = 'Votre DATA',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
      
      columnDefs = list(list(className = 'dt-center')),
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
        "}"),
      colReorder = TRUE
    )
    ))
  })  
  
  observeEvent(input$file3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Visualisation Data" = "Data Upload",
                      "Resume"= "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  
  
  observeEvent(input$quote3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Visualisation Data" = "Data Upload",
                      "Resume" = "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  observeEvent(input$sep3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Visualisation Data" = "Data Upload",
                      "Resume" = "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  df = reactive({
    
    upload.data = data.frame(datainrespl())
    
    df = data.frame(0)
    nn = "x1"
    df1 = data.frame(0)
    nn1 = "x1"
    for(i in 1:length(upload.data))
    { if (is.numeric(upload.data[,i])){
      df = try(data.frame(df,upload.data[,i]))
      nn = try(c(nn, colnames(upload.data)[i]))
    }
    }
    colnames(df)=nn
    df = df[-1]
    
    df
  })
  
  df1 = reactive({
    upload.data = data.frame(datainrespl())
    
    df1 = data.frame(0)
    nn1 = "x1"
    for(i in 1:length(upload.data))
    { if (!is.numeric(upload.data[,i])){
      
      df1 = try(data.frame(df1,upload.data[,i]))
      nn1 = try(c(nn1, colnames(upload.data)[i]))
    }
    }
    colnames(df1)=nn1
    df1 = df1[-1]
    
    df1
  })
  observeEvent(input$file3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Methode d'utilisation" = "Data Upload",
                      "Visualistion Data"= "Data Upload",
                      "Methode d'utilisation" = "Data Upload",
                      "Resume" = "Data Upload",
                      "Documentation" = "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  
  observeEvent(input$quote3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Methode d'utilisation" = "Data Upload",
                      "Visualistion Data"= "Data Upload",
                      "Methode d'utilisation" = "Data Upload",
                      "Resume" = "Data Upload",
                      "Documentation" = "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  observeEvent(input$sep3,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Methode d'utilisation" = "Data Upload",
                      "Visualistion Data"= "Data Upload",
                      "Methode d'utilisation" = "Data Upload",
                      "Resume" = "Data Upload",
                      "Documentation" = "Data Upload"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
  })
  
  observeEvent( input$sbmitrespl,{ 
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$resplpan,
                      "Methode d'utilisation" = "Resume",
                      "Visualistion Data"= "Resume",
                      "Methode d'utilisation" = "Resume",
                      "Data Upload"="Resume"
    )
    
    updateTabItems(session,"resplpan",restab11)
    
    if(input$quantivar==""){
      toggleModal(session, "emptyrespl", toggle ="close")
    }else{
      
      output$summ = renderPrint({
        summary(datainrespl())
      })
      
      output$box = renderPlot({
        
        data.up = data.frame(datainrespl())
        if(input$dtvis == "BoxPlot"){
          p<-ggplot(data = data.up, aes(x=data.up[,input$qualivar], y=data.up[,input$quantivar], fill=data.up[,input$qualivar])) +
            geom_boxplot()+
            labs(x=input$qualivar, y = input$quantivar)+
            ggtitle(paste("Plot de",input$quantivar,"par",input$qualivar)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title = element_blank())
          
          
        }else if(input$dtvis == "ViolinPlot"){
          p<-ggplot(data = data.up, aes(x=data.up[,input$qualivar], y=data.up[,input$quantivar], fill=data.up[,input$qualivar])) +
            geom_violin()+
            labs(x=input$qualivar, y = input$quantivar)+
            ggtitle(paste("Plot de",input$quantivar,"par",input$qualivar)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title = element_blank())
          
          
        }else if(input$dtvis == "BarPlot"){
          p<-ggplot(data = data.up, aes(x=data.up[,input$qualivar], y=data.up[,input$quantivar], fill=data.up[,input$qualivar])) +
            geom_bar(stat="identity")+
            labs(x=input$qualivar, y = input$quantivar)+
            ggtitle(paste("Plot de",input$quantivar,"par",input$qualivar)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title = element_blank())
          
          
        }else if(input$dtvis == "Cammembert"){
          bp<- ggplot(data = data.up, aes(x="", y=data.up[,input$quantivar], fill=data.up[,input$qualivar]))+
            geom_bar(width = 1, stat = "identity")+
            labs(x=input$qualivar, y = input$quantivar)+
            ggtitle(paste("Plot de",input$quantivar,"par",input$qualivar)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title = element_blank())
          p <- bp + coord_polar("y", start=0)
          
          
        }
        
        output$foo = downloadHandler(
          filename = 'test.png',
          content = function(file) {
            device <- function(..., width, height) {
              grDevices::png(..., width = width, height = height,
                             res = 300, units = "in")
            }
            ggsave(file, plot = p, device = device)
          })
        p
      })
    }
    withProgress(message = 'Chargement en cours ...',
                 value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.1)
                   }
                   
                 })
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END statistiques~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN ACP~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  datainacp <- reactive({ 
    
    inFile <- input$file4
    if (is.null(inFile))
      return(NULL)
    Tabl=read.csv(inFile$datapath, header=input$header4, sep=input$sep4, 
                  quote=input$quote4)
    
    return(Tabl)
  })
  
  
  output$acpdata = DT::renderDataTable({
    try(updateSelectInput(session, "quantivaracp","quanti.sup", choices = names(dfacp())))
    try(updateSelectInput(session, "qualivaracp","quali.sup", choices = names(dfacp1()),selected = names(dfacp1())))
    try(updateSelectInput(session, "grp","Groupe", choices = names(dfacp1())))
    
    try(datatable(datainacp(),  class = 'cell-border stripe',caption = 'Votre DATA',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
      
      columnDefs = list(list(className = 'dt-center')),
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
        "}"),
      colReorder = TRUE
    )
    ))
  })  
  
  observeEvent(input$file4,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$acppan,
                      "Documentation" = "Data Upload",
                      "Valeurs propres" = "Data Upload",
                      "Correlation" = "Data Upload",
                      "Courbe d'individus" = "Data Upload",
                      "Cercle de correlation" = "Data Upload"
    )
    
    updateTabItems(session,"acppan",restab11)
    
  })
  
  
  observeEvent(input$quote4,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$acppan,
                      "Documentation" = "Data Upload",
                      "Valeurs propres" = "Data Upload",
                      "Correlation" = "Data Upload",
                      "Courbe d'individus" = "Data Upload",
                      "Cercle de correlation" = "Data Upload"
    )
    
    updateTabItems(session,"acppan",restab11)
    
  })
  
  observeEvent(input$sep4,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$acppan,
                      "Documentation" = "Data Upload",
                      "Valeurs propres" = "Data Upload",
                      "Correlation" = "Data Upload",
                      "Courbe d'individus" = "Data Upload",
                      "Cercle de correlation" = "Data Upload"
    )
    
    updateTabItems(session,"acppan",restab11)
    
  })
  
  dfacp = reactive({
    
    upload.data = data.frame(datainacp())
    
    df = data.frame(0)
    nn = "x1"
    df1 = data.frame(0)
    nn1 = "x1"
    for(i in 1:length(upload.data))
    { if (is.numeric(upload.data[,i])){
      df = try(data.frame(df,upload.data[,i]))
      nn = try(c(nn, colnames(upload.data)[i]))
    }
    }
    colnames(df)=nn
    df = df[-1]
    
    df
  })
  
  dfacp1 = reactive({
    upload.data = data.frame(datainacp())
    
    df1 = data.frame(0)
    nn1 = "x1"
    for(i in 1:length(upload.data))
    { if (!is.numeric(upload.data[,i])){
      
      df1 = try(data.frame(df1,upload.data[,i]))
      nn1 = try(c(nn1, colnames(upload.data)[i]))
    }
    }
    colnames(df1)=nn1
    df1 = df1[-1]
    
    df1
  })
  
  
  
  observeEvent(input$sbmitacp,{
    
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$acppan,
                      "Documentation" = "Valeurs propres",
                      "Data Upload" = "Valeurs propres",
                      "Correlation" = "Valeurs propres",
                      "Courbe d'individus" = "Valeurs propres",
                      "Cercle de correlation" = "Valeurs propres"
    )
    
    updateTabItems(session,"acppan",restab11)
    
    output$valp = renderPrint({
      c = input$quantivaracp
      d = input$qualivaracp
      h = matrix(0,1,length(input$quantivaracp))
      g = matrix(0,1,length(input$qualivaracp))
      
      colnames(h)=c
      colnames(g)=d
      for( i in 1:length(h))
      {
        h[,c[i]]=which(names(datainacp())==c[i])
      }
      for( i in 1:length(g))
      {
        g[,d[i]]=which(names(datainacp())==d[i])
      }
      
      f = as.vector(h[1,]) 
      f1 = as.vector(g[1,])
      
      if(length(which(colnames(datainacp()) == input$quantivaracp))==0){
        res.pca <- try(PCA(data.frame(datainacp()),scale.unit = T,quanti.sup =NULL,quali.sup =f1 ,ncp=input$ncp, graph = FALSE))
      }else{
        res.pca <- try(PCA(data.frame(datainacp()),scale.unit = T,quanti.sup = f ,quali.sup =f1 ,ncp=input$ncp, graph = FALSE))
        
      }
      if(class(res.pca) == "try-error" || ncol(df())<1){
        toggleModal(session, "emptyacp", toggle ="close")
        return()
      }else{
        eig.val <- get_eigenvalue(res.pca)
        var <- get_pca_var(res.pca)
        
        
        output$ebl = renderPlot({
          
          p = fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))+
            ggtitle("ébouli des valeurs propres")
          output$vlp = downloadHandler(
            filename = 'valeurs propres.png',
            content = function(file) {
              device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
              }
              ggsave(file, plot = p, device = device)
            })
          p
        })
        
        eig.val
      }
    })
    
    output$cor2 = renderPlot({
      
      c = input$quantivaracp
      d = input$qualivaracp
      h = matrix(0,1,length(input$quantivaracp))
      g = matrix(0,1,length(input$qualivaracp))
      
      colnames(h)=c
      colnames(g)=d
      for( i in 1:length(h))
      {
        h[,c[i]]=which(names(datainacp())==c[i])
      }
      for( i in 1:length(g))
      {
        g[,d[i]]=which(names(datainacp())==d[i])
      }
      
      f = as.vector(h[1,]) 
      f1 = as.vector(g[1,])
      
      if(length(which(colnames(datainacp()) == input$quantivaracp))==0){
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup =NULL,quali.sup =f1 , ncp=input$ncp,graph = FALSE)
      }else{
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup = f ,quali.sup =f1 ,ncp=input$ncp, graph = FALSE)
        
      }
      eig.val <- get_eigenvalue(res.pca)
      var <- get_pca_var(res.pca)
      output$cor22 = renderPrint({
        head(var$contrib)
      })
      corrplot(var$cos2, is.corr=FALSE)
    })
    
    output$cos = renderPlot({
      c = input$quantivaracp
      d = input$qualivaracp
      h = matrix(0,1,length(input$quantivaracp))
      g = matrix(0,1,length(input$qualivaracp))
      
      colnames(h)=c
      colnames(g)=d
      for( i in 1:length(h))
      {
        h[,c[i]]=which(names(datainacp())==c[i])
      }
      for( i in 1:length(g))
      {
        g[,d[i]]=which(names(datainacp())==d[i])
      }
      
      f = as.vector(h[1,]) 
      f1 = as.vector(g[1,])
      
      if(length(which(colnames(datainacp()) == input$quantivaracp))==0){
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup =NULL,quali.sup =f1 ,ncp=input$ncp, graph = FALSE)
      }else{
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup = f ,quali.sup =f1 ,ncp=input$ncp, graph = FALSE)
        
      }
      eig.val <- get_eigenvalue(res.pca)
      var <- get_pca_var(res.pca)
      fviz_cos2(res.pca, choice = "var", axes = 1:2)
    })
    
    output$crb = renderPlot({
      c = input$quantivaracp
      d = input$qualivaracp
      h = matrix(0,1,length(input$quantivaracp))
      g = matrix(0,1,length(input$qualivaracp))
      
      colnames(h)=c
      colnames(g)=d
      for( i in 1:length(h))
      {
        h[,c[i]]=which(names(datainacp())==c[i])
      }
      for( i in 1:length(g))
      {
        g[,d[i]]=which(names(datainacp())==d[i])
      }
      
      f = as.vector(h[1,]) 
      f1 = as.vector(g[1,])
      
      if(length(which(colnames(datainacp()) == input$quantivaracp))==0){
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup =NULL,quali.sup =f1 ,ncp=input$ncp, graph = FALSE)
      }else{
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup = f ,quali.sup =f1 , ncp=input$ncp,graph = FALSE)
        
      }
      
      if(input$elip == "oui"){
        p = fviz_pca_ind (res.pca, geom.ind = "point", # Montre les points seulement (mais pas le "text")
                          col.ind = data.frame(datainacp())[,input$grp],addEllipses = TRUE,legend.title = "Groupes"
        )+ggtitle("Courbe d'individus")
        
        
      }else if(input$elip == "non"){
        p= fviz_pca_ind (res.pca, geom.ind = "point", # Montre les points seulement (mais pas le "text")
                         col.ind = data.frame(datainacp())[,input$grp],addEllipses = F,legend.title = "Groupes")+
          ggtitle("Courbe d'individus")
        
      }
      
      
      output$crbind = downloadHandler(
        filename = "courbe d'individus.png",
        content = function(file) {
          device <- function(..., width, height) {
            grDevices::png(..., width = width, height = height,
                           res = 300, units = "in")
          }
          ggsave(file, plot = p, device = device)
        })
      p
    })
    
    output$cercle = renderPlot({
      c = input$quantivaracp
      d = input$qualivaracp
      h = matrix(0,1,length(input$quantivaracp))
      g = matrix(0,1,length(input$qualivaracp))
      
      colnames(h)=c
      colnames(g)=d
      for( i in 1:length(h))
      {
        h[,c[i]]=which(names(datainacp())==c[i])
      }
      for( i in 1:length(g))
      {
        g[,d[i]]=which(names(datainacp())==d[i])
      }
      
      f = as.vector(h[1,]) 
      f1 = as.vector(g[1,])
      
      if(length(which(colnames(datainacp()) == input$quantivaracp))==0){
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup =NULL,quali.sup =f1 ,ncp=input$ncp, graph = FALSE)
      }else{
        res.pca <- PCA(data.frame(datainacp()),scale.unit = T,quanti.sup = f ,quali.sup =f1 , ncp=input$ncp,graph = FALSE)
        
      }
      p = fviz_pca_var(res.pca, col.var = "cos2",
                       repel = TRUE # Évite le chevauchement de texte
      )+ggtitle("Cercle de corrélation")
      
      output$crc = downloadHandler(
        filename = 'Cercle de corrélation.png',
        content = function(file) {
          device <- function(..., width, height) {
            grDevices::png(..., width = width, height = height,
                           res = 300, units = "in")
          }
          ggsave(file, plot = p, device = device)
        })
      p
    })
    withProgress(message = 'Chargement en cours ...',
                 value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.1)
                   }
                   
                 })
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END ACP~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BEGIN Multiple linear regression~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  datainReg <- reactive({ 
    if(is.null(input$MLFdtup)){
      toggleModal(session, "emptyReg", toggle ="close")
      return()
    }else if ( input$MLFdtup == 'File Upload') {
      inFile <- input$file1
      if(is.null(inFile))
      { return(NULL)}
      
      #load file that was specified by user and save into a uploaded data frame
      Tabl=read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
      
    }
    
    return(Tabl)
  })
  
  output$dataRegout = DT::renderDataTable({
    upload.data = data.frame(datainReg())
    df = data.frame(0)
    nn = "x1"
    for(i in 1:length(upload.data))
    { if (is.numeric(upload.data[,i])){
      df = try(data.frame(df,upload.data[,i]))
      nn = try(c(nn, colnames(upload.data)[i]))
    }
    }
    colnames(df)=nn
    df = df[-1]
    updateSelectInput(session, "choose_columns1", NULL, choices = names(df))
    updateSelectInput(session, "choose_columns2", NULL, choices = names(df))
    updateSelectInput(session, "corval", NULL, choices = names(df),selected = names(df))
    
    
    datatable(datainReg(),  class = 'cell-border stripe',caption = 'Votre DATA',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
      
      columnDefs = list(list(className = 'dt-center')),
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
        "}"),
      colReorder = TRUE
    )
    )
  })     
  
  
  
  ############################################################
  
  observeEvent(input$MLFdtup,{
    rm(list=ls())
    environment= environment()
    
    
    restab11 = switch(input$MLFres,
                      "Methode d'utilisation" = "Data Upload",
                      "Resultats Statistiques" = "Data Upload",
                      "Documentation" = "Data Upload",
                      "Régression Plot3D" = "Data Upload",
                      "Régression Plot2D" = "Data Upload"
    )
    
    updateTabItems(session,"MLFres",restab11)
    
    
  })
  
  
  observeEvent(input$sbmitReg ,{
    
    rm(list=ls())
    environment= environment()
    
    restab12 = switch(input$MLFres,
                      "Methode d'utilisation" = "Resultats Statistiques",
                      "Matrice de correlation" = "Resultats Statistiques",
                      "Resultats Statistiques" = "Resultats Statistiques",
                      "Data Upload" = "Resultats Statistiques",
                      "Documentation" = "Data Upload"
    )
    
    updateTabItems(session,"MLFres",restab12)
    
    
    if(!is.null(input$MLFdtup)){
      
      if(all(datainReg()=="") || is.null(input$choose_columns2)){
        toggleModal(session, "emptyReg", toggle ="close")
      }
      
      output$correlation = renderPlot({
        df = data.frame(datainReg())
        if(input$mix == "simple"){
          p = try(corrplot(cor(df[,c(input$corval)]),method = input$methode,type = input$type,order = input$order,addrect = input$order1))
          if(class(p)=="try-error"){
            return()
          }else{return(p)}
        }else if(input$mix == "mixed"){
          p = try(corrplot.mixed(cor(df[,c(input$corval)]),lower = input$lower,upper = input$upper))
          if(class(p)=="try-error"){
            return()
          }else{
            
            p
          }
        }
        
        
      })
      
      dataReg <<- reactive({ 
        
        df = data.frame(datainReg())
        
        y = which(colnames(df) == input$choose_columns1)
        df1 = data.frame(df[y])
        
        x=matrix(0,1,length(input$choose_columns2))
        for(i in 1:length(input$choose_columns2)){
          x[1,i] = which(colnames(df) == input$choose_columns2[i])
        }
        for(i in 1:length(x)){
          df1 = try(data.frame(df1,df[x[i]]))
          if (class(df1) == "try-error"){
            return(NULL)
          }
        }
        
        if(input$intercept1 == "oui")
        {
          mod = try(lm( df1[,1] ~ . ,df1[-1]))
          
        }else if(input$intercept1 == "non"){
          mod = try(lm( df1[,1] ~ .-1 ,df1[-1]))
          
        }
        dd = data.frame(coefficients(summary(mod)))
        for(i in 1:nrow(dd)){
          for(j in 1:ncol(dd)){
            dd[i,j]=format(as.numeric(dd[i,j]),scientific = T)
          }
        }
        
        test = 1
        for(i in 1:length(df1))
        { if (is.numeric(df1[,i])){
          test = test*1
        }else{
          test = test*0
        }
        }
        if(test ==1){
          return(dd)
        }else{
          
          toggleModal(session, "emptyReg", toggle ="close")
          return(NULL)
        }
        
        
        
      })
      
      
      
      output$regStat = DT::renderDataTable({
        datatable(dataReg(),  class = 'cell-border stripe',caption = 'Tableau statistisque des coefficients',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
          dom = 'Bt',
          columnDefs = list(list(className = 'dt-center')),
          buttons = 
            list('copy', 'print', list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
            "}"),
          colReorder = TRUE
        )
        )
      })     
      
      dataReg1 <<- reactive({
        
        if(is.null(dataReg())){
          return(NULL)
        }else{
          
          df = data.frame(datainReg())
          y = which(colnames(df) == input$choose_columns1)
          df1 = data.frame(df[y])
          
          x=matrix(0,1,length(input$choose_columns2))
          for(i in 1:length(input$choose_columns2)){
            x[1,i] = which(colnames(df) == input$choose_columns2[i])
          }
          for(i in 1:length(x)){
            df1 = data.frame(df1,df[x[i]])
          }
          if(input$intercept1 == "oui")
          {
            mod = try(lm( df1[,1] ~ . ,df1[-1]))
            if (class(mod) == "try-error"){
              
              return(NULL)
            }
          }else if(input$intercept1 == "non"){
            mod = try(lm( df1[,1] ~ .-1 ,df1[-1]))
            if (class(mod) == "try-error"){
              return(NULL)
            }
          }
          
          
          
          
          
          fdata = matrix(0,6,1)
          
          fdata[1,1]=format(as.numeric(glance(mod)$sigma),scientific = T)
          fdata[2,1]=format(as.numeric(glance(mod)$df.residual),scientific = T)
          fdata[3,1]=format(as.numeric(glance(mod)$r.squared),scientific = T)
          fdata[4,1]=format(as.numeric(glance(mod)$adj.r.squared),scientific = T)
          fdata[5,1]=format(as.numeric(summary(mod)$fstatistic[1]),scientific = T)
          fdata[6,1]=format(as.numeric(glance(mod)$p.value),scientific = T)
          
          fdata = data.frame(fdata)
          
          row.names(fdata)[1] = "Residual  Standard Error"
          row.names(fdata)[2] = "Degrees of Freedom"
          row.names(fdata)[3] ="R-squared"
          row.names(fdata)[4] ="Adjusted R-squared"
          row.names(fdata)[5] ="F-statistic"
          row.names(fdata)[6] ="p-value"
          
          colnames(fdata) = "Valeurs"
          
          fdata
        }}
        
      )
      
      output$regStat1 = DT::renderDataTable({
        dda1 = try(datatable(dataReg1(),  class = 'cell-border stripe',caption = 'Plus de Statistiques',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
          dom = 'Bt',
          columnDefs = list(list(className = 'dt-center')),
          buttons = 
            list('copy', 'print', list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
            "}"),
          colReorder = TRUE
        )
        ))
        if (class(dda1) == "try-error"){
          return()
        }else{return(dda1)}
      })     
      
      
      
      
      dataReg2 <<- reactive({ 
        
        if(is.null(dataReg())){
          return(NULL)
        }else{
          df = data.frame(datainReg())
          
          y = which(colnames(df) == input$choose_columns1)
          df1 = data.frame(df[y])
          
          x=matrix(0,1,length(input$choose_columns2))
          for(i in 1:length(input$choose_columns2)){
            x[1,i] = which(colnames(df) == input$choose_columns2[i])
          }
          for(i in 1:length(x)){
            df1 = try(data.frame(df1,df[x[i]]))
            if (class(df1) == "try-error"){
              return(NULL)
            }
          }
          if(input$intercept1 == "oui")
          {
            mod = try(lm( df1[,1] ~ . ,df1[-1]))
            
          }else if(input$intercept1 == "non"){
            mod = try(lm( df1[,1] ~ .-1 ,df1[-1]))
            
          }
          dd = data.frame(confint(mod))
          if(is.null(dataReg())){
            return(null)
          }else{
            colnames(dd)[1]="2.5%"
            colnames(dd)[2]="97.5%"
            dd
          }
        }
        
        
      })
      
      
      output$regStat2 = DT::renderDataTable({
        dda1 = try(datatable(dataReg2(),  class = 'cell-border stripe',caption = "Intervalles de confiance",  extensions = c('Buttons','ColReorder','Responsive'), options = list(
          dom = 'Bt',
          columnDefs = list(list(className = 'dt-center')),
          buttons = 
            list('copy', 'print', list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
            "}"),
          colReorder = TRUE
        )
        ))
        if (class(dda1) == "try-error"){
          return()
        }else{return(dda1)}
      })     
      
      dataReg3 <<- reactive({ 
        
        if(is.null(dataReg())){
          return(NULL)
        }else{ 
          
          df = data.frame(datainReg())
          
          y = which(colnames(df) == input$choose_columns1)
          
          df1 = data.frame(df[y])
          
          x=matrix(0,1,length(input$choose_columns2))
          for(i in 1:length(input$choose_columns2)){
            x[1,i] = which(colnames(df) == input$choose_columns2[i])
          }
          for(i in 1:length(x)){
            df1 = try(data.frame(df1,df[x[i]]))
            if (class(df1) == "try-error"){
              return(NULL)
            }
          }
          if(input$intercept1 == "oui")
          {
            mod = try(lm( df1[,1] ~ . ,df1[-1]))
            
          }else if(input$intercept1 == "non"){
            mod = try(lm( df1[,1] ~ .-1 ,df1[-1]))
            
          }
          dd = data.frame(anova(mod))
          if(is.null(dataReg())){
            return(null)
          }else{
            colnames(dd)[1]="DF"
            colnames(dd)[2]="somme des carrés"
            colnames(dd)[3]="moyenne des carrés"
            colnames(dd)[4]="statistique de Fisher"
            colnames(dd)[5]="Pr(>F)"
            dd
          }
        }
      })
      
      output$regStat3 = DT::renderDataTable({
        dda1 = try(datatable(dataReg3(),  class = 'cell-border stripe',caption = "Analyse de la variance",  extensions = c('Buttons','ColReorder','Responsive'), options = list(
          dom = 'Bt',
          columnDefs = list(list(className = 'dt-center')),
          buttons = 
            list('copy', 'print', list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download'
            )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
            "}"),
          colReorder = TRUE
        )
        ))
        if (class(dda1) == "try-error"){
          return()
        }else{return(dda1)}
      })   
      
      
      
      
      Lr.df <- reactive({ 
        
        df = data.frame(datainReg())
        
        y = which(colnames(df) == input$choose_columns1)
        df1 = data.frame(df[y])
        
        x=matrix(0,1,1)
        if(length(input$choose_columns2) == 1){
          x[1,1] = which(colnames(df) == input$choose_columns2[1])
          df1 = try(data.frame(df1,df[x[1]]))
          if(class(df1)=="try-error"){
            
            df1 = data.frame(0,0)
            
          }else{
            colnames(df1)[1]="y"  
            colnames(df1)[2]="x" 
            
          }
          
        }else{
          df1 = try(data.frame(0,0))
          colnames(df1)[1]="y"  
          colnames(df1)[2]="x" 
          
        }
        
        if(is.numeric(df1[,1]) & is.numeric(df1[,2])){
          df1
          
        }else{
          df1 = try(data.frame(0,0))
          colnames(df1)[1]="y"  
          colnames(df1)[2]="x" 
          df1
        }
        
        
      })
      
      colorcurve <<- "yellow"
      if(is.null(input$col)){
        return()
      }else{
        colorcurve<<- as.factor(input$col)
      }
      colorpts <<- "yellow"
      if(is.null(input$ptcol)){
        return()
      }else{
        colorpts<<- as.factor(input$ptcol)
      }
      visPR <- try(reactive({
        p = try(data.frame(Lr.df()) %>% ggvis(~x, ~y) %>%
                  
                  layer_model_predictions(model = "lm",se= TRUE,stroke := colorcurve, fill := "#ff0066")%>%
                  layer_points(size := 50, size.hover := 200,
                               fillOpacity := 1, fillOpacity.hover := 0.5,fill:=colorpts ) %>%
                  add_axis("x", title = input$choose_columns2) %>%
                  add_axis("y",title = input$choose_columns1 ) %>%
                  
                  add_axis("x", orient = "top", ticks = 0, title = input$mtitle,
                           properties = axis_props(
                             axis = list(stroke = "white"),
                             labels = list(fontSize = 15),
                             title= list(fontSize =input$mtitlesize)))%>%
                  set_options(width = "auto")
                
                
        )
        if(class(p)=="try-error"){
          df = matrix(0,1,2)
          df = data.frame(df)
          colnames(df)[1]="y"
          colnames(df)[2]="x"
          p = df%>% ggvis(~x, ~y)
          
          p
        }else{
          return(p)
        }
      }))
      p = try(visPR %>% bind_shiny("plot2"))
      if(class(p)=="try-error"){
        output$plot2 = renderGvis({df = matrix(0,1,2)
        df = data.frame(df)
        colnames(df)[1]="y"
        colnames(df)[2]="x"
        p = df%>% ggvis(~x, ~y)
        
        p})
      }else{
        p
      }
      
      
      colorcurve1 <<- "yellow"
      if(is.null(input$col1)){
        return()
      }else{
        colorcurve1<<- as.factor(input$col1)
      }
      colorpts1 <<- "yellow"
      if(is.null(input$ptcol1)){
        return()
      }else{
        colorpts1<<- as.factor(input$ptcol1)
      }
      
      output$plot3 = renderPlotly({
        df = data.frame(datainReg())
        
        y = which(colnames(df) == input$choose_columns1)
        
        df1 = data.frame(df[y])
        
        x=matrix(0,1,2)
        if(length(input$choose_columns2) == 2){
          x[1,1] = which(colnames(df) == input$choose_columns2[1])
          x[1,2] = which(colnames(df) == input$choose_columns2[2])
          df1 = try(data.frame(df1,df[x[1]]))
          df1 = try(data.frame(df1,df[x[2]])) 
          if(class(df1)=="try-error"){
            df1 = matrix(0,1,3)
            df1 =data.frame(df1)
          }else{
            colnames(df1)[1]="y"  
            colnames(df1)[2]="x1"
            colnames(df1)[3]="x2"
            
          }
          
          l <- list(
            font = list(
              family = "sans-serif",
              size = 12,
              color = "#000"),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 2)
          MLFY.df = data.frame(df1)
          if(input$intercept1 == "oui"){
            md=try(lm(y~x1+x2,MLFY.df))
          }else if(input$intercept1 == "non"){
            md=try(lm(y~x1+x2-1,MLFY.df))
          }
          y1=round(predict.lm(md),digits=2)
          grad1 = seq(round(min(MLFY.df$x1),digits=2),round(max(MLFY.df$x1),digits=2),length.out = 7001)
          grad2= seq(round(min(MLFY.df$x2),digits=2),round(max(MLFY.df$x2),digits=2),length.out = 7001)
          grad3 = seq(min(y1),max(y1),length.out = 7001)
          gradY.df=data.frame(grad1,grad2,grad3)
          
          
          p = try(plot_ly()%>%
                    add_trace(MLFY.df,x = ~round(MLFY.df$x1,digits=2), y = ~round(MLFY.df$x2,digits=2), z = ~round(MLFY.df$y,digits=2),name =paste("distribution de",input$choose_columns2[1],",",input$choose_columns2[2],"et",input$choose_columns1), type = 'scatter3d',  mode = "markers", 
                              
                              marker = list(size = 3.5, color = colorpts1 , colorscale = 'Greens', cmin = -20, cmax = 50))%>%
                    
                    add_trace(gradY.df,x = ~grad1, y = ~grad2, z = ~grad3, name = paste("l'alignement des valeurs du modèle") ,type = 'scatter3d',mode = "lines", 
                              
                              line = list(width = 6, color = colorcurve1, colorscale = 'Viridis'))%>% 
                    layout(legend = list(orientation = 'h'))%>% layout(legend = l)%>% 
                    
                    layout(autosize = T,
                           
                           scene = list(
                             xaxis = list(title = input$choose_columns2[1]),
                             yaxis = list(title = input$choose_columns2[2]),
                             zaxis = list(title = input$choose_columns1)
                           ))
                  
          )
          
          
          
          if(class(p)=="try-error"){
            MLFY.df = matrix(0,1,3)
            MLFY.df = data.frame(MLFY.df)
            colnames(MLFY.df)[1]="y"
            colnames(MLFY.df)[2]="x1"
            colnames(MLFY.df)[3]="x2"
            p=NULL
            p
            toggleModal(session, "error3D", toggle ="close")
          }else{
            return(p)
          }
          
        }else{
          
          toggleModal(session, "error3D", toggle ="close")
          return(NULL)
        }
        
        
        
      })
      
      
      
      
      
    }else{
      toggleModal(session, "supplyMLF", toggle ="close")
      return()
    }
    
    withProgress(message = 'Chargement en cours ...',
                 value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.1)
                   }
                   
                 })
    
    
    
    
    
    
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END Multiple linear regression~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Begin classification Test~~~~~~~~~~~~~~~~~~~~~~~~~~#
  output$dataRegout1 = DT::renderDataTable({
    datatable(datainReg1(),  class = 'cell-border stripe',caption = 'Votre DATA',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
      
      columnDefs = list(list(className = 'dt-center')),
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
        "}"),
      colReorder = TRUE
    )
    )
  })     
  
  datainReg1 <- reactive({ 
    if(is.null(input$fildtp)){
      return()
    }else if ( input$fildtp == 'File Upload') {
      inFile <- input$file2
      if(is.null(inFile))
      { return(NULL)}
      
      #load file that was specified by user and save into a uploaded data frame
      Tabl=read.csv(inFile$datapath, header=input$header2, sep=input$sep2, 
                    quote=input$quote2)
      
    }
    
    return(Tabl)
  })
  
  
  output$percent = renderUI({
    prgoressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                            striped = FALSE, active = FALSE, vertical = FALSE) {
      stopifnot(is.numeric(value))
      if (value < 0 || value > 100)
        stop("'value' should be in the range from 0 to 100.", call. = FALSE)
      if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
        stop("'color' should be a valid status or color.", call. = FALSE)
      if (!is.null(size))
        size <- match.arg(size, c("sm", "xs", "xxs"))
      text_value <- paste0(value, "%")
      if (vertical)
        style <- htmltools::css(height = text_value, `min-height` = "2em")
      else
        style <- htmltools::css(width = text_value, `min-width` = "2em")
      tags$div(
        class = "progress",
        class = if (!is.null(size)) paste0("progress-", size),
        class = if (vertical) "vertical",
        class = if (active) "active",
        tags$div(
          class = "progress-bar",
          class = paste0("progress-bar-", color),
          class = if (striped) "progress-bar-striped",
          style = style,
          role = "progressbar",
          `aria-valuenow` = value,
          `aria-valuemin` = 0,
          `aria-valuemax` = 100,
          tags$span(class = if (!label) "sr-only", text_value)
        )
      )
    }
    
    prgoressBar(value = input$tau, label = T, color = "aqua", size = NULL,
                striped = FALSE, active = T, vertical = FALSE)
    
    
  })
  
  
  observeEvent(input$stepTOW,{
    nextab = switch(input$classdt,
                    "Classification" = "Echantillonage"
    )
    updateTabItems(session,"classdt",nextab)
  } )
  observeEvent(input$stepONEbkw,{
    nextab = switch(input$classdt,
                    "Echantillonage" = "Classification"  
    )
    updateTabItems(session,"classdt",nextab)
  })
  
  koptimal<-function(data.input,n,tau,kmin,kmax,meth.class,meth.ech){
    
    for(i in 1:length(rownames(data.input))){
      data = data.input
      rownames(data)[i] = i
    }
    
    if(meth.ech =="Sans remise"){
      ech_sans_remise=function(data,gamma){
        echantillon=sample(1:nrow(data),gamma*nrow(data),replace=F)
        echantillon=sort(echantillon)
        return(echantillon)
      }
      nblinessample = matrix(0,n,tau*nrow(data))
      for (i in 1:n){
        nblinessample [i,] = as.integer(ech_sans_remise(data,tau))
      }
      
    }else if(meth.ech =="Avec remise"){
      ech_avec_remise=function(data,gamma){
        echantillon=sample(1:nrow(data),gamma*nrow(data),replace=T)
        echantillon=sort(echantillon)
        return(echantillon)
      }
      nblinessample = matrix(0,n,tau*nrow(data))
      for (i in 1:n){
        nblinessample [i,] = as.integer(ech_avec_remise(data,tau))
      }
      
    }else if(meth.ech =="Stratifiée"){
      ech_strat=function(data,gamma){
        mm = c(1:nrow(data))
        ech = data.frame(data,mm)
        echantillon = stratified(ech,c(1), gamma)
        echantillon = as.data.frame(echantillon)
        a = echantillon[,length(echantillon)]
        a = sort(a)
        return(a)
      }
      
      
      nblinessample = matrix(0,n,length(ech_strat(data,tau)))
      for (i in 1:n){
        nblinessample [i,] = as.integer(ech_strat(data,tau))
      }
    }
    
    
    moyenneRandmat = matrix(0,1,kmax)
    moyenneRandmat = as.data.frame(moyenneRandmat)
    rownames(moyenneRandmat) <- c("moyennes des indices de Rand")
    
    for(i in 1 : kmax){
      
      colnames(moyenneRandmat)[i] = paste("k =",i)
    }
    
    moyenneRandmat1 = matrix(0,1,1)
    rownames(moyenneRandmat1) <- c("selon les indices de Rand")
    colnames(moyenneRandmat1) = c("K optimal")
    
    moyenneRand = 0
    
    for(i in 1:kmax){
      moyenneRand[i]=0
    }
    
    if(meth.class=="Kmeans"){
      
      for(k in kmin:kmax){
        
        clusterpop = kmeans(data,k,nstart = 1)$cluster
        moy = 0
        
        for(i in 1:n){
          
          echantillon= data[nblinessample[i,],]
          clusterech = kmeans(echantillon,k,nstart = 20)$cluster
          r = rand(clusterpop[nblinessample[i,]],clusterech)
          
          moy=(moy+(r/n))
          
        }
        
        moyenneRand[k]=moy
        
      }
      
      for(i in 1:kmax){ 
        
        moyenneRandmat[1,i]=moyenneRand[i]
        
      }
      
      moyenneRandmat1[1,1] = which(moyenneRand==max(moyenneRand))[length( which(moyenneRand==max(moyenneRand)))]
      
    }else if(meth.class == "Complète"){
      
      for(k in kmin:kmax){
        
        clusterpop = hclust (dist (data),method = "complete")
        moy = 0
        
        for(i in 1:n){
          
          echantillon= data[nblinessample[i,],]
          clusterech = hclust (dist (echantillon),method = "complete")
          r = rand(cutree(clusterpop,k)[nblinessample[i,]],cutree(clusterech,k))
          
          moy=(moy+(r/n))
          
        }
        
        moyenneRand[k]=moy
        
      }
      
      for(i in 1:kmax){ 
        
        moyenneRandmat[1,i]=moyenneRand[i]
        
      }
      
      moyenneRandmat1[1,1] = which(moyenneRand==max(moyenneRand))[length( which(moyenneRand==max(moyenneRand)))]
      
    }else if(meth.class == "Moyenne"){
      
      for(k in kmin:kmax){
        
        clusterpop = hclust (dist (data),method = "average")
        moy = 0
        for(i in 1:n){
          
          echantillon= data[nblinessample[i,],]
          clusterech = hclust (dist (echantillon),method = "average")
          r = rand(cutree(clusterpop,k)[nblinessample[i,]],cutree(clusterech,k))
          
          moy=(moy+(r/n))
          
        }
        
        moyenneRand[k]=moy
        
      }
      for(i in 1:kmax){ 
        
        moyenneRandmat[1,i]=moyenneRand[i]
        
      }
      
      moyenneRandmat1[1,1] = which(moyenneRand==max(moyenneRand))[length( which(moyenneRand==max(moyenneRand)))]
      
    }else if(meth.class == "Hiérarchique"){
      
      for(k in kmin:kmax){
        
        clusterpop = hclust (dist (data),method = "ward.D2")
        moy = 0
        
        for(i in 1:n){
          
          echantillon= data[nblinessample[i,],]
          clusterech = hclust (dist (echantillon),method = "ward.D2")
          r = rand(cutree(clusterpop,k)[nblinessample[i,]],cutree(clusterech,k))
          
          moy=(moy+(r/n))
          
        }
        
        moyenneRand[k]=moy
        
        
      }
      for(i in 1:kmax){ 
        
        moyenneRandmat[1,i]=moyenneRand[i]
        
      }
      
      moyenneRandmat1[1,1] = which(moyenneRand==max(moyenneRand))[length( which(moyenneRand==max(moyenneRand)))]
      
    }else if(meth.class == "Médianne"){
      
      for(k in kmin:kmax){
        
        clusterpop = hclust (dist (data),method = "median")
        moy = 0
        for(i in 1:n){
          
          echantillon= data[nblinessample[i,],]
          clusterech = hclust (dist (echantillon),method = "median")
          r = rand(cutree(clusterpop,k)[nblinessample[i,]],cutree(clusterech,k))
          
          moy=(moy+(r/n))
          
        }
        
        moyenneRand[k]=moy
        
        
      }
      
      for(i in 1:kmax){ 
        
        moyenneRandmat[1,i]=moyenneRand[i]
        
      }
      
      moyenneRandmat1[1,1] = which(moyenneRand==max(moyenneRand))[length( which(moyenneRand==max(moyenneRand)))]
      
      
    }
    
    if(meth.class == "Kmeans"){
      
      data.acp <- princomp(data)
      
      data.acp.km <- kmeans(data.acp$scores, moyenneRandmat1[1,1])
      
      return(c(meth.class,meth.ech,n,moyenneRandmat[,kmin:kmax],moyenneRandmat1))
      
      
    }else{
      
      dend <- as.dendrogram(clusterpop)
      return(c(meth.class,meth.ech,n,moyenneRandmat[,kmin:kmax],moyenneRandmat1))
      
      
    }
    
    
    
  }
  
  data.input = reactive({
    upload.data = try(data.frame(datainReg1()))
    
    df = data.frame(0)
    nn = "x1"
    for(i in 1:length(upload.data))
    { if (is.numeric(upload.data[,i])){
      df = try(data.frame(df,upload.data[,i]))
      nn = try(c(nn, colnames(upload.data)[i]))
    }
    }
    colnames(df)=nn
    df = df[-1]
    df
    
  })
  
  observeEvent(input$nbech,{
    
    rm(list=ls())
    environment= environment()
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Resultats" ,
                         "Les classes" = "Resultats" 
    )
    
    updateTabItems(session,"class1",nextabclass)
  })
  
  observeEvent(input$mthclass,{
    
    rm(list=ls())
    environment= environment()
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Resultats" ,
                         "Les classes" = "Resultats" 
    )
    
    updateTabItems(session,"class1",nextabclass)
  })
  
  observeEvent(input$mthech,{
    
    rm(list=ls())
    environment= environment()
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Resultats" ,
                         "Les classes" = "Resultats" 
    )
    
    updateTabItems(session,"class1",nextabclass)
  })
  
  observeEvent(input$choik,{
    
    rm(list=ls())
    environment= environment()
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Resultats" ,
                         "Les classes" = "Resultats" 
    )
    
    updateTabItems(session,"class1",nextabclass)
  })
  observeEvent(input$tau,{
    
    rm(list=ls())
    environment= environment()
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Resultats" ,
                         "Les classes" = "Resultats" 
    )
    
    updateTabItems(session,"class1",nextabclass)
  })
  
  observeEvent(input$fildtp,{
    rm(list=ls())
    environment= environment()
    
    
    nextabclass = switch(input$class1,
                         "Plot des classes" = "Data Upload" ,
                         "Les classes" = "Data Upload" ,
                         "Resultats" = "Data Upload"  , 
                         "Methode d'utilisation" = "Data Upload", 
                         "Documentation" = "Data Upload"
    )
    
    updateTabItems(session,"class1",nextabclass)
    
    
  })
  
  
  observeEvent(input$sbmitclass,{
    
    if(is.null(input$fildtp)){
      toggleModal(session, "supplyclass", toggle ="close")
    }else{
      
      rm(list=ls())
      environment= environment()
      
      nextabclass = switch(input$class1,
                           "Plot des classes" = "Resultats" ,
                           "Les classes" = "Resultats" ,
                           "Data Upload" = "Resultats", 
                           "Methode d'utilisation" = "Resultats", 
                           "Documentation" = "Resultats"
      )
      
      updateTabItems(session,"class1",nextabclass)
      
      
      
      output$ss=renderDataTable({
        
        data.input = try(data.frame(data.input()))
        
        p = try(koptimal(data.input,input$nbech,input$tau/100,input$choik[1],input$choik[2],input$mthclass,input$mthech))
        if(class(p)=="try-error"){
          toggleModal(session, "emptyclass", toggle ="close")
          return()
        }else{
          c = as.numeric(input$choik[2])-as.numeric(input$choik[1])
          o = p[[(4+c+1)]]
          updateNumericInput(session,"nbclass",value = 1,min=1,max = o)
          
          if(input$choik[1]==input$choik[2]){
            
            output$ss1 = renderText({
              
              paste("Le nombre de classes choisi :",p[[(4+c+1)]])
              
              
            })
            
            output$ss2 = renderText({
              
              paste("La méthode de classification :",p[[1]])
              
              
            })
            
            output$ss3 = renderText({
              
              return(NULL)
              
              
            })
            
            output$ss4 = renderText({
              
              return(NULL)
              
              
            })
          }else{
            
            output$ss1 = renderText({
              
              paste("Le nombre de classes optimal :",p[[(4+c+1)]])
              
              
            })
            
            output$ss2 = renderText({
              
              paste("La méthode de classification :",p[[1]])
              
              
            })
            
            output$ss3 = renderText({
              
              paste("La méthode d'échantillonage :",p[[2]])
              
              
            })
            
            output$ss4 = renderText({
              
              paste("Le nombre d'echantillons utilisés :",p[[3]])
              
              
            })
          }
          
          output$plott2 = renderPlot({
            if(input$mthclass == "Kmeans"){
              
              p = autoplot(pam(data.input,p[[(4+c+1)]]), frame = TRUE, frame.type = 'norm',label = TRUE,label.size = 3)+
                ggtitle("les classes \n Méthode : Kmeans")+ theme(
                  plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))
              
            }else if(input$mthclass == "Moyenne"){
              dend <- data.input%>% scale %>% dist %>% 
                hclust("average") %>% as.dendrogram %>%
                set("branches_k_color", k=o) %>% set("branches_lwd", 1)%>% set("labels_col", k=o)
              
              ggd1 <- as.ggdend(dend)
              if(input$hor == "non"){
                p = ggplot(ggd1, horiz = F, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Moyenne")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))
              }else{
                p = ggplot(ggd1, horiz = T, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Moyenne")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))}
              
            }else if(input$mthclass == "Médianne"){
              dend <- data.input%>% scale %>% dist %>% 
                hclust("median") %>% as.dendrogram %>%
                set("branches_k_color", k=o) %>% set("branches_lwd", 1)%>%  set("labels_col", k=o)
              
              ggd1 <- as.ggdend(dend)
              if(input$hor == "non"){
                p = ggplot(ggd1, horiz = F, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Médianne")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5)) 
              }else{
                p =  ggplot(ggd1, horiz = T, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Médianne")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))}
              
            }else if(input$mthclass == "Complète"){
              dend <- data.input%>% scale %>% dist %>% 
                hclust("complete") %>% as.dendrogram %>%
                set("branches_k_color", k=o) %>% set("branches_lwd", 1)%>%  set("labels_col", k=o)
              
              ggd1 <- as.ggdend(dend)
              if(input$hor == "non"){
                p = ggplot(ggd1, horiz = F, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Complète") + theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))
              }else{
                p = ggplot(ggd1, horiz = T, theme = theme_minimal())+
                  ggtitle("Dendogramme \n Méthode : Complète")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))}
              
            }else if(input$mthclass == "Hiérarchique"){
              
              
              dend <- data.input%>% scale %>% dist %>% 
                hclust("ward.D2") %>% as.dendrogram %>%
                set("branches_k_color", k=o) %>% set("branches_lwd", 1)%>%  set("labels_col", k=o)
              
              ggd1 <- as.ggdend(dend)
              if(input$hor == "non"){
                p = ggplot(ggd1, horiz = F, theme = theme_minimal()) +
                  ggtitle("Dendogramme \n Méthode : Hiérarchique")+ theme(
                    plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))
              }else{p = ggplot(ggd1, horiz = T, theme = theme_minimal())+
                ggtitle("Dendogramme \n Méthode : Hiérarchique")+ theme(
                  plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5))}
            }
            
            output$pltclass = downloadHandler(
              filename = 'plot des classes.png',
              content = function(file) {
                device <- function(..., width, height) {
                  grDevices::png(..., width = width, height = height,
                                 res = 300, units = "in")
                }
                ggsave(file, plot = p, device = device)
              })
            p
            
          })
          
          
          
          output$sss = renderDataTable({
            
            if(input$mthclass == "Kmeans"){ 
              y<-kmeans(data.input,o)
              
              classe <-y$cluster
              dd = data.frame(datainReg1())
              
              dd <-cbind(dd ,classe)
              
              x1<- subset(dd , classe == input$nbclass)
              size1 = nrow(x1)
              
            }else if(input$mthclass == "Hiérarchique"){
              
              y<-dist(data.input)
              clust<-hclust(y,method = "ward.D2")
              classe <-cutree(clust, k=o)
              dd = data.frame(datainReg1())
              
              dd <-cbind(dd ,classe)
              
              x1<- subset(dd , classe == input$nbclass)
              size1 = nrow(x1)
              
            }else if(input$mthclass == "Moyenne"){
              
              y<-dist(data.input)
              clust<-hclust(y,method = "average")
              classe <-cutree(clust, k=o)
              dd = data.frame(datainReg1())
              
              dd <-cbind(dd ,classe)
              
              x1<- subset(dd , classe == input$nbclass)
              size1 = nrow(x1)
              
            }else if(input$mthclass == "Médianne"){
              
              y<-dist(data.input)
              clust<-hclust(y,method = "median")
              classe <-cutree(clust, k=o)
              dd = data.frame(datainReg1())
              
              dd <-cbind(dd ,classe)
              
              x1<- subset(dd , classe == input$nbclass)
              size1 = nrow(x1)
              
            }else if(input$mthclass == "Complète"){
              
              y<-dist(data.input)
              clust<-hclust(y,method = "complete")
              classe <-cutree(clust, k=o)
              dd = data.frame(datainReg1())
              
              dd <-cbind(dd ,classe)
              
              x1<- subset(dd , classe == input$nbclass)
              size1 = nrow(x1)
            }
            
            output$size = renderText({
              paste("La taille de la classe :",size1)
            })
            
            datatable(data.frame(x1),  class = 'cell-border stripe',caption = 'Classe',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
              
              columnDefs = list(list(className = 'dt-center')),
              buttons = 
                list('copy', 'print', list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download'
                )),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
                "}"),
              colReorder = TRUE
            )
            )
            
          })
          
          df = data.frame(p[4:(4+c)])
          
          rownames(df)="moyennes des indices de Rand"
          for(i in 4:(4+c)){
            colnames(df)[i-3]=names(p[i])
          }
          
          for(i in 1:length(df)){
            df[1,i] = format(as.numeric(df[1,i]),scientific = T)
          }
          
          
          datatable(df,  class = 'cell-border stripe',caption = 'Moyennes des indices de Rand',  extensions = c('Buttons','ColReorder','Responsive'), options = list(
            dom = 'Bt',
            columnDefs = list(list(className = 'dt-center')),
            buttons = 
              list('copy', 'print', list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#222d32', 'color': '#fff'});",
              "}"),
            colReorder = TRUE
          )
          )
          
        }
        
      })
      withProgress(message = 'Chargement en cours ...',
                   value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.1)
                     }
                     
                   })
    }
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~END classification TEST~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  output$videoReg = renderUI({
    embed_url("https://www.youtube.com/watch?v=Xh6Rex3ARjc")
  })
  
  output$videoClass = renderUI({
    embed_url("https://www.youtube.com/watch?v=ImbXdYrT59s")
  })
  
  output$videoACP = renderUI({
    embed_url("https://www.youtube.com/watch?v=1QPRsg3Bxok")
  })
  
  output$iris <- downloadHandler(
    filename = function() {
      paste("iris-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(iris, file,row.names=FALSE,col.names=TRUE,sep=";")
    }
  )
  
  output$decathlon <- downloadHandler(
    filename = function() {
      paste("decathlon-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(decathlon, file,row.names=FALSE,col.names=TRUE,sep=";")
    }
  )
  
  output$uscereal <- downloadHandler(
    filename = function() {
      paste("UScereal-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(UScereal, file,row.names=FALSE,col.names=TRUE,sep=";")
    }
  )
  
})