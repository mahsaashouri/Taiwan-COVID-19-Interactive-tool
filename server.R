

library(shiny)
library(partykit)
library(ggplot2)
library(ggparty)
library(quantmod)
library(dplyr)
library(reshape2)
library(DT)
library(tidyverse)
library(lubridate)
library(ie2misc)
library(forecast)
library(DescTools)
source('olsfc.single.R', local = TRUE)


shinyServer(function(input, output) {
  df_upload <- reactive({
    inFile <-  input$target_upload
    if (is.null(inFile)){
      return(NULL)
    }
    ## reading file & deleting zero series
    confirmed.Taiwan <- read.csv(inFile$datapath, header = TRUE) %>% 
      dplyr::group_by(city) %>%
      filter(mean(cases)!=0) %>%
      as.data.frame()
    return(confirmed.Taiwan)
  })
  ## Series structures
  values <- reactiveValues()
  observe({
    if (is.null(df_upload()))
      return(NULL)
      values$nrows <- nrow(df_upload())
      values$nseries <- length(unique(df_upload()$city))
      values$serieslength <-  nrow(df_upload())/length(unique(df_upload()$city))
      values$frequency <- 7
  })
    df_change <- reactive ({
      if (is.null(df_upload()))
        return(NULL)
      ## date added
      confirmed.Taiwan <- df_upload() %>%
      mutate(Date = rep(rep(ymd("2021-01-01") + 0:(values$serieslength - 1)),values$nseries))
    return(confirmed.Taiwan)
  }) 
  dattrain <- reactive({
    if (is.null(df_change()))
      return(NULL)
    ## trend and season added
    dattr <- df_change() %>%
      dplyr::filter(Date <=  ymd("2021-06-03")) %>%
      as.data.frame()
    nrowst <- nrow(dattr)
    nseriest <- length(unique(dattr$city))
    serieslengtht <-  nrow(dattr)/length(unique(dattr$city))
    dattr <- dplyr::group_by(dattr, Date) %>%
      plyr::mutate(cases = ts(dattr$cases, frequency = values$frequency)) %>%
      plyr::mutate(season = factor(cycle(cases)), trend = rep(1:serieslengtht, nseriest))%>%
      as.data.frame()
    
    ## Normalizing the series 
    dattr <- dattr %>%
      plyr::ddply("city", transform, Confirmed.std = scale(cases)) %>%
      plyr::ddply("city", transform, Confirmed.sd = sd(cases)) %>%
      plyr::ddply("city", transform, Confirmed.mean = mean(cases)) %>%
      as.data.frame()
    
    
    ## category columns
    colsfac <- c('region', 'imported', 'administrative', 'airport')
    dattr <- dattr %>% mutate_at(colsfac, list(~factor(.)))
    
    ## lags added
    category_sort <- sort(unique(dattr$city))
    lag_making <- list()
    for (i in seq_along(category_sort))
      lag_making[[i]] <-
      quantmod::Lag(dattr$Confirmed.std[dattr$city == category_sort[i]], 1:7)
    lag_making <- do.call(rbind.data.frame, lag_making)
    dattr <- dattr[order(dattr$city), ]
    dattr <- cbind.data.frame(dattr, lag_making)
    return(dattr)
  })
  dattest <- reactive({
    if (is.null(df_change()))
      return(NULL)
    datte <- df_change()  %>%
      dplyr::filter(Date >  ymd("2021-06-03"))%>%
      as.data.frame()
    return(datte)
  })
  
  datawhole <- reactive({
    if (is.null(df_change()))
      return(NULL)
    ## trend and season added
    datwh <- df_change() %>%
      as.data.frame()
    nrowst <- nrow(datwh)
    nseriest <- length(unique(datwh$city))
    serieslengtht <-  nrow(datwh)/length(unique(datwh$city))
    datwh <- dplyr::group_by(datwh, Date) %>%
      plyr::mutate(cases = ts(datwh$cases, frequency = values$frequency)) %>%
      plyr::mutate(season = factor(cycle(cases)), trend = rep(1:serieslengtht, nseriest))%>%
      as.data.frame()
    
    ## Normalizing the series 
    datwh <- datwh %>%
      plyr::ddply("city", transform, Confirmed.std = scale(cases)) %>%
      plyr::ddply("city", transform, Confirmed.sd = sd(cases)) %>%
      plyr::ddply("city", transform, Confirmed.mean = mean(cases)) %>%
      as.data.frame()
    
    
    ## category columns
    colsfac <- c('region', 'imported', 'administrative', 'airport')
    datwh <- datwh %>% mutate_at(colsfac, list(~factor(.)))
    
    ## lags added
    category_sort <- sort(unique(datwh$city))
    lag_making <- list()
    for (i in seq_along(category_sort))
      lag_making[[i]] <-
      quantmod::Lag(datwh$Confirmed.std[datwh$city == category_sort[i]], 1:7)
    lag_making <- do.call(rbind.data.frame, lag_making)
    datwh <- datwh[order(datwh$city), ]
    datwh <- cbind.data.frame(datwh, lag_making)
    return(datwh)
  })
  
  fit <- reactive({
    if (is.null(dattrain()))
      return(NULL)
    var <- input$SplitVariables
    var1 <- as.vector(unlist(var))
    form <- "Confirmed.std ~ trend +  season"
    for (i in 1:values$frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- paste(form , '|')
    for (i in 1:(length(var1)-1))
      form <- paste0(form, var1[i], " + ")
    form <- paste0(form, var1[length(var1)])
    formula <- as.formula(form)
    ### defining fit function
    linear <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
      glm(y ~ 0 + x, family = gaussian, start = start, ...)
    }
    depth <- input$Depth
    ## running MOB tree
    MOBtree <- mob( formula, data = dattrain() , fit = linear, control =
                      mob_control(prune = input$Prune,  maxdepth = depth, alpha = 0.01))
  })
  
  fitwh <- reactive({
    if (is.null(datawhole()))
      return(NULL)
    var <- input$SplitVariables
    var1 <- as.vector(unlist(var))
    form <- "Confirmed.std ~ trend +  season"
    for (i in 1:values$frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- paste(form , '|')
    for (i in 1:(length(var1)-1))
      form <- paste0(form, var1[i], " + ")
    form <- paste0(form, var1[length(var1)])
    formula <- as.formula(form)
    ### defining fit function
    linear <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
      glm(y ~ 0 + x, family = gaussian, start = start, ...)
    }
    depth <- input$Depth
    ## running MOB tree
    MOBtree <- mob( formula, data = datawhole() , fit = linear, control =
                      mob_control(prune = input$Prune,  maxdepth = depth, alpha = 0.01))
  })
  
  #  forecasting models - training
  fit2 <- reactive({
    ## number of validation set 
    h1 <- 7
    if (is.null(dattrain()))
      return(NULL)
    train.cluster <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    #test.cluster <- split(na.omit(dattest()), predict(fit(), type = "node", newdata = na.omit(dattest())))
    test.cluster <- list()
    for(i in 1:length(train.cluster)){
      test.cluster[[i]] <- dattest() %>%
        filter(city %in% train.cluster[[i]]$city)
    }
    
    form <- "Confirmed.std ~ trend + season"
    for (i in 1:values$frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- as.formula(form)
    ## defining fit functions for each cluster
    fitt <- list()
    for(i in 1:length(train.cluster))
      fitt[[i]] <- lm(form, data = train.cluster[[i]])
    train.cluster.single <- list()
    for(i in 1:length(train.cluster)){
      train.cluster.single[[i]] <- matrix(train.cluster[[i]]$Confirmed.std, ncol = nrow(train.cluster [[i]])/(values$serieslength - values$frequency - h1), nrow = (values$serieslength - values$frequency - h1))
      colnames(train.cluster.single[[i]]) <- unique(train.cluster[[i]]$city)
    }
    train.cluster.ets <- list()
    for(i in 1:length(train.cluster)){
      train.cluster.ets[[i]] <- matrix(train.cluster[[i]]$cases, ncol = nrow(train.cluster [[i]])/(values$serieslength - values$frequency - h1), nrow = (values$serieslength - values$frequency - h1))
      colnames(train.cluster.ets[[i]]) <- unique(train.cluster[[i]]$city)
    }
    ## Forecast horizon
    h <- 7
    fc.list <- list()
    for(i in 1:length(train.cluster)){
      fc <- array(NA, c(Horizon = h, Series = NCOL(train.cluster.single[[i]]),Method = 2))
      dimnames(fc) <- list(Horizon = paste0('h=',seq(h)), Series = colnames(train.cluster.single[[i]]), Method = c('OLS', 'ETS'))
      
      for(j in seq(NCOL(train.cluster.single[[i]]))){
        ## Single ols model
        fc[,j,'OLS'] <- olsfc.single(train.cluster.single[[i]][,j], h, values$frequency ,maxlag = values$frequency, fitt[[i]])
        ## ETS model
        fc[,j,'ETS'] <- forecast(ets(ts(train.cluster.ets[[i]][,j], frequency = 7)), h = h)$mean
        #fc[,j,'ARIMA'] <- forecast(auto.arima(ts(train.cluster.ets.arima[[i]][,j], frequency = 7)), h = 8)$mean
      }
      fc.list[[length(fc.list)+1]] <- fc
    }
    fc.single <- list()
    for (i in seq(fc.list)) {
      fc.listi <- as.data.frame(fc.list[[i]])
      testt1 <- dplyr::group_by(train.cluster[[i]], city) %>%
        filter(row_number() == c(1:h1))
      meani <- matrix(testt1$Confirmed.mean, ncol = ncol(fc.listi), nrow = h1)
      sdi <- matrix(testt1$Confirmed.sd, ncol = ncol(fc.listi), nrow = h1)
      fci <- matrix(NA, nrow = h, ncol = ncol(fc.listi))
      for(j in 1:(ncol(fc.listi)/2))
        fci[,j] <- (fc.listi[,j]*as.numeric(sdi[1,j])) + as.numeric(meani[1,j])
      for(j in ((ncol(fc.listi)/2)+1):ncol(fc.listi))
        fci[,j] <- fc.listi[,j]
      testi <- cbind.data.frame(matrix(test.cluster[[i]]$cases, ncol = ncol(fc.listi)/2, nrow = h1),
                                matrix(test.cluster[[i]]$cases, ncol = ncol(fc.listi)/2, nrow = h1))
      fc.errori <-  testi - fci[c(1:h1),]
      nameserrori <- cbind.data.frame(matrix(test.cluster[[i]]$city, ncol = ncol(fc.listi)/2, nrow = h1),
                                      matrix(test.cluster[[i]]$city, ncol = ncol(fc.listi)/2, nrow = h1)) 
      fc.errori2 <- as.data.frame(matrix(unlist(fc.errori), ncol = 1))
      colnames(fc.errori2) <- 'error'
      ## Validation set forecast results
      fc.i <-  fc.errori2 %>%
        mutate('fc' = reshape2::melt(fci[c(1:h1),])$value) %>%
        mutate('actual' = reshape2::melt(testi)$value) %>%
        mutate("series" = rep(as.vector(unlist(nameserrori)), each=1)) %>%
        mutate('horizon' = rep(c('h=1', 'h=2', 'h=3', 'h=4', 'h=5', 'h=6', 'h=7'), ncol(fc.errori))) %>%
        mutate('cluster' = paste0('Cluster', i)) %>%
        mutate('method' = rep(c('OLS', 'ETS'), each = (ncol(fc.listi)/2)*h1)) %>%
        mutate('method1' = 'test')
      fc.single[[length(fc.single)+1]] <- fc.i
    }
    
    fc.OLS.ETS <- do.call('rbind.data.frame', fc.single)
  })
  
  fit2wh <- reactive({
    ## number of validation set 
    h1 <- 7
    if (is.null(datawhole()))
      return(NULL)
    whole.cluster <- split(na.omit(datawhole()), predict(fitwh(), type = "node"))
    form <- "Confirmed.std ~ trend + season"
    for (i in 1:values$frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- as.formula(form)
    ## defining fit functions for each cluster
    fitt <- list()
    for(i in 1:length(whole.cluster))
      fitt[[i]] <- lm(form, data = whole.cluster[[i]])
    whole.cluster.single <- list()
    for(i in 1:length(whole.cluster)){
      whole.cluster.single[[i]] <- matrix(whole.cluster[[i]]$Confirmed.std, ncol = nrow(whole.cluster [[i]])/(values$serieslength - values$frequency), nrow = (values$serieslength - values$frequency))
      colnames(whole.cluster.single[[i]]) <- unique(whole.cluster[[i]]$city)
    }
    whole.cluster.ets <- list()
    for(i in 1:length(whole.cluster)){
      whole.cluster.ets[[i]] <- matrix(whole.cluster[[i]]$cases, ncol = nrow(whole.cluster [[i]])/(values$serieslength - values$frequency), nrow = (values$serieslength - values$frequency))
      colnames(whole.cluster.ets[[i]]) <- unique(whole.cluster[[i]]$city)
    }
    ## Forecast horizon
    h <- 7
    fc.list <- list()
    for(i in 1:length(whole.cluster)){
      fc <- array(NA, c(Horizon = h, Series = NCOL(whole.cluster.single[[i]]),Method = 2))
      dimnames(fc) <- list(Horizon = paste0('h=',seq(h)), Series = colnames(whole.cluster.single[[i]]), Method = c('OLS', 'ETS'))
      
      for(j in seq(NCOL(whole.cluster.single[[i]]))){
        ## Single ols model
        fc[,j,'OLS'] <- olsfc.single(whole.cluster.single[[i]][,j], h, values$frequency ,maxlag = values$frequency, fitt[[i]])
        ## ETS model
        fc[,j,'ETS'] <- forecast(ets(ts(whole.cluster.ets[[i]][,j], frequency = 7)), h = h)$mean
        #fc[,j,'ARIMA'] <- forecast(auto.arima(ts(whole.cluster.ets.arima[[i]][,j], frequency = 7)), h = 8)$mean
      }
      fc.list[[length(fc.list)+1]] <- fc
    }
    fc.single.ahead <- list()
    for (i in seq(fc.list)) {
      fc.listi <- as.data.frame(fc.list[[i]])
      testt1 <- dplyr::group_by(whole.cluster[[i]], city) %>%
        filter(row_number() == c(1:h1))
      meani <- matrix(testt1$Confirmed.mean, ncol = ncol(fc.listi), nrow = h1)
      sdi <- matrix(testt1$Confirmed.sd, ncol = ncol(fc.listi), nrow = h1)
      fci <- matrix(NA, nrow = h, ncol = ncol(fc.listi))
      for(j in 1:(ncol(fc.listi)/2))
        fci[,j] <- (fc.listi[,j]*as.numeric(sdi[1,j])) + as.numeric(meani[1,j])
      for(j in ((ncol(fc.listi)/2)+1):ncol(fc.listi))
        fci[,j] <- fc.listi[,j]
      ## Validation set forecast results
      namei <- cbind.data.frame(matrix(whole.cluster[[i]]$city, ncol = ncol(fc.listi)/2, nrow = values$serieslength - h1))
      namei <- namei[1,]
      nametest <- as.vector(namei)
      ## Validation set forecast results
      fc.ahead <- fci %>%
        reshape2::melt() %>%
        select('fc' = value) %>%
        mutate("series" = rep(rep(as.vector(unlist(nametest)), each = h), 2)) %>%
        mutate('horizon' = rep(c('h=8', 'h=9', 'h=10', 'h=11', 'h=12', 'h=13', 'h=14'), ncol(fci)))%>%
        mutate('cluster' = paste0('Cluster', i)) %>%
        mutate('method' = rep(c('OLS', 'ETS'), each = (ncol(fc.listi)/2)*h1)) %>%
        mutate('method1' = 'ahead') 
      fc.single.ahead[[length(fc.single.ahead)+1]] <- fc.ahead 
    }
    
    fc.OLS.ETS.wh <- do.call('rbind.data.frame', fc.single.ahead)
  })
  ## title
  output$titleMSE = renderText({})
  ## MSE for different MOB depth
  output$MSE <- renderTable({
    if (is.null(dattrain()))
      return(NULL)
    x <- na.omit(dattrain())$Confirmed.std
    round(mean((x - predict(fit(), type = "response")) ^ 2), digits = 4)
  }, colnames = FALSE, digits = 4 ,  align = "l")
  # # ## title
  output$titleHeatmap1 = renderText({})
  ## MOB and heatmap plotting
  output$MOBTree1 <- renderPlot({
    h1 <- 7
    if (is.null(dattrain()))
      return(NULL)
    split.confirmed.Taiwan.train <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    for(i in 1: length(split.confirmed.Taiwan.train)){
      split.confirmed.Taiwan.train[[i]]$cluster <-  i
    }
    ## reordering series based on the series values
    order.data <- function(df) {
      new_matrix <- as.data.frame(t(matrix((df$cases - min(df$cases))/(max(df$cases - min(df$cases))), nrow = (values$serieslength - values$frequency - h1), ncol = length(unique(df$city)))))
      new_matrix <- cbind.data.frame("ID" = paste('s', 1:nrow(new_matrix), sep = ''), new_matrix)
      colnames(new_matrix) <- c("ID", paste('t', 1:(values$serieslength - values$frequency - h1), sep = ''))
      new_matrix.melt <-reshape2:: melt(new_matrix)
      order <- dplyr:: arrange(new_matrix, paste('t', 1:(values$serieslength - values$frequency - h1), collapse = ','))
      new_matrix.melt$ID <- factor(new_matrix.melt$ID, levels = order$ID, labels = order$ID)
      return(new_matrix.melt)
    }
    long.confirmed.Taiwan.train <- lapply(split.confirmed.Taiwan.train, order.data)
    long.confirmed.Taiwan.train <- rev(long.confirmed.Taiwan.train)
    for(i in 1: length(long.confirmed.Taiwan.train)){
      long.confirmed.Taiwan.train[[i]]$cluster <- i
    }
    final.long.confirmed.Taiwan.train <- do.call("rbind", long.confirmed.Taiwan.train)
    ## heatmap
    heatmap.plot <- ggplot(final.long.confirmed.Taiwan.train,
                           aes(y = ID, x = variable)) +
      #geom_raster() +
      geom_tile(aes(fill = value)) +
      facet_grid(final.long.confirmed.Taiwan.train$cluster~., scales="free_y", space = "free") +
      scale_fill_gradientn(colours=c("white", "green", "darkgreen", "pink", "red", "darkred"),
                           values= scales::rescale(c(0, 0.2, 0.4, 0.6, 0.8, 1)), guide="colorbar") +
      #scale_fill_gradient2(high = "red",  low = "darkgreen", guide="colorbar") +
      theme_test()+
      theme(legend.position = "bottom", text  = element_text(size = 12),
            panel.spacing = unit(0.1, "lines"),axis.text.y=element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(), axis.ticks=element_blank(),
            axis.title.y = element_blank())
    if ( input$Depth == 1 ){
      plot(heatmap.plot)
    }
    else{
      ## MOB
      tree.plot <- ggparty(fit()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 4, fontface = "bold") +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 12),
                           list(size = 12),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      ## printing both plots
      grid.newpage()
      print(heatmap.plot, vp = viewport(x = 0.25, y = 0.5, width = 0.5, height = 1))
      print(tree.plot, vp = viewport(x = 0.75, y = 0.51, width = 0.5, height = 1))
    }
    
  })
  
  output$titleHeatmap2 = renderText({})
  # ## heatmap by day of week
  output$MOBTree2 <- renderPlot({
    h1 <- 7
    if (is.null(dattrain()))
      return(NULL)
    split.confirmed.Taiwan.train <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    for(i in 1: length(split.confirmed.Taiwan.train)){
      split.confirmed.Taiwan.train[[i]]$cluster <-  i
    }
    order.day <- function(df){
      m.day <- as.data.frame(t(matrix((df$cases - min(df$cases))/(max(df$cases - min(df$cases))), nrow = (values$serieslength - values$frequency - h1),
                                      ncol =  length(unique(df$city)))))
      colnames(m.day) <- c(rep(c('Fri' ,'Sat', 'Sun' ,'Mon' ,'Tue', 'Wed', 'Thu'), (values$serieslength - (2*values$frequency) - h1)/values$frequency), c('Fri' ,'Sat', 'Sun' ,'Mon' ,'Tue', 'Wed'))
      m.day2 <-  split.default(m.day, names(m.day))
      m.day3 <- cbind.data.frame(m.day2$Mon, m.day2$Tue, m.day2$Wed, m.day2$Thu, m.day2$Fri, m.day2$Sat, m.day2$Sun)
      return(reshape2::melt(t(m.day3)))
    }
    ordar.day.confirmed <- lapply(split.confirmed.Taiwan.train, order.day)
    sort.dat.row <- function(df){
      new_matrix <- as.data.frame(t(matrix(df$value, nrow = (values$serieslength - values$frequency -  h1 - 1), ncol = nrow(df)/(values$serieslength - values$frequency - h1 - 1))))
      colnames(new_matrix) <- unique(df$Var1)
      new_matrix <- cbind.data.frame("ID" = paste('s', 1:nrow(new_matrix), sep = ''), new_matrix)
      new_matrix.melt <-reshape2:: melt(new_matrix)
      order <- dplyr::arrange(new_matrix, paste('Fri', paste('Fri', 1:(values$serieslength - (2*values$frequency) - h1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Sat', paste('Sat', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Sun', paste('Sun', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Mon', paste('Mon', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Tue', paste('Tue', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Wed', paste('Wed', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','),
                              paste('Thu', paste('Thu', 1:(values$serieslength - (2*values$frequency) - 1)/values$frequency, collapse = ','),  collapse = ','))
      new_matrix.melt$ID <- factor(new_matrix.melt$ID, levels = order$ID, labels = order$ID)
      return(new_matrix.melt)
    }
    order.day.confirmed.Taiwan.train <- lapply(ordar.day.confirmed, sort.dat.row)
    order.day.confirmed.Taiwan.train <- rev(order.day.confirmed.Taiwan.train)
    for(i in 1: length(order.day.confirmed.Taiwan.train)){
      order.day.confirmed.Taiwan.train[[i]]$cluster <- i
    }
    final.order.day.confirmed.Taiwan.train <- do.call("rbind", order.day.confirmed.Taiwan.train)
    
    heatmap.plot2 <- ggplot(final.order.day.confirmed.Taiwan.train , aes(y = ID, x = variable)) +
      geom_tile(aes(fill = value)) +
      #geom_raster() +
      facet_grid(final.order.day.confirmed.Taiwan.train$cluster~., scales="free_y", space = "free") +
      scale_fill_gradientn(colours=c("white", "green", "darkgreen", "pink", "red", "darkred"),
                           values= scales::rescale(c(0, 0.2, 0.4, 0.6, 0.8, 1)), guide="colorbar") +
      #scale_fill_gradient2(high = "red",  low = "darkgreen", guide="colorbar") +
      scale_x_discrete(labels = as.character(c(rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency - 10), 'Fri', rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Sat',
                                               rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Sun', rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Mon',
                                               rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Tue', rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Wed',
                                               rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency), 'Thu', rep('', (values$serieslength - (2*values$frequency) - 1)/values$frequency - 5)))) +
      theme_test()+
      theme(legend.position = "bottom", text  = element_text(size = 15),
            panel.spacing = unit(0.1, "lines"),
            axis.title.x = element_blank(), axis.ticks=element_blank(), axis.text.y=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 15),
            axis.title.y = element_blank())
    if ( input$Depth == 1 ){
      plot(heatmap.plot2)
    }
    else{
      ## MOB
      tree.plot <- ggparty(fit()) +
        geom_edge() +
        coord_flip() +
        geom_edge_label( size = 4, fontface = "bold") +
        geom_node_label(
          line_list = list(aes(label = splitvar),
                           aes(label = paste("p =",
                                             formatC(p.value,
                                                     format = "f",
                                                     digits = 2))),
                           aes(label = "")
          ),
          line_gpar = list(list(size = 12),
                           list(size = 12),
                           list(size = 2)
          ),
          # only inner nodes
          ids = "inner") +
        geom_node_info()
      # printing both plots
      grid.newpage()
      print(heatmap.plot2, vp = viewport(x = 0.25, y = 0.5, width = 0.5, height = 0.99))
      print(tree.plot, vp = viewport(x = 0.75, y = 0.51, width = 0.5, height = 0.8))
    }
  })
  ## title
  output$titleline = renderText({})
  ## all series plot
  output$ClusterSeries <- renderPlot({
    if (is.null(dattrain()))
      return(NULL)
    split.confirmed.Taiwan.train <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    split.confirmed.Taiwan.train <- rev(split.confirmed.Taiwan.train)
    for(i in 1: length(split.confirmed.Taiwan.train)){
      split.confirmed.Taiwan.train[[i]]$cluster <- paste0 ("Cluster", i, collapse = ".")
    }
    confirmed.Taiwan2 <- do.call("rbind", split.confirmed.Taiwan.train)
    if ( input$Depth == 1 ){
      ggplot(data = confirmed.Taiwan2, aes( x = confirmed.Taiwan2$trend,
                                            y = (confirmed.Taiwan2$cases - min(confirmed.Taiwan2$cases))/(max(confirmed.Taiwan2$cases)- min(confirmed.Taiwan2$cases)),
                                            group = confirmed.Taiwan2$city)) +
        geom_line(size = 1, col = "gray") +
        stat_summary(fun = mean,geom="line",lwd=1,aes(group=1), col = "red", linetype = "solid") +
        xlab("") + ylab("") +
        theme_light()+
        theme(text  = element_text(size = 20), title = element_text(colour = "darkgreen", size = 15),
              axis.text.y=element_text(size = 10), axis.text.x=element_text(size = 10))
    }
    else{
      ggplot(data = confirmed.Taiwan2, aes( x = confirmed.Taiwan2$trend,
                                            y = (confirmed.Taiwan2$cases - min(confirmed.Taiwan2$cases))/(max(confirmed.Taiwan2$cases)- min(confirmed.Taiwan2$cases)),
                                            group = confirmed.Taiwan2$city)) +
        geom_line(size = 1, col = "gray") +
        stat_summary(fun = mean,geom="line",lwd=1,aes(group=1), col = "red", linetype = "solid") +
        xlab("") + ylab("") +
        facet_wrap(confirmed.Taiwan2$cluster~., ncol = 2, scales = 'free')+
        theme_light()+
        theme(text  = element_text(size = 20), title = element_text(colour = "darkgreen", size = 15),
              axis.text.y=element_text(size = 10), axis.text.x=element_text(size = 12))
    }
  })
  ## title
  output$titleline = renderText({})
  ## Coefficient plot
  output$Coefficientplot <- renderPlot({
    if (is.null(dattrain()))
      return(NULL)
    if ( input$Depth == 1 ){
      coef <- matrix(coef(fit()), nrow = 1)
    }
    else{
      coef <- as.data.frame(coef(fit()))
      coef <- apply(coef,2,rev)
    }
    id <- c()
    for(i in nrow(coef):1){
      id[i] <-  paste0 ("Cluster", i, collapse = ".")
    }
    coef <- cbind.data.frame(coef, id)
    colnames(coef) <- c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                        "Lag5","Lag6","Lag7","Clusters")
    long.data <- reshape:: melt( data.frame(coef, row = 1:nrow(coef)), id.vars = c("Clusters", "row"),
                                 measure.vars = c("Intercept","trend", "Sun","Mon","Tus","Wed","Thr","Fri", "Lag1","Lag2","Lag3","Lag4",
                                                  "Lag5","Lag6","Lag7"), variable.name = "Variable", value.name = "Value")
    ggplot(long.data, aes( x = variable, y = value, group = row, color = Clusters )) + geom_line(size = 0.8) +
      geom_point(size = 1, shape = 15, colour = "gray50") +
      ylab("Coefficients")+
      xlab("")+
      guides(fill=guide_legend(nrow=1)) +
      theme_gray()+
      theme(panel.grid.major.x = element_line(colour = "grey99"),
            axis.title.x = element_text(size = rel(2)),
            axis.text.x = element_text(angle=60,  vjust=0.8, hjust=0.8,size = rel(2)),
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5)),
            legend.direction = "horizontal",
            legend.position = "bottom",
            legend.key.size = unit(1.5, "cm"),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  output$info <- renderText({
    paste0("y=", input$plot_click$y)
  })
  output$titleline = renderText({})
  output$forecasts <- renderPlot({
    if (is.null(dattrain()))
      return(NULL)
    boxplot.stat <- function(x) {
      coef <- 1.5
      n <- sum(!is.na(x))
      # calculate quantiles
      stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
      names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
      iqr <- diff(stats[c(2, 4)])
      outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
      if (any(outliers)) {
        stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
      }
      return(stats)
    }
    dataset <- fit2() %>%
      filter(horizon == c('h=1', 'h=2', 'h=3', 'h=4', 'h=5', 'h=6', 'h=7'))
    
    forecast_plot1 <- ggplot(data = dataset, aes(x = method, y = error, fill = method)) +
      stat_summary(fun.data = boxplot.stat, geom = "boxplot", alpha = 0.5) +
      scale_fill_grey() +
      xlab("") + ylab("Error") +
      guides(fill = guide_legend(nrow = 1, bycol = TRUE)) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom"
      )
    
    forecast_plot2 <-  ggplot(data = dataset, aes(x = error, color = method)) +
      geom_line(stat = "density", size = 1) +
      ylab("Density") + xlab("")+
      xlim (-10,10) +
      theme_minimal() +
      guides(color = guide_legend("Method", nrow = 1))+
      theme(
        axis.text.x = element_text(hjust = 1, size = 12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.key.size = unit(1, "cm"))
    grid.newpage()
    print(forecast_plot1, vp = viewport(x = 0.25, y = 0.5, width = 0.5, height =1))
    print(forecast_plot2, vp = viewport(x = 0.75, y = 0.5, width = 0.5, height = 1.1))
    
  })
  ### correlation coefficients - ols model
  output$ForecastAccuracyTitleOLScorr = renderText({})
  output$ForecastAccuracyOLScorr <- renderTable({
    if (is.null(dattrain()))
      return(NULL)
    testOLS <- fit2() %>%
      filter(horizon == c('h=1', 'h=2', 'h=3', 'h=4', 'h=5', 'h=6', 'h=7'), method == 'OLS') %>%
      group_split(cluster)
    PearsonCC <- list()
    CCC <- list()
    for (i in 1:length(testOLS)) {
      PearsonCC[[i]] <- round(stats::cor(testOLS[[i]]$fc, testOLS[[i]]$actual , method = "pearson", use = "complete.obs"), digits = 3)
      CCC[[i]] <- round(DescTools::CCC(testOLS[[i]]$fc, testOLS[[i]]$actual)$rho.c$est, digits = 3)
    }
    tab1 <- do.call(rbind, PearsonCC)
    tab2 <- do.call(rbind, CCC)
    tab3 <- NULL
    for(i in 1: length(PearsonCC)){
      tab3 <- rbind(tab3, paste('Cluster', i, sep = '') )
    }
    tab11 <- cbind( tab3,  tab1, tab2)
    tab <- rbind(c('', 'Pearson', 'Concordance'), tab11)
  }, colnames = FALSE, align = "l") 
  ## forecast accuracy ols
  output$ForecastAccuracyTitleOLS = renderText({})
  output$ForecastAccuracyOLS <- renderTable({
    if (is.null(dattrain()))
      return(NULL)
    testOLS <- fit2()  %>%
      filter(horizon == c('h=1', 'h=2', 'h=3', 'h=4', 'h=5', 'h=6', 'h=7'), method == 'OLS') 
    MAEOLS <-  round((mean(abs(testOLS$actual - testOLS$fc))) , digits = 3)
    RMSEOLS <- round(sqrt(mean((testOLS$actual - testOLS$fc)^2)) , digits = 3)
    tab <- rbind("RMSE" = RMSEOLS,
                 "MAE" = MAEOLS)
    cbind(c('RMSE', 'MAE'), tab)
  }, colnames = FALSE, align = "l") 
  ## forecast accuracy ets
  output$ForecastAccuracyTitleETS = renderText({})
  output$ForecastAccuracyETS <- renderTable({
    if (is.null(dattrain()))
      return(NULL)
    testETS <- fit2()  %>%
      filter(horizon == c('h=1', 'h=2', 'h=3', 'h=4', 'h=5', 'h=6', 'h=7'), method == 'ETS') 
    MAEETS <-  round((mean(abs(testETS$actual - testETS$fc))) , digits = 3)
    RMSEETS <- round(sqrt(mean((testETS$actual - testETS$fc)^2)) , digits = 3)
    
    tab <- rbind("RMSE" = RMSEETS,
                 "MAE" =  MAEETS)
    cbind(c('RMSE', 'MAE'), tab)
  }, colnames = FALSE, align = "l") 
  #######################################################################
  ## title
  output$titleforefutureOLS = renderText({})
  output$forecastfutureOLS <- DT::renderDataTable({
    if (is.null(dattrain()))
      return(NULL)
    dataset1.OLS <- fit2wh() %>%
      filter(method == 'OLS') %>%
      select(fc, series, horizon)
    
    dataset2.OLS <- as.data.frame(matrix(trunc(dataset1.OLS$fc), nrow = 7))
    coltitle.OLS <- fit2() %>%
      filter(horizon == c('h=1'), method == 'OLS') %>%
      select(series)
    colnames(dataset2.OLS) <- as.vector(coltitle.OLS$series)
    dataset2.OLS <- cbind.data.frame('horizon' = c(1:7), dataset2.OLS)
    DT::datatable(dataset2.OLS, class = 'cell-border stripe', rownames = FALSE,
                  extensions = c('Buttons', 'Scroller'), options = list(scrollY = 300,
                                                                        scrollX = 500,
                                                                        deferRender = TRUE,
                                                                        scroller = TRUE,
                                                                        buttons = list('excel'),
                                                                        dom = 'lBfrtip',
                                                                        fixedColumns = TRUE))
    
  })
  output$titleforefutureETS = renderText({})
  output$forecastfutureETS <- DT::renderDataTable({
    if (is.null(dattrain()))
      return(NULL)
    dataset1.ETS <- fit2wh() %>%
      filter(method == 'ETS') %>%
      select(fc, series, horizon)
    
    dataset2.ETS <- as.data.frame(matrix(trunc(dataset1.ETS$fc), nrow = 7))
    coltitle.ETS <- fit2() %>%
      filter(horizon == c('h=1'), method == 'ETS') %>%
      select(series)
    colnames(dataset2.ETS) <- as.vector(coltitle.ETS$series)
    dataset2.ETS <- cbind.data.frame('horizon' = c(1:7), dataset2.ETS)
    DT::datatable(dataset2.ETS, class = 'cell-border stripe', rownames = FALSE,
                  extensions = c('Buttons', 'Scroller'), options = list(scrollY = 300,
                                                                        scrollX = 500,
                                                                        deferRender = TRUE,
                                                                        scroller = TRUE,
                                                                        buttons = list('excel'),
                                                                        dom = 'lBfrtip',
                                                                        fixedColumns = TRUE))
  })
  
  # screenshot
  observeEvent(input$go, {
    screenshot(scale = 3,
               filename = "screenshot")
  })
})
