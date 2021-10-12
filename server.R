
library(shiny)
library(partykit)
library(ggplot2)
library(ggparty)
library(quantmod)
library(dplyr)
library(reshape2)
library(plyr)
library(dtwclust)
library(DT)
library(strucchange)
library(data.table)
library(tidyverse)
library(lubridate)


shinyServer(function(input, output) {
  df_products_upload <- reactive({
    inFile <-  input$target_upload
    if (is.null(inFile))
      return(NULL)
    confirmed.Taiwan <- read.csv(inFile$datapath, header = TRUE)

    confirmed.Taiwan <- confirmed.Taiwan[ ave(confirmed.Taiwan$cases != 0,confirmed.Taiwan$city, FUN = any), ]
    # Series structures
    nrows <- nrow(confirmed.Taiwan)
    nseries <- length(unique(confirmed.Taiwan$city))
    serieslength <- nrows/nseries
    frequency <- 7
    ## date added
    confirmed.Taiwan <- confirmed.Taiwan %>%
      mutate(Date = rep(rep(ymd("2021-01-01") + 0:(serieslength - 1)),nseries))
    ## trend and season added
    confirmed.Taiwan <- group_by(confirmed.Taiwan, Date) %>%
      mutate(cases = ts(confirmed.Taiwan$cases, frequency = frequency)) %>%
      mutate(season = factor(cycle(cases)), trend = rep( 1:serieslength, nseries ))

      ## Normalizing the series
      confirmed.Taiwan <- confirmed.Taiwan %>%
      ddply("city", transform, Confirmed.std = scale(cases)) %>%
      ddply("city", transform, Confirmed.sd = sd(cases)) %>%
      ddply("city", transform, Confirmed.mean = mean(cases))
     confirmed.Taiwan$mean<- mean(confirmed.Taiwan$cases)
     confirmed.Taiwan$sd <- sd(confirmed.Taiwan$cases)
    
     ## category columns
     colsfac <- c('region', 'imported', 'administrative', 'airport')
     confirmed.Taiwan <- confirmed.Taiwan %>% mutate_at(colsfac, list(~factor(.)))

    ## lags added
     category_sort <- sort(unique(confirmed.Taiwan$city))
     lag_making <- list()
     for (i in seq_along(category_sort))
       lag_making[[i]] <-
       Lag(confirmed.Taiwan$Confirmed.std[confirmed.Taiwan$city == category_sort[i]], 1:frequency)
     lag_making <- do.call(rbind, lag_making)
     confirmed.Taiwan <- confirmed.Taiwan[order(confirmed.Taiwan$city), ]
     confirmed.Taiwan <- cbind(confirmed.Taiwan, lag_making)
    return(confirmed.Taiwan)
  }) 
  
  dattrain <- reactive({
    if (is.null(df_products_upload()))
      return(NULL)
    dattr <- df_products_upload() %>%
       dplyr::filter(Date <=  ymd("2021-06-03"))
     return(dattr)
   })
   dattest<- reactive({
     if (is.null(df_products_upload()))
       return(NULL)
     datte <- df_products_upload()  %>%
       dplyr::filter(Date >  ymd("2021-06-03"))
     return(datte)
   })

  fit <- reactive({
    if (is.null(dattrain()))
      return(NULL)
    var <- input$SplitVariables
    var1 <- as.vector(unlist(var))
    form <- "Confirmed.std ~ trend +  season"
    for (i in 1:frequency)
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
  fit2 <- reactive({
    if (is.null(dattrain()))
      return(NULL)
    train.cluster <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    test.cluster <- split(na.omit(dattest()), predict(fit(), type = "node", newdata = na.omit(dattest())))
    form <- "Confirmed.std ~ trend +  season"
    for (i in 1:frequency)
      form <- paste(form , " + ", paste("Lag", i, sep = '.'))
    form <- as.formula(form)
    ## defining fit functions for each cluster
    fitt <- list()
    for(i in 1:length(train.cluster))
      fitt[[i]] <- lm(form, data = train.cluster[[i]])
    train.cluster.single <- list()
    for(i in 1:length(train.cluster)){
      train.cluster.single[[i]] <- matrix(train.cluster[[i]]$Confirmed.std, ncol = nrow(train.cluster [[i]])/(serieslength - frequency - 1), nrow = (serieslength - frequency - 1))
      colnames(train.cluster.single[[i]]) <- unique(train.cluster[[i]]$city)
    }
    h <- 8
    fc.single.list <- list()
    for(i in 1:length(train.cluster)){
      fc.single <- array(NA, c(Horizon = h, Series = NCOL(train.cluster.single[[i]]),Method = 1))
      dimnames(fc.single) <- list(
        Horizon = paste0("h=",seq(h)),
        Series = colnames(train.cluster.single[[i]]),
        Method = c('OLS.single'))

      for(j in seq(NCOL(train.cluster.single[[i]]))){
        fc.single[,j,'OLS.single'] <- olsfc.single(train.cluster.single[[i]][,j], h, frequency ,maxlag = frequency, fitt[[i]])
      }
      fc.single.list[[length(fc.single.list)+1]] <- fc.single
    }
    library(mefa)
    tests <- list()
    for (i in 1:length(test.cluster)){
      tests1 <-  rep(test.cluster[[i]], each = h)
      tests[[length(tests)+1]] <- tests1
    }

    fc.single <- list()
    for (i in seq(fc.single.list)) {
      res.value <- data.frame('fc' = ((reshape2::melt(fc.single.list[[i]])$value*tests[[i]]$Confirmed.sd) + (tests[[i]]$Confirmed.mean)))
      res.value$Error <- tests[[i]]$cases - res.value$fc
      res.value$Series <- reshape2::melt(fc.single.list[[i]])$Series
      res.value$Horizon <- reshape2::melt(fc.single.list[[i]])$Horizon
      res.value$Cluster <- paste0('Cluster', i)
      fc.single[[length(fc.single)+1]] <- res.value
    }
    fc.single <- do.call('rbind', fc.single)
    fc.single $Method <- 'OLS.single'
    fc.single <- as.data.frame(fc.single)
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
  ## title
  output$titleHeatmap1 = renderText({})
  ## MOB and heatmap plotting
  output$MOBTree1 <- renderPlot({
    if (is.null(dattrain()))
      return(NULL)
    split.confirmed.Taiwan.train <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    for(i in 1: length(split.confirmed.Taiwan.train)){
      split.confirmed.Taiwan.train[[i]]$cluster <-  i
    }
    ## reordering series based on the series values
    order.data <- function(df) {
      new_matrix <- as.data.frame(t(matrix((df$cases - min(df$cases))/(max(df$cases - min(df$cases))), nrow = (serieslength - frequency - 1), ncol = length(unique(df$city)))))
      new_matrix <- cbind.data.frame("ID" = paste('s', 1:nrow(new_matrix), sep = ''), new_matrix)
      colnames(new_matrix) <- c("ID", paste('t', 1:(serieslength - frequency - 1), sep = ''))
      new_matrix.melt <-reshape2:: melt(new_matrix)
      order <- dplyr:: arrange(new_matrix, paste('t', 1:(serieslength - frequency - 1), collapse = ','))
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
                           aes(y = ID, x = variable, fill = value)) +
      geom_raster() +
      facet_grid( final.long.confirmed.Taiwan.train$cluster~., scales="free_y", space = "free")+
      scale_fill_gradient2(high="red",  low="darkgreen")+
      theme_test()+
      theme(legend.position = "bottom", text  = element_text(size = 15),
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
  ## heatmap by day of week
  output$MOBTree2 <- renderPlot({
    if (is.null(dattrain()))
      return(NULL)
    split.confirmed.Taiwan.train <- split(na.omit(dattrain()), predict(fit(), type = "node"))
    for(i in 1: length(split.confirmed.Taiwan.train)){
      split.confirmed.Taiwan.train[[i]]$cluster <-  i
    }
    order.day <- function(df){
      m.day <- as.data.frame(t(matrix((df$cases - min(df$cases))/(max(df$cases - min(df$cases))), nrow = (serieslength - frequency - 1),
                                      ncol =  length(unique(df$city)))))
      colnames(m.day) <- c(rep(c('Fri' ,'Sat', 'Sun' ,'Mon' ,'Tue', 'Wed', 'Thu'), (serieslength - (2*frequency) - 1)/frequency), c('Fri' ,'Sat', 'Sun' ,'Mon' ,'Tue', 'Wed'))
      m.day2 <-  split.default(m.day, names(m.day))
      m.day3 <- cbind.data.frame(m.day2$Mon, m.day2$Tue, m.day2$Wed, m.day2$Thu, m.day2$Fri, m.day2$Sat, m.day2$Sun)
      return(reshape2::melt(t(m.day3)))
    }
    ordar.day.confirmed <- lapply(split.confirmed.Taiwan.train, order.day)
    sort.dat.row <- function(df){
      new_matrix <- as.data.frame(t(matrix(df$value, nrow = (serieslength - frequency - 2), ncol = nrow(df)/(serieslength - frequency - 2))))
      colnames(new_matrix) <- unique(df$Var1)
      new_matrix <- cbind.data.frame("ID" = paste('s', 1:nrow(new_matrix), sep = ''), new_matrix)
      new_matrix.melt <-reshape2:: melt(new_matrix)
      order <- dplyr::arrange(new_matrix, paste('Fri', paste('Fri', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Sat', paste('Sat', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Sun', paste('Sun', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Mon', paste('Mon', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Tue', paste('Tue', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Wed', paste('Wed', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','),
                              paste('Thu', paste('Thu', 1:(serieslength - (2*frequency) - 1)/frequency, collapse = ','),  collapse = ','))
      new_matrix.melt$ID <- factor(new_matrix.melt$ID, levels = order$ID, labels = order$ID)
      return(new_matrix.melt)
    }
    order.day.confirmed.Taiwan.train <- lapply(ordar.day.confirmed, sort.dat.row)
    order.day.confirmed.Taiwan.train <- rev(order.day.confirmed.Taiwan.train)
    for(i in 1: length(order.day.confirmed.Taiwan.train)){
      order.day.confirmed.Taiwan.train[[i]]$cluster <- i
    }
    final.order.day.confirmed.Taiwan.train <- do.call("rbind", order.day.confirmed.Taiwan.train)

    heatmap.plot2 <- ggplot(final.order.day.confirmed.Taiwan.train ,
                            aes(y = ID, x = variable, fill = value)) +
      geom_raster() +
      facet_grid(final.order.day.confirmed.Taiwan.train$cluster~., scales="free_y", space = "free") +
      scale_fill_gradient2(high = "red",  low = "darkgreen") +
      scale_x_discrete(labels = as.character(c(rep('', (serieslength - (2*frequency) - 1)/frequency - 10), 'Fri', rep('', (serieslength - (2*frequency) - 1)/frequency), 'Sat',
                                               rep('', (serieslength - (2*frequency) - 1)/frequency), 'Sun', rep('', (serieslength - (2*frequency) - 1)/frequency), 'Mon',
                                               rep('', (serieslength - (2*frequency) - 1)/frequency), 'Tue', rep('', (serieslength - (2*frequency) - 1)/frequency), 'Wed',
                                               rep('', (serieslength - (2*frequency) - 1)/frequency), 'Thu', rep('', (serieslength - (2*frequency) - 1)/frequency - 5)))) +
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
              axis.text.y=element_text(size = 10), axis.text.x=element_text(size = 10))
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
            axis.text.x = element_text(angle=60,  vjust=.8, hjust=0.8,size = rel(2)),
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
  ## title
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
      filter(Horizon == 'h=1')

    forecast_plot1 <- ggplot(data = dataset, aes(x = Method, y = Error, fill = Method)) +
      stat_summary(fun.data = boxplot.stat, geom = "boxplot", alpha = 0.5) +
      scale_fill_grey() +
      xlab("") + ylab("Error") +
      guides(fill = guide_legend(nrow = 1, bycol = TRUE)) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none"
      )

    forecast_plot2 <-  ggplot(data = dataset, aes(x = Error, color = Method)) +
      geom_line(stat = "density", size = 1) +
      ylab("Density") + xlab("")+
      xlim (-10,10) +
      theme_minimal() +
      guides(color = guide_legend("Method", nrow = 1))+
      theme(
        axis.text.x = element_text(hjust = 1, size = 15),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.position = "none",
        legend.text = element_text(size = 15),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"))
    grid.newpage()
    print(forecast_plot1, vp = viewport(x = 0.25, y = 0.5, width = 0.5, height =1))
    print(forecast_plot2, vp = viewport(x = 0.75, y = 0.5, width = 0.5, height = 1.1))

  })
  output$titleforefuture = renderText({})
  output$forecastfuture <- DT::renderDataTable({
    if (is.null(dattrain()))
      return(NULL)
    dataset1 <- fit2() %>%
      filter(Horizon != 'h=1') %>%
      select(fc, Series)

    dataset2 <- as.data.frame(matrix(round(dataset1$fc), nrow = 7))
    mah <- fit2() %>%
      filter(Horizon == 'h=1') %>%
      select(Series)

    colnames(dataset2) <- as.vector(mah$Series)

    dataset2 <- cbind.data.frame('Horizon' = c(1:7), dataset2)
    DT::datatable(dataset2, class = 'cell-border stripe', rownames = FALSE,
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
