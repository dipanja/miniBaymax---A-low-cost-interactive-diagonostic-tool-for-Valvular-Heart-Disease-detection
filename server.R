options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard,
               tableHTML,
               dplyr,
               data.table,
               Boruta,
               h2o,
               plotly,
               ggplot2,
               ggpubr,
               GGally,
               h2o,
               lime)

h2o.init(nthreads = -1,
         min_mem_size = "8g",
         max_mem_size = "10g")

fetched.model <- h2o.loadModel(saved.model)



shinyServer(function(input, output){
  
  my_modeling_data <- reactive({
    
    my_modeling_data <- read.csv("modeling_data.csv",
                                 header = T,
                                 sep = ",",
                                 stringsAsFactors = T)
    
    my_modeling_data <- as.data.frame(my_modeling_data)
    
  })
  
  output$o.RawData <- renderDataTable({
    
    if(is.null(my_modeling_data())){return()}
    else
      head(my_modeling_data(), 5)
  }) 
  
  output$o.featureGraphs_conti <- renderPlotly({
    
    p1 <- ggdensity(my_modeling_data(), 
                    x = "Pulse.rate",
                    add = "mean", 
                    rug = TRUE,
                    xlab = "Pulse rate",
                    ylab = F,
                    color = "Valvular.heart.disease", 
                    fill = "Valvular.heart.disease",
                    palette = c("#00AFBB", "#FC4E07"))
    
    p1 <- ggplotly(p1)
    
    p2 <- ggdensity(my_modeling_data(), 
                    x = "EF.TTE",
                    add = "mean", 
                    rug = TRUE,
                    xlab = "Ejection fraction",
                    ylab = F,
                    color = "Valvular.heart.disease", 
                    fill = "Valvular.heart.disease",
                    palette = c("#00AFBB", "#FC4E07"))
    
    p2 <- ggplotly(p2)
    
    p.one <- subplot(p1, 
                     p2,
                     nrows = 1,
                     titleX = T,
                     titleY = T,
                     shareY = F)
    
    
  })
  
  output$o.featureGraphs_discreet <- renderPlotly({
    
    p4 <- ggplot(my_modeling_data(),
                 aes(Diastolic.Murmur,
                     group = Valvular.heart.disease)) +
      geom_bar(aes(y = ..prop..,
                   fill = factor(..x..)),
               stat="count") +
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage frequencies") +
      facet_grid(~Valvular.heart.disease)+
      theme(legend.title=element_blank())+ 
      theme(legend.position="bottom")
    
    p4 <- ggplotly(p4)
    
    p5 <- ggplot(my_modeling_data(),
                 aes(Systolic.Murmur,
                     group = Valvular.heart.disease)) +
      geom_bar(aes(y = ..prop..,
                   fill = factor(..x..)),
               stat="count") +
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage frequencies") +
      facet_grid(~Valvular.heart.disease)+
      theme(legend.title=element_blank())+ 
      theme(legend.position="bottom")
    
    p5 <- ggplotly(p5)
    
    p6 <- ggplot(my_modeling_data(),
                 aes(Lung.rales,
                     group = Valvular.heart.disease)) +
      geom_bar(aes(y = ..prop..,
                   fill = factor(..x..)),
               stat="count") +
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage frequencies") +
      facet_grid(~Valvular.heart.disease)+
      theme(legend.title=element_blank())+ 
      theme(legend.position="bottom")
    
    p6 <- ggplotly(p6)
    
    p7 <- ggplot(my_modeling_data(),
                 aes(Dyspnea,
                     group = Valvular.heart.disease)) +
      geom_bar(aes(y = ..prop..,
                   fill = factor(..x..)),
               stat="count") +
      scale_y_continuous(labels=scales::percent) +
      ylab("Percentage frequencies") +
      facet_grid(~Valvular.heart.disease)+
      theme(legend.title=element_blank())+ 
      theme(legend.position="bottom")
    
    p7 <- ggplotly(p7)
    
    p.two <- subplot(p4, 
                     p5,
                     p6,
                     p7,
                     nrows = 1,
                     margin = c(0.05, 0.05, 0.05,0.05),
                     titleX = T,
                     titleY = T,
                     shareY = F)
  })
  
  output$o.para_coord <- renderPlotly({
    
    p_pc <- ggparcoord(my_modeling_data(), 
                       columns = c(1:8), 
                       groupColumn = 9,
                       scale = "uniminmax",
                       missing = "exclude") +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size=12))
    
    p_pc
  })
  
  #Prediction on test data
  
  output$o.prediction_on_test <- renderPrint({
    
    h2o.no_progress()
    
    final.df <- as.h2o(my_modeling_data())
    
    split_h2o <- h2o.splitFrame(final.df,
                                c(0.7,
                                  0.1),
                                seed = 1234 )
    
    train_h2o <- h2o.assign(split_h2o[[1]],
                            "train" )
    
    valid_h2o <- h2o.assign(split_h2o[[2]],
                            "valid" )
    
    test_h2o  <- h2o.assign(split_h2o[[3]],
                            "test" )
    
    target <- "Valvular.heart.disease"
    
    predictors <- setdiff(names(train_h2o),
                          target)
    
    predictors
    
    fetched.model <- h2o.loadModel(saved.model)
    
    # h2o.varimp_plot(automl_leader)
    
    
    prediction_on_test <- h2o.predict(object = fetched.model,
                                      newdata = test_h2o)
    
    model_performance_on_test <- h2o.performance(automl_leader,
                                                 test_h2o)
    
    model_performance_on_test
    
    
  })
  
  #Globally significant variables
  output$o.global_plots <- renderPlot({
    
    
    fetched.model <- h2o.loadModel(saved.model)
    
    p8 <- h2o.varimp_plot(fetched.model)
    
    p8  
    
  })
  
  #Final prediction on new user input data
  
  user.inputs <- reactive({
    
    
    
    user.inputs <- data.frame(as.factor(input$Family.history),
                              as.integer(input$Pulse.rate),
                              as.factor(input$Lung.rales),
                              as.factor(input$Systolic.Murmur),
                              as.factor(input$Diastolic.Murmur),
                              as.factor(input$Dyspnea),
                              as.factor(input$Function.Class),
                              as.integer(input$EF.TTE))
    
    predictors <- c("Family.history",
                    "Pulse.rate",
                    "Lung.rales", 
                    "Systolic.Murmur",
                    "Diastolic.Murmur",
                    "Dyspnea",
                    "Function.Class",
                    "EF.TTE")
    
    user.inputs <- as.h2o(user.inputs)
    
    names(user.inputs)[1] = c("Family.history")
    names(user.inputs)[2] = c("Pulse.rate")
    names(user.inputs)[3] = c("Lung.rales")
    names(user.inputs)[4] = c("Systolic.Murmur")
    names(user.inputs)[5] = c("Diastolic.Murmur")
    names(user.inputs)[6] = c("Dyspnea")
    names(user.inputs)[7] = c("Function.Class")
    names(user.inputs)[8] = c("EF.TTE")
    
    user.inputs
    
    
    
  })
  
  output$jib1 <- renderPrint({
    
    str(user.inputs()) 
  })
  
  user.input.predictions <- reactive({
    
    fetched.model <- h2o.loadModel(saved.model)
    
    predictions <- h2o.predict(object = fetched.model,
                               newdata = user.inputs())
    
    predictions
    
  })
  
  output$o.final.pred <- renderPrint({
    
    user.input.predictions()
  })
  
  #Local interpretation of the new data
  
  output$o.local.plot <- renderPlot({
    
    h2o.no_progress()
    
    fetched.model <- h2o.loadModel(saved.model)
    
    final.df <- as.h2o(my_modeling_data())
    
    split_h2o <- h2o.splitFrame(final.df,
                                c(0.8,
                                  0.1),
                                seed = 1234 )
    
    train_h2o <- h2o.assign(split_h2o[[1]],
                            "train" )
    
    test_h2o  <- h2o.assign(split_h2o[[3]],
                            "test" )
    
    train_org <- as.data.frame(train_h2o)
    train_org$Family.history <- as.factor(train_org$Family.history)
    train_org$Function.Class <- as.factor(train_org$Function.Class)
    
    
    # str(train_org)
    
    test_df <- as.data.frame(test_h2o)
    test_df$Family.history <- as.factor(test_df$Family.history)
    test_df$Function.Class <- as.factor(test_df$Function.Class)
    
    # str(test_df)
    
    test_sample <- as.data.frame(user.inputs())
    # str(test_sample)
    
    explain1 <- lime(train_org,
                     fetched.model,
                     bin_continuous = FALSE,
                     n_bins = 5,
                     n_permutations = 1000)
    
    # explain1
    # 
    explanation1 <- explain(test_sample,
                            explain1,
                            n_permutations = 500,
                            feature_select = "auto",
                            n_labels = 1,
                            n_features = 8)
    
    # explanation1
    # 
    p <- plot_features(explanation1)
    
    p
    
  })
  
  
  
  
  
  
  
  
  
  
})










