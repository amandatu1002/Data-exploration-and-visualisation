# server.R

shinyServer(function(input, output, session) {
  
  ##### Interactive Map #####
  # reactivate map info
  mapdf <- reactive({
    listcleandf %>%
      filter(neighbourhood_cleansed %in% input$select_area & 
               room_type %in% input$select_room & 
               price >= input$slider_price[1] &
               price <= input$slider_price[2] &
               review_scores_rating >= input$slider_rating[1] &
               review_scores_rating <= input$slider_rating[2] &
               number_of_reviews >= input$slider_review[1] &
               number_of_reviews <= input$slider_review[2])
  })
  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addLegend(position = "bottomleft", pal = groupColors, values = room, opacity = 1, title = "Room Type") %>% 
      setView(lng = 144.96, lat = -37.81, zoom = 12)
  })
  
  # observe an event
  observe({ #require a trigger to call the observe function
    proxy <- leafletProxy("map",data = mapdf()) %>% 
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      # circle
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~groupColors(room_type),
                       group = "CIRCLE",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type:', room_type,'<br/>',
                                      'Price:', price,'<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # cluster
      addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type: ', room_type, '<br/>',
                                      'Price:', price,'<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  ## reactivate count dataframe for map graph1 
  countdf <- reactive({
    mapdf() %>%
      group_by(., room_type) %>%
      summarise(., count_type = n())
  })
  
  #map graph1 
  output$count_room <- renderPlotly({
    plot_ly(data = countdf(), x = ~room_type, y = ~count_type, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo = 'text',
            text = ~count_type) %>%
      layout(xaxis = list(title = "", showticklabels = FALSE),
             yaxis = list(title = "number of reviews"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~count_type, text = ~paste(round(count_type/sum(count_type),2)*100,'%'),
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  ## reactivate price dataframe for map graph2
  pricedf <- reactive({
    mapdf() %>% 
      group_by(., room_type) %>% 
      summarise(., avg_price = round(mean(price),2))
  })
  
  #map graph2 avgprice
  output$avgprice <- renderPlotly({
    plot_ly(data = pricedf(), x = ~room_type, y = ~avg_price, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo = 'text',
            text = ~avg_price) %>% 
      layout(xaxis = list(title = "", showticklabels = FALSE), 
             yaxis = list(title = "price"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~avg_price, text = ~paste('$',avg_price),
                                xanchor = 'center', yanchor = 'bottom', 
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  ##### Listings, Areas and Price Changes #####
  ## reactivate dataframe for listings grapgh
  graph1df <- reactive({
    listcleandf %>%
      select(neighbourhood_cleansed,room_type,price,review_scores_rating) %>% 
      filter(price >= input$tab2_price[1] &
               price <= input$tab2_price[2] &
               review_scores_rating >= input$tab2_rating[1] &
               review_scores_rating <= input$tab2_rating[2]) %>% 
      group_by(neighbourhood_cleansed,room_type) %>% 
      summarise(n=n())
  })
  
  # listings grapgh
  output$graph1 <- renderPlotly({
    t <- list(size = 9)
    plot_ly(data = graph1df(), x = ~n, y = ~room_type, type = "bar", color = ~neighbourhood_cleansed,
            colors = c('#800080','#009DDC','#E03A3C','#62BB47','#FFA500'),
            orientation = 'h', showlegend = TRUE) %>%
      layout(xaxis = list(title = "count"),
             yaxis = list(title = ""), 
             barmode = 'dodge', font = t)
  })
  
  # price change graph (year/ month)
  output$tab_price <- renderPlotly({
    if(input$price_option == 'Year'){
      m <- list(size = 8)
      plot_ly(data = groupedCalendarYear, x = ~year, y = ~avg_pricemon, type = 'scatter', mode ='markers', linetype = I('solid')) %>% 
        layout(xaxis = list(title = "", showticklabels = TRUE),
               yaxis = list(title = "price"), showlegend = FALSE, font=m)} else{
                 
                 plot_ly(data = groupedCalendarMon, x = ~month, y = ~month_avg, type= 'scatter', mode = 'markers+lines', color = "Set1",
                         text = ~paste('Price: $', month_avg)) %>%
                   layout(xaxis = list(title = "month", type = 'category'),
                          yaxis = list(title = "price"))}
  })
  
  ## reactivate dataframe for wordcloud plot1
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "submit" button is pressed...
   #input$Submit
    
    withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(input$text)
    })
    
  })
  
  rwd<-reactive({terms()}) #reactive expression call it as a function using parentheses
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    set.seed(1234)
    wordcloud_rep(words = rwd()$item, freq = rwd()$freq, scale=c(2,0.2),
                  max.words=input$max,
                  random.order=FALSE, rot.per=0.2,
                  colors = c("#37db1a","#e07014","#3783b2","#9e5007"))
  })
  
  
  ## reactivate dataframe for wordcloud plot2
  
  
  output$plot2<- renderPlot({
  set.seed(789)
  wordcloud(words = wordDF$text, 
            freq = wordDF$n,
            min.freq = 10000,scale=c(4,0.5),
            max.words=300, random.order=FALSE,
            colors = c("#e06f69", "#59c6f3","#2a7d82", "#1f5560") )
  })
  
})


