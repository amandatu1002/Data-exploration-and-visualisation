# ui.R

library(shinythemes)

shinyUI(
  navbarPage(title = "Airbnb in Melbourne", 
             id ="nav",
             
             theme = shinytheme("yeti"), 
             
 ##### Overview #####       
    tabPanel("Airbnb Host Tool",

             br(),
             HTML('<h1><center>Airbnb Host Tool</center></h1>
                   <center><img src="airbnb-icon.png", height = auto, weight =auto ></center>
                   <h1><center>Manage Your Airbnb Business Efficiently</center></h1>'),             
             HTML('<center><img src="airbnb_host.png", height = auto, weight =auto >
                   <img src="airbnb_overview.png", height = auto, weight =auto >
                   <img src="airbnb_calendar.png", height = auto, weight =auto ></center>')
             ),

 ##### Map #####     
    tabPanel("Melbourne map",
      div(class="outer",
          tags$head( #customized CSS
            includeCSS("styles.css")),
          
      leafletOutput(outputId = "map", width = "100%", height = "100%"),
                          
      # Panel options: borough, Room Type, Price, Rating, Reviews
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                    top = 80, left = "auto", right = 125, bottom = "auto", width = 350, height = "auto",
                    h2("Airbnb in Melbourne"),
                    checkboxGroupInput(inputId = "select_area", label = h4("Area"), 
                         choices = area, selected = 'Melbourne')),
                    
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                    top = 80, left = 50, right = "auto", bottom = "auto", width = 350, height = "auto",                   
                    h2("Please select the types and ranges"),
                    
                    
                    checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                         choices = room, selected = room),
                    sliderInput(inputId = "slider_price", label = h4("Price"), min = 0, max = 500, step = 50,
                         pre = "$", sep = ",", value = c(60, 300)),
                    sliderInput(inputId = "slider_rating", label = h4("Rating Score"), min = 0, max = 100, step = 10,
                         value = c(60, 100)),
                    sliderInput(inputId = "slider_review", label = h4("Number of Reviews"), min = 10, max = 500, step = 50,
                         value = c(60, 500)),
      h6("Based on 2019 dataset from"),
      h6(a("Inside Airbnb", href = "http://insideairbnb.com/get-the-data.html", target="_blank"))
      ),
      
      
      # Results: count_room, avgprice
      absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                    top = 80, left = 450, right = "auto" , bottom = "auto",
                    width = 400, height = "auto",
                    plotlyOutput(outputId = "count_room",height = 200),
                    plotlyOutput(outputId = "avgprice", height = 200))
      
      )),

 ##### Listings #####              
    tabPanel("Data Analysis",    
             fluidRow(
               column(3,
                      h3("Listings by Room Type and Area"),
                      sliderInput(inputId = "tab2_price", h4("Price/Night"), min = 10, max = 500, value = c(10, 500)),
                      sliderInput(inputId = "tab2_rating", h4("Rating Score"), min = 10, max = 100, value = c(10,100)),
                      
                      br(),
                      hr(),
                      h3("Word Cloud"),
                      sliderInput(inputId ="max", h4("Maximum Number of Words:"), min = 1,  max = 100,  value = 80),
                      textInput(inputId ="text", h4("Type Key Word:"), "happy"),
                      
                      br(),
                      hr(),
                      h3("Price Changes over Time"),
                      selectInput("price_option", label = h4("Select Time Type"), 
                                  choices = list("Year" = "Year","Month" = "Month"), selected = "Month")
                      
                      #submitButton("Submit")
                      

               ),
               
               column(9,
                      h3(""),
                      plotlyOutput(outputId = "graph1", width=1200, height =500),
                      br(),
                      splitLayout(cellWidths = c("30%", "30%", "40%"), plotOutput("plot"), plotOutput("plot2"),plotlyOutput(outputId = "tab_price", width=500, height =180))
               )
                      
               )
             
             ),

 ##### References #####    
    navbarMenu("References",
               tabPanel("Inside Airbnb",
                        h3("Inside Airbnb", a("Link", href="http://insideairbnb.com/get-the-data.html"))),

               tabPanel("Shiny Gallery",
                        h3("Shiny Gallery", a("Link", href="http://shiny.rstudio.com/gallery/")))
               ) 

))
