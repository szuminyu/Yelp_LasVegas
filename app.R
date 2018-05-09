#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("setup.R")

ui <- fluidPage(
  tags$head(includeCSS("styles.css")),
            
  navbarPage("Yelp Las Vegas", id = "yelp",
             
             ##story
             tabPanel("Story",

                      mainPanel(id = "Mainstory",
                                h1("Yelp in Las Vegas"),
                                br(),
                                p("We have all experienced that moment where we are struggling to decide what to eat, and the app that most of us often turn to is Yelp. This is especially the case when exploring a new city, such as Las Vegas, which is a popular vacation spot due to its multitude of casinos and bars. Our app will help you get a sense of available restaurants in Las Vegas, providing information on the type of restaurants, the price range, yelp satisfaction levels, location, etc. 
                                  In Las Vegas, the most common type of  restaurants is American. The next common is Asian, and the least is Vegan food.
                                  "),
                                br(),
                                plotOutput("g1", width = "100%"),
                                br(),
                                p('Regarding price range, Fast Food, Latino, American, and Asian restaurants are relatively cheaper, and Vegan, Italian, and European are more usually more expensive. 
                                  It is interesting to us that Latino food is usually cheaper than Italian, thus we would like to know if people’s reviews on the two types of food differ. Another interesting thing we want to look closer to is about American food. As we can see from the above graph, there are almost equal number of restaurants under the cheapest price range as more expensive ones. Will people say different things for the different priced American restaurants? 
                                    But before we dive into those questions, let’s look at the location of the restaurants and you can pick the restaurant on the map according to your own standard!
                                    '))),
             
             ##interactive map
             tabPanel("Map",
                      
                      fixedPanel("Visualization",
                                 fixedPanel(
                                   id = "fullscreen", 
                                   top = 43, left = 0, width = "100%", height = "95%", 
                                   leafletOutput("yelpmap", width = "100%", height = "100%")),
                                 fixedPanel(id = "selections", draggable = TRUE, 
                                            right = 30, width = 400, height = "auto",
                                            id = "input_panel", style = "background-color:rgba(0, 0, 0, 0.3); padding:10px;",
                                            h4("Pick Your Restaurant!"),
                                            tabPanel("Hungry",
                                                     p("Our app features an interactive app that allows you to pick what type of restaurant you are interested in. 
                                                        You can also pick restaurants near casinos.
                                                        There is a price range that allows the user to decide their price range of what they are willing to pay for food. 
                                                        If you hit the jackpot at the casino, you can adjust your price range accordingly. 
                                                        In addition, the stars section allows for picky eaters to choose a higher satisfaction level whereas casual diners could accept a lower number of stars. 
	                                                      An notable pattern over here is that the most expensive restaurants are in “Paradise.”"),
                                                     selectInput("incat","Categories", cat),
                                                     selectInput("inprice","Price Range", price),
                                                     selectInput("instar","Stars (Satisfaction)",star)
                                                     )))
             ),
             
             ##insights
             tabPanel("Insights",
                      
                      
                      mainPanel(id = "Insights",
                                h4("Overview"),
                                p("The wordcloud displays the most common words that appear in reviews, allowing for a general overview of what most YELP users say in comments regarding restaurants in 2017. This shows us that service, wait time, and the dining experience are all very important attributes to YELP users. "),
                                br(),
                                plotOutput("wordcloud"),
                                br(),
                                br(),
                                p("Regarding price range, Fast Food, Latino, American, and Asian restaurants are relatively cheaper, and Vegan, Italian, and European are usually more expensive."),
                                plotOutput("g3"),
                                br(),
                                br(),
                                h4('The most common words in Latino and Italian food reviews'),
                                p("It is interesting to us that Latino food is usually cheaper than Italian, thus we would like to know if people’s reviews on the two types of food differ."),
                                plotOutput("wordcloud1"),
                                br(),
                                br(),
                                h4("Different words used by reviews for Italian (orange) and Latino (blue) food."),
                                plotOutput("wordcloud2"),
                                br(),
                                br(),
                                h4("Sentiment Analysis using AFINN Dictionary."),
                                plotOutput("g.sentiment"),
                                br(),
                                br(),
                                h4("Tone Analysis using Hu & Liu Dictionary"),
                                p('Lastly, we did a tone analysis on the reviews for the two types of food. The graph below shows no difference in the tone used to describe experiences eating at Latino and Italian restaurants.'),
                                plotOutput("tone.analysis"),
                                br()
                      )
             ),
	     ##team members
             tabPanel("Team",
                      mainPanel(  h2("Final Project, DS-GA 3001-015 Data Visualization"),
                                 br(),
                                 h4("This project is coded in R."),
                                 br(),
                                 br(),
                                 p("Contacts"),
                                 br(),
                                 p("Szu-Min Yu: smy320@nyu.edu"),
                                 p("Haowen Zheng: haowen.zheng@nyu.edu"),
                                 p("Jennifer Zhang: yz4677@nyu.edu")
                      )
                      
                      )
             
             
  )
  
)

server <- function(input, output, session) {
  pal = colorFactor(c('#e31a1c','#f768a1','#f16913',"#fee08b",'#41ab5d','#377eb8','#253494','#807dba','#7f2704',
                      '#525252'), domain = restaurants$categories) 
  popup <- paste0("<strong>Business: </strong>",
                  restaurants1$name, "<br>",
                  "<strong>Categories: </strong>",
                  restaurants1$categories, "</br>",
                  "<strong>Price Range: </strong>",
                  restaurants1$attributes.RestaurantsPriceRange2, "<br>",
                  "<strong>Stars: </strong>",
                  restaurants1$stars)
  pop1 <- paste0("<strong>Business: </strong>",
                 casino$name, "<br>",
                 "<strong>Categories: </strong>",
                 casino$categories, "</br>",
                 "<strong>Price Range: </strong>",
                 casino$attributes.RestaurantsPriceRange2, "<br>",
                 "<strong>Stars: </strong>",
                 casino$stars)
  
  output$yelpmap <- renderLeaflet({
    map <-leaflet(restaurants) %>%
      addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      setView(lat = 36.164277, lng = -115.1111, zoom = 12)%>%
      addCircles(~longitude, ~latitude, color = ~pal(restaurants1$categories), weight = 3, opacity = 0.7,popup = popup, group = "Restaurants")%>%
      addCircles(data = casino, ~longitude, ~latitude, color = '#41ab5d', weight = 3, opacity = 0.7, popup = pop1, group = "Casinos")%>%
      leaflet::addLegend(pal = pal, values = ~restaurants$categories, title = "Type of Restaurant", opacity = 0.7, position="bottomleft")%>%
      addLayersControl(overlayGroups = c("Restaurants","Casinos"),
                       options = layersControlOptions(collapsed = FALSE), position = "bottomright")
    map
  })
  
  filteredData <- reactive({restaurants1})
  
  observeEvent({ 
    input$incat
    input$inprice
    input$instar
  },
  {
    if(input$incat == "All" & input$inprice == "All" & input$instar == "All"){
      filteredData <- reactive({
        restaurants })
    } else if(input$inprice == "All" & input$instar == "All" ) {
      filteredData <- reactive({
        restaurants %>%
          filter(categories == input$incat)})
    } else if(input$incat == "All" & input$instar == "All") {
      filteredData <- reactive({
        restaurants %>%
          filter(attributes.RestaurantsPriceRange2 == input$inprice)})
    } else if(input$incat == "All" & input$inprice == "All"){
      filteredData <- reactive({
        restaurants %>%
          filter(stars1 == input$instar)
      })
    } else if(input$incat == "All"){
      filteredData <- reactive({
        restaurants %>%
          filter(attributes.RestaurantsPriceRange2 == input$inprice,
                 stars1 == input$instar)
      })
    } else if(input$inprice == "All"){
      filteredData <- reactive({
        restaurants %>%
          filter(categories == input$incat,
                 stars1 == input$instar)
      })
    } else if(input$instar == "All"){
      filteredData <- reactive({
        restaurants %>%
          filter(categories == input$incat,
                 attributes.RestaurantsPriceRange2 == input$inprice)})
    } else {
      filteredData <- reactive({
        restaurants %>%
          filter(categories == input$incat,
                 attributes.RestaurantsPriceRange2 == input$inprice,
                 stars1 == input$instar)})
    } 
    
    popup <- paste0("<strong>Business: </strong>",
                    filteredData()$name, "<br>",
                    "<strong>Categories: </strong>",
                    filteredData()$categories, "</br>",
                    "<strong>Price Range: </strong>",
                    filteredData()$attributes.RestaurantsPriceRange2, "<br>",
                    "<strong>Stars: </strong>",
                    filteredData()$stars)
    
    
    pal = colorFactor(c('#e31a1c','#f768a1','#f16913',"#fee08b",'#41ab5d','#377eb8','#253494','#807dba','#7f2704',
                        '#525252'), domain = restaurants$categories) 
    
    map <- leafletProxy("yelpmap", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(~longitude, ~latitude, color = ~pal(filteredData()$categories), weight = 3, opacity = 0.7, popup = popup, group= "Restaurants")%>%
      leaflet::addLegend(pal = pal, values = ~restaurants$categories, title = "Type of Restaurant", opacity = 0.7, position="bottomleft")%>%
      addCircles(data = casino, ~longitude, ~latitude, color = '#41ab5d', weight = 3, opacity = 0.7, popup = pop1, group = "Casinos")%>%
      addLayersControl(overlayGroups = c("Restaurants","Casinos"),
                       options = layersControlOptions(collapsed = FALSE), position = "bottomright")
    
    updateSelectizeInput(session, "inSelect1",
                         label = "select input label",
                         choice = input$incat)
    updateSelectizeInput(session, "inSelect2",
                         label = "select input label",
                         choice = input$inprice)
    updateSelectizeInput(session, "inSelect3",
                         label = "select input label",
                         choice = input$instar)
  })
  
  
  ##story
  output$g1 <- renderPlot({g1})
  
  
  ##insights
  output$wordcloud <- renderPlot({wordcloud(sample.cleaned.corpus, max.words = 50, scale=c(3,.5), random.order=F, 
                                            colors= c('#9ecae1','#6baed6','#3182bd','#08519c'))})
  output$g3 <- renderPlot({g3})
  output$badgraph <- renderImage({"badgraph.png"})
  output$wordcloud1 <- renderPlot({commonality.cloud(pyr_m, max.words = 60, random.order=FALSE, colors = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"))})
  output$wordcloud2 <- renderPlot({comparison.cloud(pyr_m, colors = c("orange", "blue"), 
                                                    scale=c(0.2,1.5), title.size= 1, 
                                                    max.words = 80)})
  output$g.sentiment <- renderPlot({g.sentiment})
  output$tone.analysis<- renderPlot({tone.analysis})
}

shinyApp(ui = ui, server = server)
