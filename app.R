library(leaflet)
data <- read.csv("C:/Users/Bural/Desktop/webMiningPROJECT/final_marmaris.csv")
data$Price <- gsub("[,]",".",data$Price)
data$Price <- gsub("[TL]","",data$Price)
data$Price <- as.numeric(data$Price)
data = data[,2:9]
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    
    uiOutput("price_slider"),
    uiOutput("choose_address"),
    uiOutput("choose_district"),
    uiOutput("choose_restaurant"),
    uiOutput("calorie_slider"),
    uiOutput("checkbox"),
    br(),
    leafletOutput("mymap")
  ),
  mainPanel(
    tableOutput("data_table")
    
  )
)

server<-function(input, output) {
  output$price_slider <- renderUI({
    sliderInput("price", "price", 0, max(data$Price), c(0,max(data$Price)), pre = "TL")
  })
  output$choose_address <- renderUI({
    selectInput("address", "Address Name", choices = unique(data$Address),selected = data$Address[1])
  })
  output$choose_district <- renderUI({
    selectInput("district", "Choose district", 
                choices  = unique(data$District),selected = data$District[1])
  })
  output$calorie_slider <- renderUI({
    sliderInput("calorie", "Calorie Filter", 0, max(data$calories), c(0,max(data$calories)), pre = "Kcal")
  })
  
  output$checkbox <- renderUI({
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = unique(subset(data,data$District == input$district)$Restaurant.Name),selected = subset(data,data$District == input$district)$Restaurant.Name)
  })
  
  
  output$data_table <- renderTable({
    
    points <- eventReactive(input$recalc, {
      cbind(c(28.264753,28.274515,28.255545,28.255618,28.256212,28.243789,28.270931,28.271968,28.254370),
            c(36.853729,36.853029,36.847336,36.847340,36.845710,36.855651,36.854794,36.855334,36.845405))
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points())
    })
    if (is.null(input$columns)){
      return()
      }
    filtered <-
      data %>% filter(data$Price >= input$price[1]&
               data$Price <= input$price[2]&data$Address == input$address[1]&data$District == input$district[1]
             &data$calories >= input$calorie[1]&data$calories<=input$calorie[2]&data$Restaurant.Name == input$columns
      )
    #filtered <- filtered[order(-filtered$calories),] 
    filtered
  })
}

shinyApp(ui, server)