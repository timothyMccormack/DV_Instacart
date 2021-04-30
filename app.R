library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)
library(ggvis)
library(dplyr)
library(readr)


aisles <- read.csv("aisles.csv")
dep <- read.csv("departments.csv")
order_p <- read.csv("order_products__prior.csv")
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")

productPopularity <- productOrders1 %>% group_by(product_name) %>% summarise(avgPlacement = mean(add_to_cart_order), avgReOrder = mean(reordered), numOrders = n() )
pnames <- head(productPopularity %>% arrange(-numOrders),350)
keep_names <- pnames$product_name

new <- merge(x=productPopularity,y=products,by="product_name",all=T)
new <- merge(x=new,y=dep,by="department_id",all=T)


new2 <- merge(x=productPopularity,y=products,by="product_name",all=T)
new2 <- select(new2, -1,-2,-3,-5,-6)
new2 <- merge(x=dep,y=new2,by='department_id',all=T)

new2<- new2 %>% 
    group_by(department, department_id) %>% 
    summarise(Total = sum(numOrders, na.rm = TRUE))


ui <- fluidPage(
        mainPanel(
            plotlyOutput("plot0"),plotlyOutput("plot1"), plotlyOutput("plot2"), plotlyOutput("plot3")
        )
)


server <- function(input, output) {
    output$plot0 <- renderPlotly({
        plot_ly(data=new2, labels = ~department, values = ~Total, type = 'pie')
    })
    output$plot1 <- renderPlotly({
        plot_ly(data=new, x=~department, y=~avgReOrder, type = "bar")
    })
    output$plot2 <- renderPlotly({
        plot_ly(data=orders, x = ~order_hour_of_day, type = "histogram")
    })
    output$plot3 <- renderPlotly({
        plot_ly(data=orders, x=~days_since_prior_order, type = "histogram")
    })
}


shinyApp(ui = ui, server = server)
