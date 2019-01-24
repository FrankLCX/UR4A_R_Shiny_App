setwd("~/Desktop/UR4A")
library(shiny)
library(wordcloud)
library(data.table)
library(dplyr)
library(tidytext)
load('forshiny.Rda')
load('lassofitng2.Rda')
load('trainng2.Rda')
source('predictor.R')
d$brand_name<-ifelse(is.na(d$brand_name),'Missing', d$brand_name)
d$FirstCategory<-ifelse(is.na(d$FirstCategory),'Missing', d$FirstCategory)
d$SecondCategory<-ifelse(is.na(d$SecondCategory),'Missing', d$SecondCategory)
d$ThirdCategory<-ifelse(is.na(d$ThirdCategory),'Missing', d$ThirdCategory)
d$item_description<-ifelse(is.na(d$item_description),'Missing', d$item_description)

# User Interface
ui <- fluidPage(
        
        h1("Online pricing suggestion engine"),
        p(style = "font-family:Calibri",
          "Online Shopping Product Price Prediction"),
        tags$br(),
        tags$br(),
        
        fluidRow(
                column(4, textInput(inputId = "Item_name", 
                                    label = "Please Put Your Item name Here",
                                    value = "Item Name")),
                column(4,selectInput(inputId = 'Brand_name',
                                     label = 'Input your brand here:',
                                     choices = sort(unique(d$brand)),
                                     selected = NULL)),
                column(4,selectInput(inputId = 'Shipping',
                                     label = 'Would you like to provide fee shipping for your product?',
                                     choices = c('Yes!','No.'),
                                     selected = NULL))),
        
        fluidRow(
                column(4,selectInput(inputId = 'First_category',
                                     label = 'Please select your first category',
                                     choices = sort(unique(d$FirstCategory)),
                                     selected = NULL)),
                column(4,selectInput(inputId = 'Second_category',
                                     label = 'Please select your second category',
                                     choices = sort(unique(d$SecondCategory)),
                                     selected = NULL)),
                column(4,selectInput(inputId = 'Third_category',
                                     label = 'Please select your third category',
                                     choices = sort(unique(d$ThirdCategory)),
                                     selected = NULL))),
        selectInput(inputId = 'Cond',
                    label  = 'Please select your item condition (1~5: New~Old)',
                    choices = c(1,2,3,4,5),
                    selected=NULL),
        textInput(inputId = "item_description", 
                  label = "Please Put Your Item Description Here",
                  value = "Item Description",
                  width='80%'),
        numericInput(inputId = "I_price",
                     label = "How much do you wish to sell your product?",
                     min = 0, max = 3000, value=0),
        htmlOutput('text'),
        textOutput('summary'),
        plotOutput('pricedist'),
        plotOutput('cloud')
        
)

# Server
server <- function(input, output,session) {
        observe({
                updateSelectInput(session=session, 
                                  inputId = 'Second_category',
                                  choices = sort(unique(subset(d,FirstCategory==input$First_category)$SecondCategory)))
        })
        observe({
                updateSelectInput(session=session, 
                                  inputId = 'Third_category',
                                  choices = sort(unique(subset(d,FirstCategory==input$First_category
                                                               &SecondCategory==input$Second_category)$ThirdCategory)))
        })  
        observe({
                updateSelectInput(session=session, 
                                  inputId = 'Brand_name',
                                  choices = sort(unique(subset(d,FirstCategory==input$First_category
                                                               &SecondCategory==input$Second_category
                                                               &ThirdCategory==input$Third_category)$brand_name)))
        })  
        
        ############################################################################################################
        ##Price Prediction
        item1<-reactive(data.frame(Item.name=input$Item_name,
                                   Shipping.cost=ifelse(input$Shipping=='Yes!',1,0),
                                   Brand=input$Brand_name,
                                   First.category=input$First_category,
                                   Second.category=input$Second_category,
                                   Third.category=input$Third_category,
                                   Item.description=input$item_description,
                                   ItemCondition=input$Cond))
        output$text<-renderUI({
                str0 <- paste("____________________________________________________________________________________________________________________________________________________ ")
                str1 <- paste('Based on the information provided, the recommended pricing level of your product is: ')
                HTML(paste(str0,str1,sep='<br/>'))
        })
        output$summary<- renderText(predictor(item1()))
        
        ############################################################################################################
        ##Graphs
        df<-reactive(d%>%filter(FirstCategory==input$First_category
                                &SecondCategory==input$Second_category
                                &ThirdCategory==input$Third_category))
        draw<-reactive(df()%>%unnest_tokens(output=word,input=item_description)%>%
                               count(word,sort=T)%>%
                               anti_join(stop_words)%>%
                               anti_join(data_frame(word=c('0','1','2','3','4','5','6','7','8','9',type=rep('numbers',5)))))
        
        output$pricedist<-renderPlot({
                plot(density((df()$price)),main = 'Distribution of the all items in this category')
                lines(x=rep(input$I_price,100),y=c(0,1,runif(98,0,1)),col='red')
        })
        output$cloud <- renderPlot({
                title<-paste0('The wordcloud for ',
                              input$First_category,'/',
                              input$second_category, '/',
                              input$Third_category)
                layout(matrix(c(1, 2), nrow=2), heights=c(1, 20),widths = c(10,20))
                par(mar=rep(0, 4))
                plot.new()
                text(x=0.5, y=0.5, title)
                #tags$br()
                wordcloud(words = draw()$word, freq = draw()$n, 
                          random.order = F,scale=c(6,0.5), max.words = 200,
                          colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"),
                          rot.per = 0.2)
        })
}

# Run the app
shinyApp(ui = ui, server = server)   
#Wrap with runApp() if calling from a sourced script or inside a function

