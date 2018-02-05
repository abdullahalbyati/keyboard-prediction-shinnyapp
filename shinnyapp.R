
####################################################
#                  APP UI CODE                     #
#                                                  #
####################################################

library(shiny)
library(NLP)
library(tm)
library(data.table)

ui <- fluidPage(
  
  # Application title
  navbarPage("Coursera Word Predection Final Project",
             tabPanel("Home"),
             navbarMenu("About",
                        tabPanel("Description", p("This app uses a ngram backoff model to predict the next word in a sentence."))
                        )),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      textInput("sentence", "Write your words below", value = ""),
      sliderInput("obs", "maximum predictions:",
                  min = 0, max = 30, value = 10
      )
      
    ),
    
    mainPanel(
      h4("Complete Sentence"),
      verbatimTextOutput("text"),
      
      h4("Prediction"),
      verbatimTextOutput("prediction")
    )
     )
      )
# Load enviroment
load("FinalProject1.RData")
############################################################################
#                        APP Server Code                                   #
#                                                                          #
############################################################################
server <- function(input, output) {
  
  ############################################################################
  #                         prediction function                              #
  #                                                                          #
  ############################################################################
  pred_words <- function(sentence, n = 10){
    words <- unlist(strsplit(sentence, split = " " ))
    words <- tail(words, 5)
    word1 <- words[1];word2 <- words[2];word3 <- words[3];word4 <- words[4];word5 <- words[5];
    datasub <- data.table()
    
    if (nrow(datasub)==0 & !is.na(word5)) {
      if(nrow(datasub) == 0) datasub <- subset(ngram6, w1==word1 & w2==word2 & w3==word3 & w4==word4 & w5==word5)
      if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word2 & w2==word3 & w3==word4 & w4==word5)
      if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word3 & w2==word4 & w3==word5)
      if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word4 & w2==word5)
      if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word5)
    }
    
    if (nrow(datasub)==0 & !is.na(word4)) {
      if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word1 & w2==word2 & w3==word3 & w4==word4)
      if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word2 & w2==word3 & w3==word4)
      if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word3 & w2==word4)
      if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word4)
    }
    
    if (nrow(datasub)==0 & !is.na(word3)) {
      if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word1 & w2==word2 & w3==word3)
      if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word2 & w2==word3)
      if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word3)
    }
    
    if (nrow(datasub)==0 & !is.na(word2)) {
      if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word1 & w2==word2)
      if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word2)
    }
    
    if (nrow(datasub)==0 & !is.na(word1)) {
      if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word1)
      if(nrow(datasub) == 0) datasub <- head(ngram1)
    }
    
    if(nrow(datasub) > 0){
      datasub$freq <- datasub$count / sum(datasub$count)
      as.data.frame(head(datasub[order(-freq)], min(n, nrow(datasub))))
    }
    
  }
  
  
  data_prediction <- reactive({
    pred_words(input$sentence, input$obs);
  })
  
  output$prediction <- renderPrint({
    ds <- data_prediction()
    if(nrow(ds)>0) {
      head(subset(ds, freq==max(ds$freq))[,ncol(ds)-1],3)
      cat( 
        paste( head(ds[,ncol(ds)-1]), collapse=', ' )
      )
    }
  })
  output$text <- renderText({
    paste("Sentence: ", input$sentence, ' ')
  });
  
}
# Run the application 
shinyApp(ui = ui, server = server)
