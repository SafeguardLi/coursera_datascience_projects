library(shiny)
library(tm)
library(wordcloud)
library(memoise)

getTermMatrix <- memoise(function() {

  text <- readLines("./lyrics.txt", encoding="UTF-8")

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "the", "and", "but"))

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})


ui <- fluidPage(
    # Application title
    titlePanel("Word cloud tells you what words Linkin Park likes:)"),

    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(

        sliderInput("freq",
                    "Choose the Minimum Frequency of Words:",
                    min = 1,  max = 50, value = 15),
        sliderInput("max",
                    "Choose the Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),

      # Show Word Cloud
      mainPanel(
        plotOutput("plot")
      )
    )
  )


server <-   function(input, output, session) {
    # Define a reactive expression for the document term matrix
    terms <- reactive({

      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix()
        })
      })
    })

    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)

    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
    })
  }

shinyApp(ui = ui, server = server)
