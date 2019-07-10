library(shiny)
library(tm)
library(wordcloud)
library(memoise)

list <- list("20 Songs"="lyrics", "Hybrid Theory"="Hybrid Theory")

getTermMatrix <- memoise(function(lyric) {

  text <- readLines(sprintf("./%s.txt", lyric), encoding="UTF-8")

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "the", "and", "but","mike","bennington","chester","shinoda"))

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})


ui <- fluidPage(
    titlePanel("Word Cloud of Linkin Park's Lyrics"),

    sidebarLayout(
      sidebarPanel(
        
        selectInput("selection", "Choose lyrics:", choices = list),
        actionButton("update","Change"),
        sliderInput("freq",
                    "Choose the Minimum Frequency of Words:",
                    min = 1,  max = 50, value = 15),
        sliderInput("max",
                    "Choose the Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100)
      ),

      mainPanel(
        h3("Word Cloud knows what Linkin Park likes"),
        plotOutput("plot"),
        br(),
        h3("Instruction"),
        p("You could tell the frequency of words in Linkin Park's lyrics from this app.",br(),
          " 20 songs are my favorite songs and hybrid theory is my favorite album. You could choose one of them to analyze and change the result by adjusting two slides.",br(),
          strong("note: after you choose the lyrics, please click the change button"),br(),
          "1. To change the minimum frequency of words to analyze, the user could slide the first bar so to adjust the choice of words, i.e. only words with frequency higher than the selected value could be shown in the word cloud.",br(),
          "2. To change the number of words to be shown in the word cloud, the user could slide the second bar so to set a limitation of number of words on the screen. "),
          "For detailed instruction, visit: ", a("document for word cloud of linkin park's lyrics", href = "https://safeguardli.github.io/SafeguardLi.github.io/document.html")
      )
    )
  )


server <-   function(input, output, session) {
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      input$update
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection)
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
