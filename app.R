library(shiny)
library(tm)
library(wordcloud)
library(memoise)

## global

## Books downloaded from Project Gutenberg

#Pride & Prejudice by Jane Austen (1342)
#Frankenstein by Mary Shelley (84)
#The Importance of Being Earnest by Oscar Wilde (844)
#A Doll's House by Henrik Ibsen (2542)
#A Tale of Two Cities by Charles Dickens (98)

# The list of valid books
books <<- list("Pride & Prejudice" = "austen",
               "Frankenstein" = "shelley",
               "The Importance of Being Earnest" = "wilde",
               "A Doll's House" = "ibsen",
               "A Tale of Two Cities" = "dickens"
               )

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt.gz", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "the", "and", "or", "if", "of", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


## ui
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud: Top 5 Books on Project Gutenberg"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a book:",
                  choices = books),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 100, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 50)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

## server
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
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
    wordcloud_rep(names(v), v, scale=c(6,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Set1"))
  })
}
shinyApp(ui = ui, server = server)
