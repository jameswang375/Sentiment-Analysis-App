# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Details: 
# Author: 
# Date:


# ===============================================
# R packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(DT)        # to work with HTML table widgets
library(hash)
library(gridExtra)

# =======================================================
# Sentiment Lexicons
# Uncomment the lexicon(s) that you would like to use if
# you choose to perform some kind of sentiment analysis
# =======================================================
bing = read_csv("bing.csv", col_types = "cc")
nrc = read_csv("nrc.csv", col_types = "cc")
loughran = read_csv("loughran.csv", col_types = "cc")
afinn = read_csv("afinn.csv", col_types = "cd")

lexicons = hash()
lexicons[["bing"]] = bing
lexicons[["nrc"]] = nrc
lexicons[["loughran"]] = loughran
lexicons[["afinn"]] = afinn

# =======================================================
# Import data
# Uncomment the commands below in order to import the data
# =======================================================
dat2 = read_csv(
  file = "lyrics.csv", 
  col_types = cols(
    artist = col_character(),
    album = col_character(),
    year = col_character(),
    song = col_character(),
   lyrics = col_character()
 ))

# for demo purposes in this "template", we use data starwars
# (but you will have to replace this with the lyrics data)
#dat <- dplyr::starwars



# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Sentiment Analysis Application for Song Lyrics"),
  hr(),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # replace with your widgets
    column(3,
           h3(em("Artist")),
           radioButtons(inputId = "artist", 
                        label = "Select an Artist", 
                        choices = unique(dat2$artist),
                       selected = "Beyonce")
    ), # closes column 1
    
    # replace with your widgets
    column(3,
           h3(em("Songs")),
           selectInput(inputId = "song", 
                        label = "Select a Song",
                       choices = ""
                        )
    ), # closes column 2
    
    # replace with your widgets
    # column(3,
    #        p(em("Inputs (column 3)")),
    #        checkboxInput("bing", label = "Bing", value = TRUE),
    #        checkboxInput("loughran", label = "Loughran", value = FALSE),
    #        checkboxInput("afinn", label = "Afinn", value = FALSE),
    #        checkboxInput("nrc", label = "Nrc", value = FALSE)
    # ),
     # closes column 3
    
    column(3,
           h3(em("Lexicons")),
           sliderInput(inputId = "lexicon", label = "(1 = Bing, 2 = Nrc 3 = Loughran 4 = Afinn)", 
                       min = 1, 
                       max = 4, value = 1)
    ),
    
    # replace with your widgets
    column(3,
           h3(em("Song to Compare")),
           textInput(inputId = "multiple", 
                     label = "Type in a Song to Compare (Must be from Same Artist)", 
                     value = "")
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Main Panel with outputs: plot and table
  # Feel free to customize the following output elements
  # -------------------------------------------------------
  mainPanel(
    h3("Plot"),
    uiOutput("plot"),
    #plotOutput(outputId = "plot"),
    hr(),
    h3("Table"),
    dataTableOutput(outputId = "table"),
  ) # closes mainPanel
  
) # closes ui



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive object(s)
  # ------------------------------------------------------------
  # the following code is for demo purposes only;
  # replace the code below with your code!!!
  
  artist_react = reactive({
    dat2 |> 
      filter(artist == !!input$artist)
  })
  
  observe({
    updateSelectInput(inputId = "song", choices = filter(dat2, artist == !!input$artist)$song,
                      selected = filter(dat2, artist == !!input$artist)$song[1])
  })
  
   observeEvent(input$artist, 
     updateTextInput(inputId = "multiple",
                       value = "")
   )
  
  
  result_bing = reactive({ 
    filter(artist_react(), song == !!input$song) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(bing, by = "word") |>
      count(index = linenumber %/% 25, sentiment) |>
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
      mutate(sentiment = tryCatch({positive - negative}, error = function(e) {
        0
      }
      )
      )
  })
  
  result_bing_multiple = reactive({if (input$multiple != "") { 
    filter(artist_react(), song == input$multiple) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(bing, by = "word") |>
      count(index = linenumber %/% 25, sentiment) |>
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
      mutate(sentiment = tryCatch({positive - negative}, error = function(e) {
        0
      }
      )
      )
  }
  })
  
  
  
  result_nrc = reactive({ 
    filter(artist_react(), song == input$song) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(nrc, by = "word") |>
      group_by(sentiment) |>
      count() |>
      mutate(song = input$song)
  })
  
  result_nrc_multiple = reactive({if (input$multiple != "") {
    filter(artist_react(), song == input$multiple) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(nrc, by = "word") |>
      group_by(sentiment) |>
      count() |>
      mutate(song = input$multiple)
    
  }
  })
  
  result_loughran = reactive({ 
    filter(artist_react(), song == input$song) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(loughran, by = "word") |>
      group_by(sentiment) |>
      count() |>
      mutate(song = input$song)
  })
  
  result_loughran_multiple = reactive({if (input$multiple != "") {
    filter(artist_react(), song == input$multiple) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(loughran, by = "word") |>
      group_by(sentiment) |>
      count() |>
      mutate(song = input$multiple)
  }
  })
  
  result_afinn = reactive({ 
    sent_afinn = filter(artist_react(), song == !!input$song) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(afinn, by = "word") |>
      mutate(section = linenumber %/% 25) |>
       group_by(section) |>
       summarise(sums = sum(value))
  })
  
  result_afinn_multiple = reactive({if (input$multiple != "") {
  
    sent_afinn = filter(artist_react(), song == !!input$multiple) |>
      unnest_tokens(output = word, input = lyrics) |>
      mutate(linenumber = row_number()) |>
      inner_join(afinn, by = "word") |>
      mutate(section = linenumber %/% 25) |>
      group_by(section) |>
      summarise(sums = sum(value))
  }
  })
  
  # ------------------------------------------------------------
  # Plot
  # Adapt code to display appropriate graphic(s)
  # ------------------------------------------------------------
  output$plot = renderUI({
    if (input$multiple == "") {
    plotOutput("plot1")
    }
    else {
      fluidRow(
        splitLayout(cellWidths = c("75%", "75%"), plotOutput("plot1"), plotOutput("plot2"))
      )
      
    }
  })
  
  output$plot1 <- renderPlot({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
    
    if (input$lexicon == 1) {
    ggplot(data = result_bing(),
           aes(x = index, y = sentiment, fill = factor(sign(sentiment)))) +
      geom_col(show.legend = FALSE) +
      theme_bw() +
      labs(title = str_glue("Sentiment Throughout Song: {input$song}"),
           subtitle = "Empty Plot May Indicate Lyrics are not in English",
           x = "Time Elapsed in Song (Split into Sections)",
           y = "Sentiment Scores")
      
    }
    else if (input$lexicon == 2) {
      ggplot(data = result_nrc(),
             aes(x = sentiment, y = n, fill = factor(sentiment))) +
        geom_col() +
        theme_bw() +
        labs(title = str_glue("Frequency of Emotions in Song: {input$song}"),
             fill = "Emotions",
             y = "Count",
             x = "Emotions") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    else if (input$lexicon == 3) {
      ggplot(data = result_loughran(),
             aes(x = sentiment, y = n, fill = factor(sentiment))) +
        geom_col() +
        theme_bw() +
        labs(title = "Adjectives that Describe Sentiment of Lyrics"
        , fill = "Adjectives",
        y = "Count",
        x = "Adjectives",
        subtitle = str_glue("Song: {input$song}"))
    }
    
    else if (input$lexicon == 4) {
      ggplot(data = result_afinn(),
             aes(x = section, y = sums)) +
        geom_col(show.legend = FALSE) +
        theme_bw() +
        labs(title = str_glue("Sentiment Throughout Song: {input$song}"),
             subtitle = "Empty Plot May Indicate Lyrics are not in English",
             x = "Time Elapsed in Song (Split into Sections)",
             y = "Sentiment Scores")
    }
  })
  

  output$plot2 <- renderPlot({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!

    if (input$lexicon == 1) {
      if (input$multiple != ""){
      ggplot(data = result_bing_multiple(),
             aes(x = index, y = sentiment, fill = factor(sign(sentiment)))) +
        geom_col(show.legend = FALSE) +
        theme_bw() +
        labs(title = str_glue("Sentiment Throughout Song: {input$multiple}"),
             subtitle = "Empty Plot May Indicate Lyrics are not in English",
             x = "Time Elapsed in Song (Split into Sections)",
             y = "Sentiment Scores")
      }

    }
    else if (input$lexicon == 2) {
      if (input$multiple != "") {
      ggplot(data = result_nrc_multiple(),
             aes(x = sentiment, y = n, fill = factor(sentiment))) +
        geom_col() +
        theme_bw() +
        labs(title = str_glue("Frequency of Emotions in Song: {input$multiple}"),
             fill = "Emotions",
             y = "Count",
             x = "Emotions") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      }
    }

    else if (input$lexicon == 3) {
      if (input$multiple != "") {
      ggplot(data = result_loughran_multiple(),
             aes(x = sentiment, y = n, fill = factor(sentiment))) +
        geom_col() +
        theme_bw() +
        labs(title = "Adjectives that Describe Sentiment of Lyrics"
             , fill = "Adjectives",
             y = "Count",
             x = "Adjectives",
             subtitle = str_glue("Song: {input$multiple}"))
      }
    }

    else if (input$lexicon == 4) {
      if (input$multiple != "") {
      ggplot(data = result_afinn_multiple(),
             aes(x = section, y = sums)) +
        geom_col(show.legend = FALSE) +
        theme_bw() +
        labs(title = str_glue("Sentiment Throughout Song: {input$multiple}"),
             subtitle = "Empty Plot May Indicate Lyrics are not in English",
             x = "Time Elapsed in Song (Split into Sections)",
             y = "Sentiment Scores")
      }
    }
  })
  
  
  # ------------------------------------------------------------
  # Table
  # Adapt code to display appropriate table
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # the following code is for demo purposes only;
    # replace the code below with your code!!!
     
     if (input$lexicon == 1) {
     if (input$multiple != "") {
       tbl = filter(artist_react(), song == !!input$song) |>
         mutate("overall sentiment" = sum(result_bing()$sentiment)) |>
         select(-lyrics)
       
       tbl2 = filter(artist_react(), song == input$multiple) |>
         mutate("overall sentiment" = sum(result_bing_multiple()$sentiment)) |>
         select(-lyrics)
       
       binded_tbl = rbind(tbl, tbl2)
       binded_tbl
     }
       else {
         filter(artist_react(), song == !!input$song) |>
           mutate("overall sentiment" = sum(result_bing()$sentiment)) |>
           select(-lyrics)
       }
     }
    
    else if(input$lexicon == 2) {
      if (input$multiple != "") {
        
        binded_tbl = rbind(result_nrc(), result_nrc_multiple())
        binded_tbl |>
          rename(count = n)
        
      }
      else {
        result_nrc() |>
          rename(count = n)
      }
    }
    
    else if (input$lexicon == 3) {
      if (input$multiple != "") {
        
        binded_tbl = rbind(result_loughran(), result_loughran_multiple())
        binded_tbl |>
          rename(count = n)
        
      }
      else {
        result_loughran() |>
          rename(count = n)
      }
    }
    
    else if (input$lexicon == 4) {
      if (input$multiple != "") {
        tbl = filter(artist_react(), song == !!input$song) |>
          mutate("overall sentiment" = sum(result_afinn()$sums)) |>
          select(-lyrics)
        
        tbl2 = filter(artist_react(), song == input$multiple) |>
          mutate("overall sentiment" = sum(result_afinn_multiple()$sums)) |>
          select(-lyrics)
        
        binded_tbl = rbind(tbl, tbl2)
        binded_tbl
      }
      else {
        filter(artist_react(), song == !!input$song) |>
          mutate("overall sentiment" = sum(result_afinn()$sums)) |>
          select(-lyrics)
      }
    }

  })

  
} # closes server



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

