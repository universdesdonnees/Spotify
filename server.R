server <- function(input, output, session) {
  
  filtered_data <- reactive({
    spotify_data %>% filter(artist_name %in% input$artist_select)
  })
  
  # Calculate number of streams
  output$numStreams <- renderValueBox({
    num_streams <- filtered_data() %>% 
      summarise(n_streams = sum(streams)) %>% 
      pull()
    
    valueBox(value = num_streams, color = "olive",
             subtitle = "Number of streams")
  })
  
  # Calculate number of tracks
  output$numTracks <- renderValueBox({
    num_tracks <- nrow(filtered_data())
    valueBox(value = num_tracks, color = "olive",
             subtitle = "Number of Tracks")
  })
  
  # Calculate number of artists
  output$numArtists <- renderValueBox({
    num_artists <- length(unique(filtered_data()$artist_name))
    valueBox(value = num_artists, color = "olive",
             subtitle = "Number of Artists")
  })
  
  # Calculate number of genre
  output$numGenre <- renderValueBox({
    num_genre <- length(unique(filtered_data()$genre_by_bpm))
    valueBox(value = num_genre, color = "olive",
             subtitle = "Number of Genre")
  })

    output$datatable_track <- renderDT({
    data <- filtered_data() %>% 
      select(artist_name, track_name, streams,
             in_spotify_playlists, 
             in_apple_playlists,
             in_deezer_playlists) %>% 
      rename(artist = artist_name,
             track = track_name, 
             spotify = in_spotify_playlists, 
             apple = in_apple_playlists, 
             deezer = in_deezer_playlists) %>% 
      arrange(desc(streams))
    
    datatable(data, options = list(
      scrollX = TRUE,
      paginate = T,
      lengthMenu = c(5, 10, 15),
      pageLength = 20
    ))
    
  })
  
  output$tracksPerYearArtistPlot <- renderPlotly({
    if(is.null(input$artist_select)){
      return(NULL)
    }else{
      data <- filtered_data() %>%
        group_by(artist_name, released_year) %>%
        summarise(track_count = n(), .groups = 'drop')
      
      p <- ggplot(data, aes(x = released_year, y = track_count,
                            group = artist_name, color = artist_name)) +
        geom_line() + geom_point() +
        labs(x = "Year", y = "Number of Tracks", color ='Artist') +
        theme_minimal() 
      
      return(ggplotly(p))
      
    }
  })
  
  # Render the interactive plotly plot
  output$genrePopularityPlot <- renderPlotly({
    if(is.null(input$artist_select)){
      return(NULL)
    }else{
      data <-  filtered_data() %>% 
        group_by(artist_name, genre_by_bpm) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(desc(count))
      
      p <- ggplot(data, aes(x = reorder(genre_by_bpm, count), 
                            y = count, fill = genre_by_bpm)) +
        geom_bar(stat = "identity") +
        labs(fill ='Genre',
             x = "Genre",
             y = "Count") +
        theme_minimal() +
        coord_flip() +
        facet_grid( ~ artist_name)
      
      return(ggplotly(p))
    }
  })
  
  
  # Chatbot 
  
  execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE, reactiveValuesToList(session$input), {
      force(expr)
    }, ignoreInit = TRUE)
  }
  
  historyALL <- reactiveValues(df = data.frame() , 
                               val = character(0))
  
  # On click of send button
  observeEvent(input$submit_assistant, {
    
    if (nchar(trimws(input$prompt)) > 0) {
      
      # Spinner
      w <- Waiter$new(id = "chat-history",
                      html = spin_3(),
                      color = transparent(.5))
      w$show()
      
      # Response
      chatGPT <- ask_chatgpt(input$prompt)
      
      historyALL$val <- chatGPT
      history <- data.frame(users = c("Human", "AI"),
                            content = c(input$prompt, 
                                        markdown::mark_html(text=chatGPT)),
                            stringsAsFactors = FALSE)
      historyALL$df <- rbind(historyALL$df, history)
      updateTextInput(session, "prompt", value = "")
      
      # Conversation Interface
      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          tags$div(class = ifelse(historyALL$df[x, "users"] == "Human",
                                  "user-message",
                                  "bot-message"),
                   HTML(paste0(ifelse(historyALL$df[x, "users"] == "Human",
                                      "
<img src='images/human.jpg' class='img-wrapper2'>
",
                                      "
<img src='images/ChatGPT.png' class='img-wrapper2'>
"),
                               historyALL$df[x, "content"])))
        })
        do.call(tagList, conversations)
      })
      
      w$hide()
      execute_at_next_input(runjs(jscode))
      
    }
    
  })
  
  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({return(NULL)})
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })
  
  observe({
    req(input$clipbtn)
    CopyButtonUpdate(session,
                     id = "clipbtn",
                     label = "Copy",
                     icon = icon("clipboard"),
                     text = as.character(historyALL$val))
    
  })
  
  
}





