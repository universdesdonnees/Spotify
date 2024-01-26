base::source("packages.R")
base::source("global.r")

ui <- dashboardPage(skin = "black",
                    dbHeader,
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      box(
                        title = "Important Information",
                        status = "success",
                        solidHeader = TRUE,
                        width = 12,
                        p("This dashboard presents summarized data for one or multiple artists."),
                        p("Note: Only artists with at least 5 songs are included in the analysis.")
                      ),
                      box(
                        title = "Artist",
                        solidHeader = FALSE,collapsible = FALSE, width = 12,
                        column(3, align = "center"),
                        column(6, align = "center", 
                               pickerInput("artist_select", "",
                                           selected = "",
                                           choices = sort(unique(spotify_data$artist_name)),
                                           multiple = TRUE
                               ),
                               br()
                        ),
                        column(3, align = "center")
                      ),
                      box( collapsible = TRUE,
                        title = "Chat with ChatGPT",
                        width = 12,
                        useWaiter(),
                        useShinyjs(),
                        tags$head(tags$style(css)),
                        tags$div(
                          id = "chat-container",
                          tags$div(
                            id = "chat-header",
                            tags$img(src = "images/ChatGPT.png", alt = "AI Profile Picture")
                          ),
                          tags$div(
                            id = "chat-history",
                            uiOutput("chatThread"),
                          ),
                          tags$div(
                            id = "chat-input",
                            tags$form(
                              column(12,textAreaInput(inputId = "prompt", label="",
                                                      placeholder = "Ask your question...", 
                                                      width = "100%")),
                              fluidRow(
                                tags$div(style = "margin-left: 1.5em;",
                                         actionButton(inputId = "submit_assistant",
                                                      label = "Send",
                                                      icon = icon("paper-plane")),
                                         actionButton(inputId = "remove_chatThread",
                                                      label = "Clear History",
                                                      icon = icon("trash-can")),
                                         CopyButton("clipbtn",
                                                    label = "Copy",
                                                    icon = icon("clipboard"),
                                                    text = "")))))
                        )),
                      
                      # solidHeader = FALSE, width = 6,
                      # column(3, align = "center"),
                      # column(6, align = "center", 
                      #        textInput("message", "Your message:"),
                      #        actionButton("sendButton", "Send"),
                      #        textOutput("responseText")
                      # ),
                      #column(3, align = "center"),
                      
                      box(
                        title = "Stats",
                        solidHeader = FALSE,collapsible = FALSE, width = 6,
                        valueBoxOutput("numStreams", width = 6),
                        valueBoxOutput("numTracks", width = 6),
                        valueBoxOutput("numArtists", width = 6),
                        valueBoxOutput("numGenre", width = 6),
                        br(),
                        fluidRow(
                          box(plotlyOutput("genrePopularityPlot"),
                              title = "Popularity of Genre"),
                          box(plotlyOutput("tracksPerYearArtistPlot"),  
                              title = "No of Tracks per Year")
                        )
                      ),
                      br(), 
                      box(title = "Track & playlist presence", 
                          DTOutput("datatable_track"),
                      )
                      
                    )
)

