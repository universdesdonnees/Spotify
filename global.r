#### Lecture des data
spotify_data <- read.csv("data/spotify-2023.csv", 
                         stringsAsFactors = FALSE,
                         check.names = FALSE, quote = "\"",
                         header = TRUE, 
                         encoding = "UTF-8") %>% 
  rename(artist_name = `artist(s)_name`) %>% 
  mutate(streams = as.numeric(as.character(streams)))

#### Traitement des artistes

# séparation des artistes 
spotify_data$id_artists <- strsplit(spotify_data$artist_name, ",")
spotify_data <- tidyr::unnest(spotify_data, id_artists)

# retrait des espaces restants
spotify_data$artist_name = trimws(spotify_data$id_artists,
                                  which = "both")

# garder les artistes avec au moins 5 chansons

spotify_data <- spotify_data %>% 
  group_by(artist_name) %>%
  mutate(songs_per_artist = n()) %>%
  filter(songs_per_artist > 4) %>%
  ungroup()

#### Définition genre de musique 

# https://www.kine-formations.com/wp-content/uploads/2020/04/Liste-des-BPM-par-style-musical-Annexe-t%C3%A9l%C3%A9chargeable-%C3%A0-la-fin.pdf

get_genre_by_bpm <- function(bpm) {
  case_when(
    bpm == 80 ~ "Crunk",
    bpm >= 60 & bpm <= 90 ~ "Dub",
    bpm >= 80 & bpm <= 90 ~ "Reggae",
    bpm >= 80 & bpm <= 100 ~ "Hip-hop",
    bpm >= 80 & bpm <= 120 ~ "Rock Folk",
    bpm >= 90 & bpm <= 120 ~ "Rock pop",
    bpm >= 50 & bpm <= 56 ~ "Tango",
    bpm >= 80 & bpm <= 100 ~ "Salsa",
    bpm >= 60 & bpm <= 120 ~ "Trip hop",
    bpm >= 70 & bpm <= 120 ~ "Soul Music",
    bpm >= 100 & bpm <= 120 ~ "Chillstep",
    bpm >= 120 & bpm <= 135 ~ "Minimal",
    bpm >= 125 & bpm <= 135 ~ "Funky house",
    bpm >= 126 & bpm <= 135 ~ "Electro",
    bpm >= 125 & bpm <= 140 ~ "House music",
    bpm >= 130 & bpm <= 140 ~ "Trance",
    bpm >= 135 & bpm <= 145 ~ "Dubstep",
    bpm >= 120 & bpm <= 150 ~ "Techno",
    bpm >= 170 & bpm <= 180 ~ "Rock Punk",
    bpm >= 150 & bpm <= 190 ~ "Drum n bass",
    bpm >= 60 & bpm <= 220 ~ "Jazz",
    bpm >= 80 & bpm <= 220 ~ "Rock",
    bpm >= 160 & bpm <= 230 ~ "Hardcore",
    bpm >= 200 & bpm <= 500 ~ "Speedcore",
    bpm > 1000 ~ "Extratone",
    TRUE ~ "Other" # Default case
  )
}

spotify_data <- spotify_data %>%
  mutate(genre_by_bpm = get_genre_by_bpm(bpm))


#### Chatgpt 

css <- sass(sass_file("www/chat.scss"))

jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'

api_key <- "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"

ask_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "user",
             content = prompt),
        list(role =  "system",
             content = " I want you to assist me about the music industry")
      )
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}


#### Header application
dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <- tags$a(href='http://mycompanyishere.com',
                                          tags$img(src='my_Logo.jpg',height='40',
                                                   width='130'))
