#' This script contains a typical Shiny app. Therefore, for a better understanding, it is 
#' recommended to read the script also from the bottom to the top.


###################################################################################################
library("shiny")
library("dplyr")
library("bslib")
library("httr")
library("jsonlite")
library("tokenizers")
library("plotly")


###################################################################################################
# Global constants for settings

GLOBAL_TEXT_MIN_ZEICHEN <- 50
GLOBAL_TEXT_MAX_ZEICHEN <- 10000
GLOBAL_API_SCHLUESSEL <- ""


###################################################################################################
# Helper functions concerning the UI.

Funk_Card_Style_anpassen <- function(Arg_Cardnummer){
  #' Styles the HTML elements of the Shiny Card object with the Card number from argument 
  #' Arg_Cardnummer.
  tags$head(
    tags$style(HTML(paste0("#Header_Eingabe_Text_",
                           Arg_Cardnummer,
                           "{font-weight: bold}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Header_Ausgabe_Text_", Arg_Cardnummer,
                           "{font-weight: bold; margin-top: 20px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Button_", Arg_Cardnummer,
                           "{color: black; background-color:orange; margin-top: 7px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Card_Plot_",
                           Arg_Cardnummer,
                           "{max-height: 533px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Card_Textuebersicht_",
                           Arg_Cardnummer,
                           "{margin-top: 0}"
                    )
              )
    )
  )
}


Funk_Card_Objekt_erstellen = function(Arg_Cardnummer){
  #' Builds and returns a Shiny Card object.
  Card_Objekt <- card(
    card_body(
      h4(id = paste0("Header_Eingabe_Text_", Arg_Cardnummer),
         span(HTML(paste0("Eingabe Text ", Arg_Cardnummer, ":")))
      ),
      textAreaInput(inputId = paste0("Eingabe_Text_", Arg_Cardnummer),
                    label = NULL,
                    value = "",
                    height = "120px",
                    width = "100%",
                    placeholder = paste0("Hier bitte Text ",
                                        Arg_Cardnummer,
                                        " eingeben! (",
                                        GLOBAL_TEXT_MIN_ZEICHEN,
                                        " bis ",
                                        GLOBAL_TEXT_MAX_ZEICHEN, 
                                        " Zeichen)"
                                  ),
                    resize = "vertical"
      ),
      actionButton(paste0("Button_", Arg_Cardnummer),
                   paste("!!! Check von Text", Arg_Cardnummer,"starten !!!")
                   ),
      h4(id = paste0("Header_Ausgabe_Text_", Arg_Cardnummer),
         span(HTML(paste0("Ausgabe Text ", Arg_Cardnummer, ":")))
      ),
      card(
        id = paste0("Card_Plot_", Arg_Cardnummer),
        card_body(
          height = 533,
          max_height = 533,
          plotlyOutput(outputId = paste0("Ausgabe_Plot_", Arg_Cardnummer))
        )
      ),
      card(
        id = paste0("Card_Textuebersicht_", Arg_Cardnummer),
        card_body(
          htmlOutput(outputId = paste0("Ausgabe_String_Textuebersicht_", Arg_Cardnummer))
        )
      )
    )
  )
  
  return(Card_Objekt)
}


###################################################################################################
# Create central Shiny object for the UI.

UI_Objekt <- page_navbar(
  title = list("Text Check",
               tags$head(
                 tags$link(rel = "icon",
                           type = "image/png",
                           sizes = "32x32",
                           href = "/Icon_fuer_Browser.png"
                      )
                 )
               ),
  bg = "#2D89C8",
  inverse = TRUE,

  nav_panel(title = "Checken",
            page_fluid(
              tags$head(
                tags$style(HTML(":root{--bslib-spacer: 0.5rem}")),
                tags$style(HTML("#shiny-notification-panel{position: relative; 
                                top: 40%;left: 33%}"
                          )
                )
              ),
              layout_columns(
                col_widths = c(6, 6),
                Card_Objekt_erstellt_1 <- Funk_Card_Objekt_erstellen(Arg_Cardnummer = "1"),
                Card_Objekt_erstellt_2 <- Funk_Card_Objekt_erstellen(Arg_Cardnummer = "2")
              ),
              Funk_Card_Style_anpassen(Arg_Cardnummer = "1"),
              Funk_Card_Style_anpassen(Arg_Cardnummer = "2")
            )
  ),
  nav_panel(title = "ANLEITUNG",
            HTML(paste(readLines("Dateien_fuer_HTML/JS_Funktionen.txt"), collapse = "\n")),
            HTML(paste(readLines("Dateien_fuer_HTML/Anleitung.txt"), collapse = "\n"))
  ),
  nav_panel(title = "Hinweise",
            HTML(paste(readLines("Dateien_fuer_HTML/Hinweise.txt"), collapse = "\n"))
  ),          
  nav_panel(title = "Prompt",
            HTML(paste(readLines("Dateien_fuer_HTML/Prompt.txt"), collapse = "\n"))
  ),
)


###################################################################################################
# Helper functions concerning the server logic, i. e. the actual computations.

Funk_Liste_Saetze_erstellen <- function(Arg_Eingabe_Text){
  #' Divides the text from argument Arg_Eingabe_Text into its sentences and returns a list of 
  #' these sentences. 
  Eingabe_Text <- trimws(Arg_Eingabe_Text)
  if (is.na(Eingabe_Text)
      | is.null(Eingabe_Text)
      | (nchar(Eingabe_Text) < GLOBAL_TEXT_MIN_ZEICHEN)
      ){
    Eingabe_Text <- "Text zu kurz!"  
  }
  
  else if (nchar(Eingabe_Text) > GLOBAL_TEXT_MAX_ZEICHEN){
    Eingabe_Text <- substr(Eingabe_Text, 1, GLOBAL_TEXT_MAX_ZEICHEN)
  }
  
  Eingabe_Text_Tokens <- tokenize_sentences(Eingabe_Text)
  Liste_Saetze <- unlist(Eingabe_Text_Tokens)
  
  return(Liste_Saetze)
}


Funk_Dataframe_Satzbloecke_erstellen <- function(Arg_Liste_Saetze){
  #' Creates and returns a dataframe template for the results. This template is populated with one 
  #' block of 5 sentences per row. Later it will be merged with the results from ChatGPT.
  # List for blocks of sentences (5 sentences per block)
  Liste_Satzbloecke <- list()
  # List for blocks of sentences (5 sentences per block): formatted for hovering in Plotly
  Liste_Satzbloecke_fuer_hover <- list()
  
  # Create a new row for every 5 sentences in the text
  for (i in seq(1, length(Arg_Liste_Saetze), by = 5)) {
    Satzblock <- paste(
      Arg_Liste_Saetze[i:min(i + 4, length(Arg_Liste_Saetze))],
      collapse = " "                                                                                      )
    
    Liste_Satzbloecke[[length(Liste_Satzbloecke) + 1]] <- Satzblock
    
    Liste_Zeichen_Satzblock <- unlist(strsplit(Satzblock, split = ""))
    Satzblock_fuer_hover = ""
    
    Zaehler_Zeichen <- 0
    Zaehler_Linebreak <- 0
    
    for (Zeichen_x in Liste_Zeichen_Satzblock){
      Satzblock_fuer_hover <- paste0(Satzblock_fuer_hover, Zeichen_x)
      Zaehler_Zeichen <- Zaehler_Zeichen + 1
      
      if (Zaehler_Zeichen %/% 50 > Zaehler_Linebreak){
        if (Zeichen_x == " "){
          Zaehler_Linebreak <- Zaehler_Linebreak + 1
          Satzblock_fuer_hover <- paste0(Satzblock_fuer_hover, "<br>")
        } 
      }
    }
    
    Liste_Satzbloecke_fuer_hover[[length(Liste_Satzbloecke_fuer_hover) + 1]] <- 
      Satzblock_fuer_hover
  }
  
  # Create dataframe from populated lists
  Dataframe_Satzbloecke <- data.frame(
    Blocknummer = 1:length(Liste_Satzbloecke),
    Blockinhalt = unlist(Liste_Satzbloecke),
    Blockinhalt_fuer_hover = unlist(Liste_Satzbloecke_fuer_hover),
    stringsAsFactors = FALSE
  )
  
  return(Dataframe_Satzbloecke)
}


Funk_Nachricht_fuer_GPT_erstellen <- function(Arg_Dataframe_Satzbloecke){
  #' Creates the message for sending to ChatGPT by concatenating the prompt with the blocks of 
  #' sentences. Returns this message.
  # Load prompt from txt file
  Nachricht <- paste(readLines("Prompt.txt"), collapse = "\n")
  
  for (i in 1:nrow(Arg_Dataframe_Satzbloecke)){
    Blocknummer <- Arg_Dataframe_Satzbloecke[i, "Blocknummer"]
    Blocknummer_str <- paste(c("Satzblock", Blocknummer), collapse = " ")
    Blocknummer_str <- paste(Blocknummer_str, ":", sep = "")
    
    Blockinhalt <- Arg_Dataframe_Satzbloecke[i, "Blockinhalt"]
    
    Nachricht <- paste(Nachricht, Blocknummer_str, sep = "\n")
    Nachricht <- paste(Nachricht, Blockinhalt, sep = " ")
  }
  
  return(Nachricht)
}


Funk_Liste_fuer_JSON_erstellen <- function(){
  #' Returns a list which could be converted to JSON. This JSON be sent to ChatGPT so that it will
  #' return its output in the required format.
  Liste_fuer_JSON = list(
    type = "json_schema",
    json_schema = list(
      strict = TRUE,
      name = "Antwort",
      schema = list(
        type = "object",
        properties = list(
          Satzblock = list(
            type = "array",
            items = list(
              type = "object",
              properties = list(
                Blocknummer = list(type = "integer"),
                Komplexitaet_Sprache = list(type = "integer"),
                Komplexitaet_Inhalt = list(type = "integer"),
                Wahrheitsgehalt = list(type = "integer"),
                Neutralitaet = list(type = "integer"),
                Sentiment = list(type = "integer")
              ),
              required = list("Blocknummer",
                              "Komplexitaet_Sprache",
                              "Komplexitaet_Inhalt",
                              "Wahrheitsgehalt",
                              "Neutralitaet",
                              "Sentiment"
              ),
              additionalProperties = FALSE
            )
          )
        ),
        required = list("Satzblock"),
        additionalProperties = FALSE
      )
    )
  )
  
  return(Liste_fuer_JSON)
}


Funk_Dataframe_von_GPT_anfordern <- function(Arg_Nachricht_fuer_GPT, Arg_Liste_fuer_JSON){
  #' Sends the message from argument Arg_Nachricht_fuer_GPT to ChatGPT and returns a dataframe 
  #' which contains the formatted answer.
  Antwort_von_GPT <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    
    add_headers(Authorization = paste("Bearer", GLOBAL_API_SCHLUESSEL)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "",
      messages = list(
        list(role = "user", content = Arg_Nachricht_fuer_GPT)
      ),
      response_format = Arg_Liste_fuer_JSON
    )
  )
  
  Antwort_Kontent <- content(Antwort_von_GPT)$choices[[1]]$message$content
  Antwort_Kontent_Dataframe <- fromJSON(Antwort_Kontent)$Satzblock

  return(Antwort_Kontent_Dataframe)
}


Funk_Plotly_Objekt_erstellen <- function(Arg_Dataframe){
  # Creates a Plotly object using the data from argument Arg_Dataframe and returns it.
  Plotly_Objekt <- plot_ly(Arg_Dataframe,
                           x = ~Blocknummer,
                           y = ~Komplexitaet_Sprache,
                           name = "Komplexität Sprache",
                           type = "scatter",
                           mode = "lines+markers",
                           marker = list(size = 10),
                           height = 500,
                           text = Arg_Dataframe$Blockinhalt_fuer_hover,
                           hoverinfo = "text"
                    )
  
  Plotly_Objekt <- Plotly_Objekt %>% layout(legend = list(orientation = "v",
                                                          xanchor = "left",
                                                          x = -0.1,
                                                          yanchor = "top",
                                                          y= -0.15,
                                                          font = list(size = 10, color = "black"),
                                                          bgcolor = "#c1c1c1"
                                                      )
                                      )
  
  Liste_fuer_Traces <- list("Komplexitaet_Inhalt" = "Komplexität Inhalt",
                            "Wahrheitsgehalt" = "Wahrheitsgehalt",
                            "Neutralitaet" = "Neutralität",
                            "Sentiment" = "Sentiment"
                       )
  
  for (x in names(Liste_fuer_Traces)){
    Plotly_Objekt <- Plotly_Objekt %>% add_trace(y = Arg_Dataframe[[x]],
                                                 name = Liste_fuer_Traces[[x]],
                                                 mode = "lines+markers"
                                       )
    
  }

  Anzahl_Zeilen_df <- nrow(Arg_Dataframe)
  Anzahl_fuer_x_Achse <- max(Anzahl_Zeilen_df + 0.5, 10.5)
  
  Plotly_Objekt <- Plotly_Objekt %>% layout(
    margin = list(t = 0, b = 0),
    hoverlabel = list(font = list(size = 9), align = "left"),
    xaxis = list(title = list(text = "Satzblock", standoff = 3),
                 range = c(0, Anzahl_fuer_x_Achse),
                 dtick = 1,
                 tick0 = 0,
                 fixedrange = TRUE,
                 linecolor = "#444",
                 linewidth = 2,
                 showgrid = TRUE,
                 gridcolor = "#777",
                 gridwidth = 1
            ),
    yaxis = list(title = list(text = "Wert", standoff = 3),
                 range = c(0, 10.5),
                 dtick = 1,
                 tick0 = 0,
                 fixedrange = TRUE,
                 linecolor = "#444",
                 linewidth = 2,
                 showgrid = TRUE,
                 gridcolor = "#777",
                 gridwidth = 1
            )
    )

return(Plotly_Objekt)
}


Funk_String_Textuebersicht_erstellen <- function(Arg_Dataframe){
  #' Returns the formatted text as a single string (for presenting it to the user in the UI).
  String_Textuebersicht <- "<p style='color:red;'>Durch einen Klick auf die Kriteriennamen in der 
  Legende kannst du einzelne Linien aus- und einblenden! Du kannst außerdem im Diagramm auf die 
  Datenpunkte klicken, um die damit verbundenen Satzblöcke einzusehen. Alternativ findest du sie 
  auch unter diesem Hinweis.</p>"
  
  for (i in 1:nrow(Arg_Dataframe)){
    String_Satzblock <- paste0("<b>Satzblock ",
                               Arg_Dataframe$Blocknummer[i],
                               ": </b>",
                               Arg_Dataframe$Blockinhalt[i],
                               "</p>"
    )
    String_Textuebersicht <- paste0(String_Textuebersicht, String_Satzblock)
  }
  
  return(String_Textuebersicht)
}


Funk_Arbeiten_Buttonpress <- function(Arg_Userinput){
  #' This function uses all the above functions to create a data pipeline after the user pressed 
  #' the button in the UI.
  Liste_Saetze <- Funk_Liste_Saetze_erstellen(Arg_Eingabe_Text = Arg_Userinput)
  
  Dataframe_Satzbloecke <- Funk_Dataframe_Satzbloecke_erstellen(Arg_Liste_Saetze = Liste_Saetze)

  Nachricht_fuer_GPT <- Funk_Nachricht_fuer_GPT_erstellen(
    Arg_Dataframe_Satzbloecke = Dataframe_Satzbloecke
  )
  
  Liste_fuer_JSON <- Funk_Liste_fuer_JSON_erstellen()
  
  Dataframe_von_GPT <- Funk_Dataframe_von_GPT_anfordern(Arg_Nachricht_fuer_GPT = Nachricht_fuer_GPT,
                                                        Arg_Liste_fuer_JSON = Liste_fuer_JSON
                       )
  
  Dataframe_merged <- merge(Dataframe_Satzbloecke, Dataframe_von_GPT, by = "Blocknummer")
  
  Plotly_Objekt <- Funk_Plotly_Objekt_erstellen(Arg_Dataframe = Dataframe_merged)
  
  String_Textuebersicht <- Funk_String_Textuebersicht_erstellen(Arg_Dataframe = Dataframe_merged)
  
  # Final results of the computations
  Arbeitsergebnisse <- list("Plotly_Objekt" = Plotly_Objekt,
                            "String_Textuebersicht" = String_Textuebersicht
                            )
  
  return(Arbeitsergebnisse)
}


###################################################################################################
# Central function for the Shiny server logic, i. e. the actual computations.

Funk_Server_erstellen <- function(input, output){
  # For Button 1
  observeEvent(input$Button_1, {
    withProgress(message = "Check von Text 1 läuft!", value = 0, {
      incProgress(0.33)
      
      Arbeitsergebnisse_1 <- Funk_Arbeiten_Buttonpress(Arg_Userinput = input$Eingabe_Text_1)
      
      output$Ausgabe_Plot_1 <- 
        renderPlotly({Arbeitsergebnisse_1$Plotly_Objekt
      })
      incProgress(0.66)
      
      output$Ausgabe_String_Textuebersicht_1 <- renderText({
        Arbeitsergebnisse_1$String_Textuebersicht
      })
      incProgress(0.99)
    })  
  })
  
  # For Button 2
  observeEvent(input$Button_2, {
    withProgress(message = "Check von Text 2 läuft!", value = 0, {
      incProgress(0.33)
      
      Arbeitsergebnisse_2 <- Funk_Arbeiten_Buttonpress(Arg_Userinput = input$Eingabe_Text_2)
      
      output$Ausgabe_Plot_2 <- renderPlotly({
        Arbeitsergebnisse_2$Plotly_Objekt
      })
      incProgress(0.66)
      
      output$Ausgabe_String_Textuebersicht_2 <- renderText({
        Arbeitsergebnisse_2$String_Textuebersicht
      })
      incProgress(0.99)
    })  
  })
}


###################################################################################################
# Run the application in Shiny. This can also be considered as the main function of the script.

shinyApp(ui = UI_Objekt, server = Funk_Server_erstellen)
