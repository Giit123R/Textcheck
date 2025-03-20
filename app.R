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
# Global constants for settings.

GLOBAL_TEXT_MIN_ZEICHEN <- 50
GLOBAL_TEXT_MAX_ZEICHEN <- 10000
GLOBAL_API_SCHLUESSEL <- ""


###################################################################################################
# Helper functions concerning the UI.

funk_card_style_anpassen <- function(arg_cardnummer){
  #' Styles the HTML elements of the Shiny Card object with the Card number from argument 
  #' arg_cardnummer.
  
  tags$head(
    tags$style(HTML(paste0("#Header_Eingabe_Text_",
                           arg_cardnummer,
                           "{font-weight: bold}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Header_Ausgabe_Text_", arg_cardnummer,
                           "{font-weight: bold; margin-top: 20px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Button_", arg_cardnummer,
                           "{color: black; background-color:orange; margin-top: 7px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Card_Plot_",
                           arg_cardnummer,
                           "{max-height: 533px}"
                    )
              )
    ),
    tags$style(HTML(paste0("#Card_Textuebersicht_",
                           arg_cardnummer,
                           "{margin-top: 0}"
                    )
              )
    )
  )
}


funk_card_objekt_erstellen = function(arg_cardnummer){
  #' Builds and returns a Shiny Card object.
  
  card_objekt <- card(
    card_body(
      h4(id = paste0("Header_Eingabe_Text_", arg_cardnummer),
         span(HTML(paste0("Eingabe Text ", arg_cardnummer, ":")))
      ),
      textAreaInput(inputId = paste0("Eingabe_Text_", arg_cardnummer),
                    label = NULL,
                    value = "",
                    height = "120px",
                    width = "100%",
                    placeholder = paste0("Hier bitte Text ",
                                        arg_cardnummer,
                                        " eingeben! (",
                                        GLOBAL_TEXT_MIN_ZEICHEN,
                                        " bis ",
                                        GLOBAL_TEXT_MAX_ZEICHEN, 
                                        " Zeichen)"
                                  ),
                    resize = "vertical"
      ),
      actionButton(paste0("Button_", arg_cardnummer),
                   paste("!!! Check von Text", arg_cardnummer,"starten !!!")
                   ),
      h4(id = paste0("Header_Ausgabe_Text_", arg_cardnummer),
         span(HTML(paste0("Ausgabe Text ", arg_cardnummer, ":")))
      ),
      card(
        id = paste0("Card_Plot_", arg_cardnummer),
        card_body(
          height = 533,
          max_height = 533,
          plotlyOutput(outputId = paste0("Ausgabe_Plot_", arg_cardnummer))
        )
      ),
      card(
        id = paste0("Card_Textuebersicht_", arg_cardnummer),
        card_body(
          htmlOutput(outputId = paste0("Ausgabe_String_Textuebersicht_", arg_cardnummer))
        )
      )
    )
  )
  
  return(card_objekt)
}


###################################################################################################
# Create central Shiny object for the UI.

ui_objekt <- page_navbar(
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
                Card_Objekt_erstellt_1 <- funk_card_objekt_erstellen(arg_cardnummer = "1"),
                Card_Objekt_erstellt_2 <- funk_card_objekt_erstellen(arg_cardnummer = "2")
              ),
              funk_card_style_anpassen(arg_cardnummer = "1"),
              funk_card_style_anpassen(arg_cardnummer = "2")
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

funk_liste_saetze_erstellen <- function(arg_eingabe_text){
  #' Divides the text from argument arg_eingabe_text into its sentences and returns a list of 
  #' these sentences. 
  
  eingabe_text <- trimws(arg_eingabe_text)
  if (is.na(eingabe_text)
      | is.null(eingabe_text)
      | (nchar(eingabe_text) < GLOBAL_TEXT_MIN_ZEICHEN)
      ){
    eingabe_text <- "Text zu kurz!"  
  }
  
  else if (nchar(eingabe_text) > GLOBAL_TEXT_MAX_ZEICHEN){
    eingabe_text <- substr(eingabe_text, 1, GLOBAL_TEXT_MAX_ZEICHEN)
  }
  
  eingabe_text_tokens <- tokenize_sentences(eingabe_text)
  liste_saetze <- unlist(eingabe_text_tokens)
  
  return(liste_saetze)
}


funk_dataframe_satzbloecke_erstellen <- function(arg_liste_saetze){
  #' Creates and returns a dataframe template for the results. This template is populated with one 
  #' blocks of 5 sentences per each row of the dataframe. Later it will be merged with the results 
  #' from ChatGPT.
  
  # List for blocks of sentences (5 sentences per block)
  liste_satzbloecke <- list()
  # List for blocks of sentences (5 sentences per block): formatted for hovering in Plotly
  liste_satzbloecke_fuer_hover <- list()
  
  # Create a new block for every 5 sentences in the text and add it to the lists
  for (i in seq(1, length(arg_liste_saetze), by = 5)) {
    satzblock <- paste(
      arg_liste_saetze[i:min(i + 4, length(arg_liste_saetze))],
      collapse = " "                                                                                      )
    
    liste_satzbloecke[[length(liste_satzbloecke) + 1]] <- satzblock
    
    liste_zeichen_satzblock <- unlist(strsplit(satzblock, split = ""))
    satzblock_fuer_hover = ""
    
    zaehler_zeichen <- 0
    zaehler_linebreak <- 0
    
    for (zeichen_x in liste_zeichen_satzblock){
      satzblock_fuer_hover <- paste0(satzblock_fuer_hover, zeichen_x)
      zaehler_zeichen <- zaehler_zeichen + 1
      
      if (zaehler_zeichen %/% 50 > zaehler_linebreak){
        if (zeichen_x == " "){
          zaehler_linebreak <- zaehler_linebreak + 1
          satzblock_fuer_hover <- paste0(satzblock_fuer_hover, "<br>")
        } 
      }
    }
    
    liste_satzbloecke_fuer_hover[[length(liste_satzbloecke_fuer_hover) + 1]] <- 
      satzblock_fuer_hover
  }
  
  # Create dataframe from populated lists
  dataframe_satzbloecke <- data.frame(
    Blocknummer = 1:length(liste_satzbloecke),
    Blockinhalt = unlist(liste_satzbloecke),
    Blockinhalt_fuer_hover = unlist(liste_satzbloecke_fuer_hover),
    stringsAsFactors = FALSE
  )
  
  return(dataframe_satzbloecke)
}


funk_nachricht_fuer_gpt_erstellen <- function(arg_dataframe_satzbloecke){
  #' Creates the message for sending to ChatGPT by concatenating the prompt with the blocks of 
  #' sentences. Returns this message.
  
  # Load prompt from txt file
  nachricht <- paste(readLines("Prompt.txt"), collapse = "\n")
  
  for (i in 1:nrow(arg_dataframe_satzbloecke)){
    blocknummer <- arg_dataframe_satzbloecke[i, "Blocknummer"]
    blocknummer_str <- paste(c("Satzblock", blocknummer), collapse = " ")
    blocknummer_str <- paste(blocknummer_str, ":", sep = "")
    
    blockinhalt <- arg_dataframe_satzbloecke[i, "Blockinhalt"]
    
    nachricht <- paste(nachricht, blocknummer_str, sep = "\n")
    nachricht <- paste(nachricht, blockinhalt, sep = " ")
  }
  
  return(nachricht)
}


funk_liste_fuer_json_erstellen <- function(){
  #' Returns a list which could be converted to JSON. This JSON will be sent to ChatGPT so that 
  #' ChatGPT will return its answer / output in the required format.
  
  liste_fuer_json = list(
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
  
  return(liste_fuer_json)
}


funk_dataframe_von_gpt_anfordern <- function(arg_nachricht_fuer_gpt, arg_liste_fuer_json){
  #' Sends the message from argument arg_nachricht_fuer_gpt to ChatGPT and returns a dataframe 
  #' which contains the formatted answer.
  
  antwort_von_gpt <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    
    add_headers(Authorization = paste("Bearer", GLOBAL_API_SCHLUESSEL)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "",
      messages = list(
        list(role = "user", content = arg_nachricht_fuer_gpt)
      ),
      response_format = arg_liste_fuer_json
    )
  )
  
  antwort_kontent <- content(antwort_von_gpt)$choices[[1]]$message$content
  antwort_kontent_dataframe <- fromJSON(antwort_kontent)$Satzblock

  return(antwort_kontent_dataframe)
}


funk_plotly_objekt_erstellen <- function(arg_dataframe){
  # Creates a Plotly object using the data from argument arg_dataframe and returns it.
  plotly_objekt <- plot_ly(arg_dataframe,
                           x = ~Blocknummer,
                           y = ~Komplexitaet_Sprache,
                           name = "Komplexität Sprache",
                           type = "scatter",
                           mode = "lines+markers",
                           marker = list(size = 10),
                           height = 500,
                           text = arg_dataframe$Blockinhalt_fuer_hover,
                           hoverinfo = "text"
                    )
  
  plotly_objekt <- plotly_objekt %>% layout(legend = list(orientation = "v",
                                                          xanchor = "left",
                                                          x = -0.1,
                                                          yanchor = "top",
                                                          y= -0.15,
                                                          font = list(size = 10, color = "black"),
                                                          bgcolor = "#c1c1c1"
                                                      )
                                      )
  
  liste_fuer_traces <- list("Komplexitaet_Inhalt" = "Komplexität Inhalt",
                            "Wahrheitsgehalt" = "Wahrheitsgehalt",
                            "Neutralitaet" = "Neutralität",
                            "Sentiment" = "Sentiment"
                       )
  
  for (x in names(liste_fuer_traces)){
    plotly_objekt <- plotly_objekt %>% add_trace(y = arg_dataframe[[x]],
                                                 name = liste_fuer_traces[[x]],
                                                 mode = "lines+markers"
                                       )
    
  }

  anzahl_zeilen_df <- nrow(arg_dataframe)
  anzahl_fuer_x_achse <- max(anzahl_zeilen_df + 0.5, 10.5)
  
  plotly_objekt <- plotly_objekt %>% layout(
    margin = list(t = 0, b = 0),
    hoverlabel = list(font = list(size = 9), align = "left"),
    xaxis = list(title = list(text = "Satzblock", standoff = 3),
                 range = c(0, anzahl_fuer_x_achse),
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

return(plotly_objekt)
}


funk_string_textuebersicht_erstellen <- function(arg_dataframe){
  #' Returns the formatted text as a single string (for presenting it to the user in the UI).
  
  string_textuebersicht <- "<p style='color:red;'>Durch einen Klick auf die Kriteriennamen in der 
  Legende kannst du einzelne Linien aus- und einblenden! Du kannst außerdem im Diagramm auf die 
  Datenpunkte klicken, um die damit verbundenen Satzblöcke einzusehen. Alternativ findest du sie 
  auch unter diesem Hinweis.</p>"
  
  for (i in 1:nrow(arg_dataframe)){
    string_satzblock <- paste0("<b>Satzblock ",
                               arg_dataframe$Blocknummer[i],
                               ": </b>",
                               arg_dataframe$Blockinhalt[i],
                               "</p>"
    )
    string_textuebersicht <- paste0(string_textuebersicht, string_satzblock)
  }
  
  return(string_textuebersicht)
}


funk_arbeiten_buttonpress <- function(arg_userinput){
  #' This function uses all the above functions to create a data pipeline after the user pressed 
  #' the button in the UI.
  
  liste_saetze <- funk_liste_saetze_erstellen(arg_eingabe_text = arg_userinput)
  
  dataframe_satzbloecke <- funk_dataframe_satzbloecke_erstellen(arg_liste_saetze = liste_saetze)

  nachricht_fuer_gpt <- funk_nachricht_fuer_gpt_erstellen(
    arg_dataframe_satzbloecke = dataframe_satzbloecke
  )
  
  liste_fuer_json <- funk_liste_fuer_json_erstellen()
  
  dataframe_von_gpt <- funk_dataframe_von_gpt_anfordern(arg_nachricht_fuer_gpt = nachricht_fuer_gpt,
                                                        arg_liste_fuer_json = liste_fuer_json
                       )
  
  dataframe_merged <- merge(dataframe_satzbloecke, dataframe_von_gpt, by = "Blocknummer")
  
  plotly_objekt <- funk_plotly_objekt_erstellen(arg_dataframe = dataframe_merged)
  
  string_textuebersicht <- funk_string_textuebersicht_erstellen(arg_dataframe = dataframe_merged)
  
  # Final results of the computations
  arbeitsergebnisse <- list("Plotly_Objekt" = plotly_objekt,
                            "String_Textuebersicht" = string_textuebersicht
                            )
  
  return(arbeitsergebnisse)
}


###################################################################################################
# Central function for the Shiny server logic, i. e. the actual computations.

funk_server_erstellen <- function(input, output){
  
  # For Button 1
  observeEvent(input$Button_1, {
    withProgress(message = "Check von Text 1 läuft!", value = 0, {
      incProgress(0.33)
      
      arbeitsergebnisse_1 <- funk_arbeiten_buttonpress(arg_userinput = input$Eingabe_Text_1)
      
      output$Ausgabe_Plot_1 <- 
        renderPlotly({arbeitsergebnisse_1$Plotly_Objekt
      })
      incProgress(0.66)
      
      output$Ausgabe_String_Textuebersicht_1 <- renderText({
        arbeitsergebnisse_1$String_Textuebersicht
      })
      incProgress(0.99)
    })  
  })
  
  # For Button 2
  observeEvent(input$Button_2, {
    withProgress(message = "Check von Text 2 läuft!", value = 0, {
      incProgress(0.33)
      
      arbeitsergebnisse_2 <- funk_arbeiten_buttonpress(arg_userinput = input$Eingabe_Text_2)
      
      output$Ausgabe_Plot_2 <- renderPlotly({
        arbeitsergebnisse_2$Plotly_Objekt
      })
      incProgress(0.66)
      
      output$Ausgabe_String_Textuebersicht_2 <- renderText({
        arbeitsergebnisse_2$String_Textuebersicht
      })
      incProgress(0.99)
    })  
  })
}


###################################################################################################
# Run the application in Shiny. This can also be considered as the main function of the script.

shinyApp(ui = ui_objekt, server = funk_server_erstellen)
