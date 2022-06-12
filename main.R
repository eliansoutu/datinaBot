library(telegram.bot)
library(tidyverse)

#bot <- Bot(token = bot_token("RDatinaBot"))

# start handler function
start <- function(bot, update)
{
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hola %s! Soy Datina, puedo ayudarte a conocer datos del turismo en Argentina.",
                                 update$message$from$first_name))
  
  Sys.sleep(2)
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Te sugiero algunos temas sobre los cuáles puedo brindarte información: \n*1. Ocupación Hotelera* \n*2. Turismo internacional*",
                  parse_mode = "Markdown")
  
  Sys.sleep(2)
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Indicáme el número del tema que te interesa para contarte las últimas novedades 📈")
}


# message handler function
respuesta <- function(bot, update)
{
  if (update$message$text == "1") {
    
    eoh <- read_csv("http://datos.yvera.gob.ar/dataset/93db331e-6970-4d74-8589-c1223ac9c282/resource/d1624c27-4b0d-4b73-b0f8-e7db5c56386f/download/viajeros-hospedados-residentes-y-no-residentes.csv")
    
    mes <- as.character.Date(max(eoh$indice_tiempo), "%B")
    residentes <- as.numeric(eoh[eoh$indice_tiempo==max(eoh$indice_tiempo) & eoh$origen_viajeros == "Residentes", "viajeros"])
    
    graph_eoh <- eoh %>% 
      filter(origen_viajeros=="Residentes") %>% 
      ggplot() +
      geom_line(aes(indice_tiempo, viajeros)) +
      theme_minimal()
    
    ggsave(plot = graph_eoh, "graphs/eoh.jpg", width = 12, height = 8)
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0("Según los últimos datos, en el mes de ", mes," se hospedaron un total de ",residentes," viajeros residentes."))
    
    Sys.sleep(3)
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Mirá este gráfico con la evolución de viajeros residentes:")
    
    Sys.sleep(2)
    
    bot$sendPhoto(chat_id = update$message$chat_id,
                  photo = "graphs/eoh.jpg")
    
  } else if (update$message$text == "2") {
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Ups! Aún no me programaron para esto 😅")
    
  } else {
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Espero haber sido de ayuda!")
  }
}

# build the updater
updater <- Updater(token = bot_token("RDatinaBot")) +
  CommandHandler("start", start) +
  MessageHandler(respuesta, MessageFilters$text)

# start polling
updater$start_polling()
