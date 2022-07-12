library(telegram.bot)
library(tidyverse)

bot <- Bot(token = bot_token("RDatinaBot"))

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
topics <- function(bot, update){
  if (update$message$text == "1") {
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Según los últimos datos, en el mes de ")
    
    eoh <- read_csv("http://datos.yvera.gob.ar/dataset/93db331e-6970-4d74-8589-c1223ac9c282/resource/a8b3a099-f9e5-4bdf-826a-9ff17a730be7/download/serie-tiempo-eoh-mensual.csv")
    
    mes <- as.character.Date(max(eoh$indice_tiempo), "%B")
    residentes <- as.numeric(eoh[eoh$indice_tiempo==max(eoh$indice_tiempo) ])
    
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
    
    ti <- read_csv("http://datos.yvera.gob.ar/dataset/4cbf7d4a-702a-4911-8c1e-717a45214902/resource/39455901-5488-4a01-8594-7b2b0a1e85a1/download/serie-tiempo-turismo-internacional.csv")
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Elige un subtema: \n*A. Receptivo* \n*B. Emisivo*",
                    parse_mode = "Markdown")
    
    mes <- as.character.Date(max(ti$indice_tiempo), "%B")
    
    if (update$message$text == "A") {
      receptivo <- ti %>% select(contains("receptivo"))
      
     

    } else {
      
      graph_ti <- ti %>% 
        filter(indice_tiempo == max(ti$indice_tiempo)) %>% 
        group_by(pais) %>% 
        summarise(balanza = sum(balanza)) %>% 
        ggplot() +
        geom_col(aes(pais, balanza), fill = "#6098cc") +
        theme_minimal()
      
      ggsave(plot = graph_ti, "graphs/ti.jpg", width = 12, height = 8)
      
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = paste0("Según los últimos datos, en el mes de ", mes," la balanza turística fue de",balanza))
      
      Sys.sleep(3)
    }
    
  } else {
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Espero haber sido de ayuda!")
  }
}


# build the updater
updater <- Updater(token = bot_token("RDatinaBot")) +
  CommandHandler("start", start) +
  MessageHandler(topics, MessageFilters$text)

# start polling
updater$start_polling()
