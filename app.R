library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(curl)
library(ellmer)  # Para API DeepSeek
library(readr)
library(markdown)  # Para converter Markdown em HTML

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = img(src = "logo.png", width = "120px", height = "auto"), 
                  dropdownMenu(type = NULL, badgeStatus = NULL,
                               messageItem("Termos e Transparência",
                                           NULL,
                                           href = "termos.html",
                                           time = NULL
                               ))),
  dashboardSidebar(disable = F, collapsed = T,
                   sidebarMenu(
                     menuItem("Chat", tabName = "home", icon = icon("comment")),
                     menuItem("Nossos Advogados", tabName = "advogadosLicenciados", icon = icon("suitcase"))
                     )
                   ),
  dashboardBody(
    useShinyjs(), # Inicia o shinyjs para usar delay de mensagens
    tags$head(
      includeCSS("www/menu.css"),
      
      # JS para rolar o chat para baixo
      tags$script(HTML("
        function scrollChatToBottom() {
          var chatContainer = document.querySelector('#chatResult');
          if (chatContainer) {
            chatContainer.scrollTop = chatContainer.scrollHeight;
          }
        }
        
        $(document).on('keydown', '#inputChat', function(e) {
          // Verifica se é a tecla Enter (13) e se o Shift NÃO está pressionado
          if (e.keyCode === 13 && !e.shiftKey) {
            e.preventDefault(); // Impede que o Enter pule uma linha no campo de texto
            $('#botaoEnviar').click(); // Simula um clique no botão 'Enviar'
          }
        });
        
        $(document).on('shiny:value', function(event) {
          if (event.target.id === 'chat') {
            setTimeout(scrollChatToBottom, 100);
          }
        });
      "))
    ),
    
    # Área do chat com scroll
    tags$div(id = "chatResult",
             uiOutput("chat")
    ),
    
    tags$div(id = "inline-chat",
             style = "margin-top: 15px;",
             textAreaInput("inputChat", placeholder = "Pergunte aqui", width = "92%", 
                           label = NULL, resize = "none", autoresize = T),
             actionButton("botaoEnviar", label = "Enviar")
    ),
    tags$footer(id = "footer", p("O AdvogAI é uma Inteligência Artificial. Ela pode cometer erros."))
  )
)

server <- function(input, output, session){
  
  # Inicializar o cliente DeepSeek
  deepseek_chat <- chat_deepseek(
    system_prompt = "Você é o AdvogAI, um assistente cujo o objetivo é somente 
    responder questões e dúvidas de pessoas a respeito do Direito e somente do Direito. 
    Qualquer pergunta que fuja da sua área você deve reforçar a sua resistência ao Direito. 
    Sempre que possível mostre leis, explique de uma forma clara e leiga o assunto para a 
    pessoa para ela sair sem dúvidas e no final, mostrar a ela quais seriam os próximos passos 
    vigentes sobre a questão que foi abordada.",
    model = "deepseek-chat",
    api_key = read_lines("deepseekAPI.txt")
  )
  
  # Histórico do chat
  chat_history <- reactiveVal(list())
  
  # Variável para armazenar temporariamente a mensagem que vai para a API
  message_to_send <- reactiveVal("")
  
  observeEvent(input$botaoEnviar, {
    user_message <- input$inputChat
    if (trimws(user_message) == "") return()
    
    current <- chat_history()
    
    # Adiciona a mensagem do usuário
    current <- c(current, list(list(role = "user", content = user_message)))
    
    # Adiciona a mensagem falsa de "digitando"
    typing_html <- "<div class='typing-indicator'><span></span><span></span><span></span></div>"
    current <- c(current, list(list(role = "typing", content = typing_html)))
    
    # Atualiza o chat na tela
    chat_history(current)
    
    # Limpa o input
    updateTextAreaInput(session, "inputChat", value = "")
    
    # Salva a mensagem para usar no próximo passo
    message_to_send(user_message)
    
    
    shinyjs::runjs("setTimeout(function() { Shiny.setInputValue('trigger_api', Math.random()); }, 50);")
  })
  
  # Chama a API após a tela ter atualizado
  observeEvent(input$trigger_api, {
    user_message <- message_to_send()
    if (user_message == "") return()
    
    tryCatch({
      # Enviar para a API
      ai_response <- deepseek_chat$chat(user_message)
      
      # Pegar histórico atual
      current <- chat_history()
      
      # Remover a última mensagem 
      current <- current[-length(current)]
      
      # Adicionar a resposta real da IA
      chat_history(c(current, list(list(role = "assistant", content = ai_response))))
      
    }, error = function(e) {
      current <- chat_history()
      current <- current[-length(current)]
      chat_history(c(current, list(list(role = "assistant", content = "Erro na conexão com a API."))))
    })
    
    # Limpa a variável temporária
    message_to_send("")
  })
  
  # Renderizar UI do Chat
  output$chat <- renderUI({
    messages <- chat_history()
    if (length(messages) == 0) return(HTML("<p style='color:gray; margin-left:25%; margin-right: 25%; 
                                          margin-top: 25%; text-align: center;'>
                                          Nenhuma mensagem ainda. Envie sua pergunta.</p>"))
    
    html_parts <- lapply(messages, function(msg) {
      role <- msg$role
      content <- msg$content
      
      # Se for a animação de digitando, não tentamos converter Markdown
      if (role == "typing") {
        html_content <- content
        class_bubble <- "message-assistant"
        style_div <- "text-align: left; margin: 8px 0;"
        style_bubble <- "display: inline-block; background-color: #fff; 
        border-radius: 12px; padding: 12px 16px; box-shadow: 0 1px 1px rgba(0,0,0,0.1);"
      } else {
        # Converter Markdown em HTML
        html_content <- markdownToHTML(text = content, fragment.only = TRUE)
        html_content <- gsub("^<body>|</body>$", "", html_content)
        
        class_bubble <- ifelse(role == "user", "message-user", "message-assistant")
        style_div <- ifelse(role == "user", "text-align: right; margin: 8px 0;", 
                            "text-align: left; margin: 8px 0;")
        style_bubble <- ifelse(role == "user",
                               "display: inline-block; background-color: #dcf8c6; 
                               border-radius: 12px; padding: 8px 12px; max-width: 80%; 
                               text-align: left;", "display: inline-block; background-color: #fff; 
                               border-radius: 12px; padding: 8px 12px; max-width: 80%; 
                               box-shadow: 0 1px 1px rgba(0,0,0,0.1); text-align: left;")
      }
      
      # Criar div da mensagem
      div(
        class = class_bubble,
        style = style_div,
        div(
          style = style_bubble,
          HTML(html_content)
        )
      )
    })
    
    do.call(tagList, html_parts)
  })
}

shinyApp(ui, server)