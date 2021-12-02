#install.packages("shiny")
library(shiny)
library(readxl)

ui <- fluidPage(
  
  titlePanel("Avaliação de sensores"),
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        'input.tabs === "Tabela"',
        
        #Entrada: seleção do do arquivo
        fileInput(inputId = "arquivo", label = "Selecione o arquivo",
                  multiple = FALSE,
                  accept = ".xlsx",
                  buttonLabel = "Selecione",
                  placeholder = "Nenhum arquivo selecionado"),
      
        
      ),
      
      
      conditionalPanel(
        'input.tabs === "Resultados"',
        
        numericInput(inputId = "valor_real", label = "Informe o valor real para exatidão", value = 0),
        
        numericInput(inputId = "margem_erro", label = "Informe o valor da margem de erro para temporalidade", value = 0),
        
        actionButton("calcular", "Calcular"),
        
        hr(),
        
        radioButtons("radio_exatidao", label = "Peso Exatidão",
                     choices = c(1,2,3,4), 
                     inline = T),
        
        radioButtons("radio_precisao", label = "Peso Precisão",
                     choices = c(1,2,3,4),
                     inline = T),
        
        
        radioButtons("radio_temporalidade", label = "Peso Temporalidade",
                     choices = c(1,2,3,4),
                     inline = T),
        
        radioButtons("radio_corretude", label = "Peso Corretude",
                     choices = c(1,2,3,4),
                     inline = T),
        
        actionButton("reputacao", "Reputação do sensor")
        
      ),
      
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Tabela", dataTableOutput("tabela")),
        tabPanel("Resultados", tableOutput("calculos"), htmlOutput("reputacao"))
      )
    )
  )
)


server <- function(input, output) {
  
  calculo_exatidao <- function(media, valor_real){
    return ((valor_real * 100) / media) 
  }
  
  calculo_precisao <- function(desvio_padrao, media){
    calculo <- (desvio_padrao / media) * 100
    
    return (calculo - 100)
  }
  
  calculo_corretude <- function(dados){
    q1 <- quantile(dados, 0.25)
    q3 <- quantile(dados, 0.75)
    lim_inferior <- q1 - (1.5 * IQR(dados))
    lim_superior <- q3 + (1.5 * IQR(dados))
    
    cont <- 0
    
    for(i in 1:(length(dados))) {
      if(dados[i] < lim_inferior | dados[i] > lim_superior){
        cont = cont+1
      }
    }
    
    resultado <- ((cont*100)/length(dados)) - 100
    
    return (resultado)
  }
  
  calculo_temporalidade <- function(valor_margem_erro, delay_registro) {
    resultado <- ((valor_margem_erro * 100) / mean(delay_registro)) - 100
    return (resultado)
  }
  
  calculo_reputacao <- function(dimensoes, pesos_dimensao){
    total <- dimensoes * pesos_dimensao
    return (sum(total)/sum(pesos_dimensao))
  }
  
  datasetInput <- reactive({
    if (is.null(input$arquivo)){
      return(NULL)
    }
    
    read_excel(input$arquivo$datapath)
  })
    
  calculos <- eventReactive(input$calcular, {
    
    df <- datasetInput()
    
    exatidao <- calculo_exatidao(mean(df$Temperatura), input$valor_real)
    precisao <- abs(calculo_precisao(sd(df$Temperatura), mean(df$Temperatura)))
    corretude <- abs(calculo_corretude(df$Temperatura))
    temporalidade <- abs(calculo_temporalidade(input$margem_erro, df$Delay_Registro))
    
    #vetor que vai armazenar os valores de cada dimensão
    dimensoes <- c(exatidao,precisao,temporalidade,corretude)
    
    #Dados que vão representar o data frame com o resultado dos cálculos
    dados <- data.frame(
      dimensao = c('Exatidão', 'Precisão', 'Temporalidade', 'Corretude'),
      valores = dimensoes
    )
    
    return (dados)
    
  })
  
  reputacao <- eventReactive(input$reputacao, {
    df <- calculos()
    
    #vetor que vai armazenar os valores de peso de cada dimensão
    pesos <- c(input$radio_exatidao,input$radio_precisao,input$radio_temporalidade,input$radio_corretude)
    
    #Busca os dados da coluna valores do data frame
    dimensoes <- df$valores
    
    return(calculo_reputacao(dimensoes, as.numeric(pesos)))
  })
  
  output$tabela <- renderDataTable({ 
    tryCatch(
      {
        datasetInput()
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  output$calculos <- renderTable({
    calculos()
  })
  
  output$reputacao <- renderUI({
    h3("A reputação do sensor é de ", round(reputacao(),digits = 2), "%")
  })
  
}

options(shiny.maxRequestSize = 60*1024^6)
shinyApp(ui = ui, server = server)