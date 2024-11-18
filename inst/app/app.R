library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)  # Para usar a função fct_reorder

# Definir a interface do usuário
ui <- fluidPage(
  titlePanel("Inventário Florestal"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar arquivo Excel", accept = ".xlsx"),
      numericInput("area_total_input", "Área Total (ha)", value = 1, min = 0.01),
      numericInput("area_ua", "Tamanho da unidade amostral (m²)", value = 250, min = 1),
      numericInput("fator_formula", "Fator de Fórmula", value = 0.6, min = 0),
      actionButton("process", "Processar Dados"),
      downloadButton("downloadTable", "Baixar Tabela (.csv)"),
      downloadButton("downloadPlotCollectorCurve", "Baixar Curva Coletora (.png)"),
      selectInput("collectorCurveType", "Tipo de Curva Coletora:", 
                  choices = c("Número de Indivíduos" = "individuos", "Volume por Parcela" = "volume"),
                  selected = "individuos")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dados Importados", tableOutput("dataView")),
        tabPanel("Tabela por Espécie", tableOutput("tabelaFito")),
        tabPanel("Tabela por Área", tableOutput("tabelaArea")),
        tabPanel("Tabela por Classe Diamétrica", tableOutput("tabelaClasse")),
        tabPanel("Tabela de Variáveis", tableOutput("tabelaVariaveisUI")),
        tabPanel("Curva Coletora", plotOutput("plotCollectorCurve")),
        tabPanel("Número de Indivíduos", plotOutput("plotNumIndividuos")), 
        tabPanel("DR, FR e DoR", plotOutput("plotDRFRDoR")),
        tabPanel("Tabela de Produtos por Classe Diamétrica", tableOutput("tabelaProduto"))
      )
    )
  )
)

# Definir o servidor
server <- function(input, output) {
  
  dados <- eventReactive(input$process, {
    req(input$file)
    dados_importados <- read_excel(input$file$datapath)
    
    if (!all(c("Dap(médio)", "altura", "spp", "parc") %in% colnames(dados_importados))) {
      stop("Colunas 'Dap(médio)', 'altura', 'spp' ou 'parc' não encontradas no arquivo.")
    }
    
    dados_importados$AB <- pi * (dados_importados$`Dap(médio)`^2) / 40000
    dados_importados$VT <- dados_importados$AB * dados_importados$altura
    dados_importados$VE <- dados_importados$VT * input$fator_formula
    
    return(dados_importados)
  })
  
  output$dataView <- renderTable({
    req(dados())
  })
  
  tabela_fito <- eventReactive(input$process, {
    req(dados())
    dados_mod <- dados()
    
    N <- table(dados_mod$spp)
    
    area_ua <- input$area_ua
    n_ua <- length(unique(dados_mod$parc))
    A <- area_ua * n_ua
    
    DA <- N / (A / 10000)
    DoA <- tapply(dados_mod$VT, dados_mod$spp, sum) / (A / 10000)
    
    tabela_fito <- data.frame(
      spp = names(N),
      N = as.vector(N),
      AB = format(tapply(dados_mod$AB, dados_mod$spp, sum), scientific = FALSE),
      VT = format(tapply(dados_mod$VT, dados_mod$spp, sum), scientific = FALSE),
      VE = format(tapply(dados_mod$VE, dados_mod$spp, sum), scientific = FALSE),
      DA = format(as.vector(DA), scientific = FALSE),
      DoA = format(as.vector(DoA), scientific = FALSE)
    )
    
    return(tabela_fito)
  })
  
  output$plotDRFRDoR <- renderPlot({
    req(tabela_fito())
    
    tabela_fito_df <- tabela_fito() %>%
      mutate(
        DR = as.numeric(DA) / sum(as.numeric(DA)) * 100,
        FR = as.numeric(N) / sum(as.numeric(N)) * 100,
        DoR = as.numeric(DoA) / sum(as.numeric(DoA)) * 100
      ) %>%
      select(spp, DR, FR, DoR) %>%
      pivot_longer(cols = c(DR, FR, DoR), names_to = "par", values_to = "val")
    
    ggplot(tabela_fito_df, aes(x = fct_reorder(spp, -val), y = val, fill = par)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      scale_fill_brewer(name = 'Parâmetro', palette = 'Set2') +
      labs(title = 'Valor de Importância de Espécies na Área de Estudo') +
      theme_minimal()
  })
  
  output$plotNumIndividuos <- renderPlot({
    req(dados())
    dados_mod <- dados()
    
    individuos_por_especie <- dados_mod %>%
      group_by(spp) %>%
      summarise(num_individuos = n())
    
    ggplot(individuos_por_especie, aes(x = fct_reorder(spp, num_individuos), y = num_individuos)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Espécie", y = "Número de Indivíduos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$tabelaFito <- renderTable({
    tabela_fito()
  }, digits = 10)
  
  output$tabelaArea <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    area_total <- length(unique(dados_mod$parc)) * input$area_ua
    
    tabela_area <- dados_mod %>%
      summarise(
        Area_Total_Parcelas = area_total,
        N = n(),
        AB = sum(pi * (`Dap(médio)`^2) / 40000),
        VT = sum(pi * (`Dap(médio)`^2) / 40000 * altura),
        VE = sum(VT * input$fator_formula),
        DA = N / (area_total / 10000),
        DoA = sum(VT) / (area_total / 10000),
        VTha = VT / (area_total / 10000),
        VEha = VE / (area_total / 10000)
      ) %>%
      arrange(desc(N))
    
    options(scipen = 999)
    tabela_area
  }, digits = 10)
  
  output$tabelaClasse <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    dados_mod$classe <- cut(dados_mod$`Dap(médio)`, 
                            breaks = c(-Inf, 6, 12, 30, Inf), 
                            labels = c("0 |- 6 Lenha", "6 |- 12 Estaca", "12 |- 30 Mourão", ">30 Madeira para Serraria"),
                            right = FALSE)
    
    tabela_classe <- dados_mod %>%
      group_by(classe) %>%
      summarise(
        N = n(),
        AB = sum(AB),
        VT = sum(VT)
      )
    
    tabela_classe
  }, digits = 10)
  
  tabelaVariaveis <- eventReactive(input$process, {
    req(dados())
    
    dados_mod <- dados()
    area_total <- input$area_total_input
    n_parcelas <- length(unique(dados_mod$parc))
    volume_medio_parcela <- sum(dados_mod$VT, na.rm = TRUE) / n_parcelas
    volume_medio_ha <- volume_medio_parcela * (10000 / input$area_ua)
    
    desvio_padrao <- sd(dados_mod$VT, na.rm = TRUE)
    variancia <- var(dados_mod$VT, na.rm = TRUE)
    erro_padrao_media <- desvio_padrao / sqrt(n_parcelas)
    coeficiente_variacao <- (desvio_padrao / volume_medio_parcela) * 100
    
    t_tabelado <- qt(0.95, df = n_parcelas - 1)
    erro_amostragem_absoluto <- t_tabelado * erro_padrao_media
    erro_amostragem_relativo <- (erro_amostragem_absoluto / volume_medio_parcela) * 100
    ic_media <- c(volume_medio_ha - erro_amostragem_absoluto, volume_medio_ha + erro_amostragem_absoluto)
    
    tabela_variaveis <- data.frame(
      Variável = c("Área Total", "Nº Parcelas Amostradas", "Volume Médio por Hectare", 
                   "Volume Médio por Parcela Amostrada", "Desvio Padrão", 
                   "Variância", "Erro Padrão da Média", "Coeficiente de Variação", 
                   "Valor de t Tabelado", "Erro de Amostragem Absoluto", 
                   "Erro de Amostragem Relativo", "IC para a Média (90%)", 
                   "Total da População"),
      Resultado = c(area_total, n_parcelas, volume_medio_ha, 
                    volume_medio_parcela, desvio_padrao, variancia, 
                    erro_padrao_media, coeficiente_variacao, 
                    t_tabelado, erro_amostragem_absoluto, 
                    erro_amostragem_relativo, paste(round(ic_media[1], 2), "-", round(ic_media[2], 2)),
                    n_parcelas * volume_medio_ha)
    )
    
    tabela_variaveis
  })
  
  output$plotCollectorCurve <- renderPlot({
    req(dados())
    dados_mod <- dados()
    
    # Selecionar o tipo de curva coletora
    if (input$collectorCurveType == "individuos") {
      n_values <- cumsum(table(dados_mod$spp))
      y_values <- n_values
      y_label <- "Número de Indivíduos Amostrados"
    } else {
      # Calcular o volume por parcela
      volume_por_parcela <- dados_mod %>%
        group_by(parc) %>%
        summarise(VT = sum(pi * (`Dap(médio)`^2) / 40000 * altura))  # Total de volume por parcela
      y_values <- cumsum(volume_por_parcela$VT)  # Usar a variável VT do resumo
      y_label <- "Volume Total Amostrado (m³)"
    }
    
    n_parcelas <- seq_along(y_values)
    
    ggplot(data.frame(n_parcelas, y_values), aes(x = n_parcelas, y = y_values)) +
      geom_line() +
      geom_point() +
      labs(x = "Número de Parcelas Amostradas", y = y_label) +
      theme_minimal()
  })
  
  output$tabelaVariaveisUI <- renderTable({
    tabelaVariaveis()
  })
  
  output$tabelaProduto <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    dados_mod <- dados_mod %>%
      mutate(
        Produto_Gerado = case_when(
          `Dap(médio)` < 6 ~ "Classe diamétrica 0 |- 6 (Lenha)",
          `Dap(médio)` >= 6 & `Dap(médio)` < 12 ~ "Classe diamétrica 6 |- 12 (Estaca)",
          `Dap(médio)` >= 12 & `Dap(médio)` < 30 ~ "Classe diamétrica 12 |- 30 (Mourão)",
          `Dap(médio)` >= 30 ~ "Classe diamétrica > 30 (Madeira para Serraria)"
        )
      )
    
    if(!"Nome_Popular" %in% colnames(dados_mod)) {
      dados_mod$Nome_Popular <- "Desconhecido"
    }
    
    tabela_produto <- dados_mod %>%
      group_by(Produto_Gerado, spp, Nome_Popular) %>%
      summarise(
        Unidade = n(),
        Volume = sum(VT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Produto_Gerado, spp)
    
    tabela_produto <- tabela_produto %>%
      rename(
        `Produto Gerado` = Produto_Gerado,
        `Nome Científico` = spp,
        `Nome Popular` = Nome_Popular
      )
    
    tabela_produto
  }, digits = 10)
}



# Rodar o aplicativo Shiny
shinyApp(ui = ui, server = server)

