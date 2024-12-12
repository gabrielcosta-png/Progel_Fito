
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

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

server <- function(input, output) {

  dados <- eventReactive(input$process, {
    req(input$file)
    dados_importados <- read_excel(input$file$datapath)
    
    if (!all(c("id_planta", "Dap(médio)", "altura", "spp", "parc") %in% colnames(dados_importados))) {
      stop("Colunas 'id_planta', 'Dap(médio)', 'altura', 'spp' ou 'parc' não encontradas no arquivo.")
    }

    dados_consolidados <- dados_importados %>%
      group_by(id_planta, spp, parc) %>%
      summarise(
        `Dap(médio)` = sqrt(sum((`Dap(médio)`^2))), 
        altura = mean(altura, na.rm = TRUE),
        AB = pi * (`Dap(médio)`^2) / 40000,
        .groups = "drop"
      )

    dados_consolidados <- dados_consolidados %>%
      mutate(
        VT = AB * altura,
        VE = VT * input$fator_formula
      )
    
    return(dados_consolidados)
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
        AB = sum(AB),
        VT = sum(VT),
        VE = sum(VE),
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

}

shinyApp(ui = ui, server = server)
