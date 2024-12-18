library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(vegan)

ui <- fluidPage(
  titlePanel("Inventário Florestal"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar arquivo Excel", accept = ".xlsx"),
      numericInput("area_total_input", "Área Total (ha)", value = 1, min = 0.01),
      numericInput("area_ua", "Tamanho da unidade amostral (m²)", value = 250, min = 1),
      numericInput("fator_formula", "Fator de Fórmula", value = 0.6, min = 0),
      numericInput("fator_empilhamento", "Fator de empilhamento ", value = 3,41, min = 0),
      actionButton("process", "Processar Dados"),
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
        tabPanel("Tabela de Produtos por Classe Diamétrica", tableOutput("tabelaProduto")),
        tabPanel("Tabela de Variáveis", tableOutput("tabelaVariaveisUI")),
        tabPanel("Tabela Fitossociológicos", tableOutput("tabelaFitossociologica")),
        tabPanel("Índices de Diversidade", tableOutput("tabelaDiversidade")),
        tabPanel("Tabela de supressão", tableOutput("tabelasupressao")),
        tabPanel("Curva Coletora", plotOutput("plotCollectorCurve")),
        tabPanel("Número de Indivíduos", plotOutput("plotNumIndividuos")),
        tabPanel("DR, FR e DoR", plotOutput("plotDRFRDoR")),
        
        
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
    dados_importados <- dados_importados %>%
      mutate(parc = as.factor(parc))  
    
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
        VE = VT * input$fator_formula * input$fator_empilhamento
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
    DoA <- tapply(dados_mod$AB, dados_mod$spp, sum) / (A / 10000)
    
    # Criar a tabela original com valores numéricos
    tabela_fito <- data.frame(
      spp = names(N),
      N = as.vector(N),
      AB = as.numeric(tapply(dados_mod$AB, dados_mod$spp, sum)),
      VT = as.numeric(tapply(dados_mod$VT, dados_mod$spp, sum)),
      VT.HE = as.numeric(tapply(dados_mod$VT, dados_mod$spp, sum)) / A * 10000,
      VE = as.numeric(tapply(dados_mod$VE, dados_mod$spp, sum)),
      VE.HE = as.numeric(tapply(dados_mod$VE, dados_mod$spp, sum)) / A * 10000,
      DA = as.numeric(DA),
      DoA = as.numeric(DoA)
    )
    
    # Criar a linha de totais com valores numéricos
    total_row <- data.frame(
      spp = "Total",
      N = sum(tabela_fito$N, na.rm = TRUE),
      AB = sum(tabela_fito$AB, na.rm = TRUE),
      VT = sum(tabela_fito$VT, na.rm = TRUE),
      VT.HE = sum(tabela_fito$VT.HE, na.rm = TRUE),
      VE = sum(tabela_fito$VE, na.rm = TRUE),
      VE.HE = sum(tabela_fito$VE.HE, na.rm = TRUE),
      DA = sum(tabela_fito$DA, na.rm = TRUE),
      DoA = sum(tabela_fito$DoA, na.rm = TRUE)
    )
    
    # Combinar a tabela original com a linha de totais
    tabela_fito <- bind_rows(tabela_fito, total_row)
    
    # Formatar apenas para exibição
    tabela_fito <- tabela_fito %>%
      mutate(
        AB = format(as.numeric(AB), scientific = FALSE),
        VT = format(as.numeric(VT), scientific = FALSE),
        VT.HE = format(as.numeric(VT.HE), scientific = FALSE),
        VE = format(as.numeric(VE), scientific = FALSE),
        VE.HE = format(as.numeric(VE.HE), scientific = FALSE),
        DA = format(as.numeric(DA), scientific = FALSE),
        DoA = format(as.numeric(DoA), scientific = FALSE)
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
  output$tabelaFito <- renderTable({
    tabela_fito()
  }, digits = 10)
  
  output$tabelaArea <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    # Calcula a área total das parcelas
    area_total <- length(unique(dados_mod$parc)) * input$area_ua
    
    # Calcula valores por parcela
    tabela_area <- dados_mod %>%
      group_by(parc) %>%
      summarise(
        Area_Parcela = input$area_ua,
        N = n(),
        AB = sum(AB, na.rm = TRUE),
        VT = sum(VT, na.rm = TRUE),
        VE = sum(VE, na.rm = TRUE),
        DA = N / (input$area_ua / 10000),
        DoA = sum(AB, na.rm = TRUE) / (input$area_ua / 10000),
        VTha = sum(VT, na.rm = TRUE) / (input$area_ua / 10000),
        VEha = sum(VE, na.rm = TRUE) / (input$area_ua / 10000)
      ) %>%
      mutate(parc = as.character(parc)) %>% # Converte `parc` para texto
      arrange(as.numeric(parc)) # Ordena parcelas por ordem crescente
    
    # Calcula os totais
    total_row <- tabela_area %>%
      summarise(
        parc = "Total", # Nome da parcela para os totais
        Area_Parcela = area_total,
        N = sum(N, na.rm = TRUE),
        AB = sum(AB, na.rm = TRUE),
        VT = sum(VT, na.rm = TRUE),
        VE = sum(VE, na.rm = TRUE),
        DA = sum(N, na.rm = TRUE) / (area_total / 10000),
        DoA = sum(AB, na.rm = TRUE) / (area_total / 10000),
        VTha = sum(VT, na.rm = TRUE) / (area_total / 10000),
        VEha = sum(VE, na.rm = TRUE) / (area_total / 10000)
      )
    
    # Combina os dados das parcelas com a linha de total
    tabela_area <- bind_rows(tabela_area, total_row)
    
    # Define as opções para exibição
    options(scipen = 999)
    tabela_area
  }, digits = 10)
  
  output$tabelaClasse <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    # Cria as classes de diâmetro
    dados_mod$classe <- cut(dados_mod$`Dap(médio)`,
                            breaks = c(-Inf, 6, 12, 30, Inf),
                            labels = c("0 |- 6 Lenha", "6 |- 12 Estaca", "12 |- 30 Mourão", ">30 Madeira para Serraria"),
                            right = FALSE)
    
    # Calcula os valores por classe
    tabela_classe <- dados_mod %>%
      group_by(classe) %>%
      summarise(
        N = n(),
        AB = sum(AB, na.rm = TRUE),
        VT = sum(VT, na.rm = TRUE),
        VE = sum(VE, na.rm = TRUE),
        DA = N / (input$area_ua / 10000),
        DoA = sum(VT, na.rm = TRUE) / (input$area_ua / 10000),
        VTha = sum(VT, na.rm = TRUE) / (input$area_ua / 10000),
        VEha = sum(VE, na.rm = TRUE) / (input$area_ua / 10000)
      ) %>%
      arrange(classe) # Ordena as classes
    
    # Calcula os totais
    total_row <- tabela_classe %>%
      summarise(
        classe = "Total",
        N = sum(N, na.rm = TRUE),
        AB = sum(AB, na.rm = TRUE),
        VT = sum(VT, na.rm = TRUE),
        VE = sum(VE, na.rm = TRUE),
        DA = sum(N, na.rm = TRUE) / (length(unique(dados_mod$parc)) * input$area_ua / 10000),
        DoA = sum(AB, na.rm = TRUE) / (length(unique(dados_mod$parc)) * input$area_ua / 10000),
        VTha = sum(VT, na.rm = TRUE) / (length(unique(dados_mod$parc)) * input$area_ua / 10000),
        VEha = sum(VE, na.rm = TRUE) / (length(unique(dados_mod$parc)) * input$area_ua / 10000)
      )
    
    # Combina os dados por classe com a linha de total
    tabela_classe <- bind_rows(tabela_classe, total_row)
    
    # Define as opções para exibição
    options(scipen = 999)
    tabela_classe
  }, digits = 10)
  
  output$tabelaProduto <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    # Classifica os produtos gerados com base no Dap(médio)
    dados_mod <- dados_mod %>%
      mutate(
        Produto_Gerado = case_when(
          `Dap(médio)` < 6 ~ "Classe diamétrica 0 |- 6 (Lenha)",
          `Dap(médio)` >= 6 & `Dap(médio)` < 12 ~ "Classe diamétrica 6 |- 12 (Estaca)",
          `Dap(médio)` >= 12 & `Dap(médio)` < 30 ~ "Classe diamétrica 12 |- 30 (Mourão)",
          `Dap(médio)` >= 30 ~ "Classe diamétrica > 30 (Madeira para Serraria)"
        )
      )
    
    # Caso a coluna 'Nome_Popular' não exista, não a adicionamos
    if(!"Nome_Popular" %in% colnames(dados_mod)) {
      dados_mod$Nome_Popular <- NULL
    }
    
    # Criação da tabela de produto
    tabela_produto <- dados_mod %>%
      group_by(Produto_Gerado, spp) %>%
      summarise(
        Unidade = n(),
        Volume = sum(VT, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Produto_Gerado, spp)
    
    # Renomeia as colunas para o formato desejado
    tabela_produto <- tabela_produto %>%
      rename(
        `Produto Gerado` = Produto_Gerado,
        `Nome Científico` = spp
      )
    
    # Exibe a tabela
    tabela_produto
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
  
  output$tabelaFitossociologica <- renderTable({
    req(dados())
    dados_mod <- dados()
    
    # Contagem dos indivíduos por espécie
    N <- table(dados_mod$spp)
    
    # Área basal por espécie
    AB <- tapply(dados_mod$AB, dados_mod$spp, sum)
    
    # Densidade absoluta (DA)
    area_ua <- input$area_ua
    n_ua <- length(unique(dados_mod$parc))
    A <- area_ua * n_ua
    DA <- N / (A / 10000)
    
    # Frequência absoluta (FA) - parcelas com indivíduos de cada espécie
    N <- table(dados_mod$spp)
    ut <- length(unique(dados_mod$parc))
    FA <- sapply(names(N), function(spp) {
      ui <- length(unique(dados_mod$parc[dados_mod$spp == spp]))  # Número de unidades amostrais em que a espécie ocorre
      (ui / ut * 100)  
    })
    
    # Frequência relativa (FR)
    total_FA <- sum(FA)
    FR <- (FA / total_FA) * 100
    
    # Dominância absoluta (DoA)
    DoA <- AB / (A / 10000)
    
    # Dominância relativa (DoR)
    DoR <- DoA / sum(DoA) * 100
    
    # Valor de cobertura (VC) = DR + DoR
    DR <- DA / sum(DA) * 100
    VC <- DR + DoR
    
    # Valor de importância (VI) = DR + FR + DoR (ou DA, FR, DoR)
    VI <- DR + FR + DoR
    
    # Calculando porcentagens de VC e VI
    VC_percent <- (VC / sum(VC)) * 100
    VI_percent <- (VI / sum(VI)) * 100
    
    # Montando a tabela com todos os parâmetros
    tabela_fitossociologica <- data.frame(
      `Espécie` = names(N),
      `N` = format(N, scientific = FALSE),
      `AB` = format(AB, scientific = FALSE),
      `DA` = format(DA, scientific = FALSE),
      `DR` = format(DR, scientific = FALSE),
      `FA` = format(FA, scientific = FALSE),
      `FR` = format(FR, scientific = FALSE),
      `DoA` = format(DoA, scientific = FALSE),
      `DoR` = format(DoR, scientific = FALSE),
      `VC` = format(VC, scientific = FALSE),
      `VI` = format(VI, scientific = FALSE),
      `VC%` = format(VC_percent, scientific = FALSE),
      `VI%` = format(VI_percent, scientific = FALSE)
    )
    
    # Calculando a linha final (totais)
    linha_final <- data.frame(
      `Espécie` = "Total",
      `N` = sum(N),
      `AB` = sum(AB),
      `DA` = sum(DA),
      `DR` = sum(DR),
      `FA` = sum(FA),
      `FR` = sum(FR),
      `DoA` = sum(DoA),
      `DoR` = sum(DoR),
      `VC` = sum(VC),
      `VC%` = 100, 
      `VI` = sum(VI),
      `VI%` = 100  
    )
    
    # Combinando a tabela com a linha final
    tabela_fitossociologica <- rbind(tabela_fitossociologica, linha_final)
    
    # Exibe a tabela com os parâmetros fitossociológicos e a linha final
    options(scipen = 999)
    tabela_fitossociologica
  }, digits = 10)
  
  tabela_diversidade <- eventReactive(input$process, {
    req(dados())
    dados_mod <- dados()
    
    # Calcular número de indivíduos e espécies por parcela
    diversidade_por_parcela <- dados_mod %>%
      group_by(parc) %>%
      summarise(
        N = n(),  # Número de indivíduos
        S = n_distinct(spp),  # Número de espécies
        H_prime = diversity(table(spp), index = "shannon"),  # Índice de Shannon
        C = diversity(table(spp), index = "simpson"),  # Índice de Simpson
        J = H_prime / log(S),  # Equabilidade de Pielou
        OM = 1 - (C / (1 - (1 / S))),  # Coeficiente de Mistura de Jentsch
        Ln_S = log(S),  # Logaritmo do número de espécies
        .groups = "drop"
      )
    
    # Adicionar uma linha de totais ou médias
    total_row <- data.frame(
      parc = "Total",  # Aqui, "Total" será um character
      N = sum(diversidade_por_parcela$N),
      S = n_distinct(dados_mod$spp),
      H_prime = mean(diversidade_por_parcela$H_prime, na.rm = TRUE),
      C = mean(diversidade_por_parcela$C, na.rm = TRUE),
      J = mean(diversidade_por_parcela$J, na.rm = TRUE),
      OM = mean(diversidade_por_parcela$OM, na.rm = TRUE),
      Ln_S = mean(diversidade_por_parcela$Ln_S, na.rm = TRUE)
    )
    
    # Garantir que a coluna 'parc' seja do tipo character antes de combinar
    diversidade_por_parcela$parc <- as.character(diversidade_por_parcela$parc)
    total_row$parc <- as.character(total_row$parc)
    
    diversidade_por_parcela <- bind_rows(diversidade_por_parcela, total_row)
    
    return(diversidade_por_parcela)
  })
  
  output$tabelaDiversidade <- renderTable({
    tabela_diversidade()
  }, digits = 4)
  output$tabelasupressao <- renderTable({
    req(dados())
    
    # Obtém os dados consolidados
    dados_mod <- dados()
    tabela_fito_reactive <- tabela_fito()
    
    # Calcula os valores necessários
    AVS <- input$area_total_input
    F <- as.numeric(tabela_fito_reactive %>% filter(spp == "Total") %>% pull(DA))
    VE <- as.numeric(tabela_fito_reactive %>% filter(spp == "Total") %>% pull(VE.HE))
    VS <- round(AVS * F)
    EFS <- (AVS * VE) / (input$area_ua / 10000)
    EFS_M3 <- EFS / input$fator_empilhamento
    
    # Cria um data frame com os resultados
    tabela_nova <- data.frame(
      AVS = AVS,
      F <- F,
      VE = VE,
      VS = VS,
      EFS = EFS,
      EFS_M3 = EFS_M3
    )
    
    return(tabela_nova)
  })
}
shinyApp(ui = ui, server = server)
