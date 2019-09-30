library(shiny)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(stringr)
library(shinydashboard)
library(curl)

#DATA-------------

Por_Regiao <- jsonlite::fromJSON("http://api.sidra.ibge.gov.br/values/t/3653/n1/all/n3/all/v/3139,3140,4139/p/all/c544/129314,129315,129316/d/v3139%201,v3140%201,v4139%201")
colnames(Por_Regiao) <-  as.character(unlist(Por_Regiao[1,]))
Por_Regiao = Por_Regiao[-1, ]
Por_Regiao <- Por_Regiao %>% select("Brasil e Unidade da Federação (Código)", 
                                    "Brasil e Unidade da Federação", 
                                    "Variável (Código)", 
                                    "Variável", 
                                    "Mês (Código)", 
                                    "Mês", 
                                    "Seções e atividades industriais (CNAE 2.0) (Código)", 
                                    "Seções e atividades industriais (CNAE 2.0)", 
                                    "Unidade de Medida (Código)", 
                                    "Unidade de Medida", 
                                    "Valor")
colnames(Por_Regiao) <- c("UNID.DA.FEDER.COD", "UF", "VAR.COD", "VAR", "MES.COD", "DATA", 
                          "GRUP.COD", "GRUP.IND", "MED.COD", "MED", "VALOR")
Por_Regiao$VALOR <- as.numeric(Por_Regiao$VALOR)

Por_Regiao$MES.COD <- as.character(Por_Regiao$MES.COD)

Datas <- Por_Regiao$MES.COD
meses <- str_sub(Datas, 5, 6)
str_sub(Datas, 5) <- "-"
str_sub(Datas, 6) <- meses
str_sub(Datas, 8) <- "-01"
Datas <- as.Date(Datas, '%Y-%m-%d')
Por_Regiao$MES.COD <- Datas
names(Por_Regiao$MES.COD) <- Por_Regiao$DATA
rm(Datas, meses)

Por_Regiao$MANDATO <- ifelse(Por_Regiao$MES.COD < '2007-01-01', "Lula 1",
                             ifelse(Por_Regiao$MES.COD >= '2007-01-01' & Por_Regiao$MES.COD < '2011-01-01', "Lula 2",
                                    ifelse(Por_Regiao$MES.COD >= '2011-01-01' & Por_Regiao$MES.COD < '2015-01-01', "Dilma 1", 
                                           ifelse(Por_Regiao$MES.COD >= '2015-01-01' & Por_Regiao$MES.COD < '2016-08-01', "Dilma 2",
                                                  ifelse(Por_Regiao$MES.COD >= '2016-08-01' & Por_Regiao$MES.COD < '2019-01-01', "Temer", "Bolsonaro"
                                                  )))))


ui <- dashboardPage(
   dashboardHeader(title = "Indústria" 
   ),#dashHEADER
   
   dashboardSidebar(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
      menuItem("Explicação", tabName = "explicacao", icon = icon("th")),
      
      selectInput("var", "Variável", unique(Por_Regiao$VAR)),
      checkboxGroupInput("grupind", "Sessão", unique(Por_Regiao$GRUP.IND), 
                         selected = unique(Por_Regiao$GRUP.IND)),
      checkboxGroupInput("gestao", "Gestão", unique(Por_Regiao$MANDATO), 
                         selected = unique(Por_Regiao$MANDATO)),
      checkboxGroupInput("uf", "UF", unique(Por_Regiao$UF), 
                         selected = "Brasil")
      
   ),#dashSIDEBAR
   
   dashboardBody(
      tabItems(
         tabItem(tabName = "dashboard", 
                 fluidRow(
                    box(collapsible = TRUE, 
                        plotOutput("pieuf")),
                    box(collapsible = TRUE,
                        plotOutput("box"))
                 ),
                 fluidRow(
                    box(collapsible = TRUE,
                        plotOutput("piegrupind")),
                    box(collapsible = TRUE,
                        plotOutput("temp"))
                 )
         ),
         tabItem(tabName = "explicacao"
                 
         )
      )#tabITENS
      
   )
)#dashPAGE

server <- function(input, output) {
   
   output$box <- renderPlot({
      Por_Regiao %>% filter(UF == "Brasil",
                            VALOR != "NA", 
                            VAR == input$var, 
                            GRUP.IND == input$grupind, 
                            MANDATO == input$gestao) %>%
         ggplot(aes(x = MANDATO, y = VALOR)) + geom_boxplot() + 
         scale_x_discrete(limits = c("Lula 1", "Lula 2", "Dilma 1", "Dilma 2", 
                                     "Temer", "Bolsonaro")) +
         theme_minimal() + theme(axis.text.y = element_text(angle = 90))
      
   })
   
   output$piegrupind <- renderPlot({
      
      Por_Regiao2 <- Por_Regiao %>% filter(UF == "Brasil",
                                           VALOR != "NA", 
                                           VAR == input$var, 
                                           GRUP.IND != "1 Indústria geral", 
                                           MANDATO == input$gestao) %>%
         group_by(GRUP.IND) %>% summarise(n())
      
      names(Por_Regiao2) <- c("GRUP.IND2", "NUMERO")
      
      Por_Regiao2 %>% 
         ggplot(aes(x = "", y = NUMERO )) +
         geom_bar(aes(fill = GRUP.IND2), width = .3, stat = "identity") + 
         scale_fill_brewer(type = 'div', palette = "Set1") + 
         labs(title = "Por sessão") +
         theme_light() + theme(legend.position = "bottom") + coord_polar("y", start=0)
      
   })
   
   output$pieuf <- renderPlot({
      
      Por_Regiao3 <- Por_Regiao %>% filter(UF == input$uf,
                                           VALOR != "NA", 
                                           VAR == input$var, 
                                           GRUP.IND == input$grupind, 
                                           MANDATO == input$gestao) %>%
         group_by(UF) %>% summarise(n())
      
      names(Por_Regiao3) <- c("UF3", "NUMERO")
      
      Por_Regiao3 %>% 
         ggplot(aes(x = UF3, y = NUMERO )) +
         geom_bar(stat = "identity", fill = "darkblue", color = "white") + 
         labs(title = "Por sessão") +
         theme_light() + theme(axis.text.x = element_text(angle = 90))
      
   })
   
   output$temp <- renderPlot({ 
      Por_Regiao %>% filter(UF == "Brasil",
                            VALOR != "NA", 
                            VAR == input$var, 
                            GRUP.IND == input$grupind,
                            MANDATO == input$gestao) %>% 
         ggplot(aes(x = MES.COD, y = VALOR)) + geom_line(size = 1, color = "darkgray") + 
         geom_hline(yintercept = 0, color = 'black', size = 1, linetype = 3) +
         xlab("Tempo") +
         scale_x_date() + 
         theme(axis.text.x = element_text(angle = 90, size = 10)) + theme_light()
   })
   
   
   
}

shinyApp(ui = ui, server = server)
