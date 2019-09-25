library(shiny)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(stringr)
library(plotly)
#DATA-------------

#---
# Load Data:
#---

Por_grupo <- jsonlite::fromJSON("http://api.sidra.ibge.gov.br/values/t/3650/n1/all/v/3139,3140,3141/p/all/c542/all/d/v3139%201,v3140%201,v3141%201")
colnames(Por_grupo) <- as.character(unlist(Por_grupo[1,]))
Por_grupo = Por_grupo[-1, ]
colnames(Por_grupo) <- c("BRA.COD", "BRASIL", "VAR.COD", "VAR", "MES.COD", "DATA",
                         "GRUPOS.IND.COD", "GRUPOS.IND", "UNIDAD.VALOR", "UNIDAD.MEDIDA", "VALOR")

Por_categoria <- jsonlite::fromJSON("http://api.sidra.ibge.gov.br/values/t/3651/n1/all/v/3139,3140,3141,4139/p/all/c543/all/d/v3139%201,v3140%201,v3141%201,v4139%201")
colnames(Por_categoria) <-  as.character(unlist(Por_categoria[1,]))
Por_categoria = Por_categoria[-1, ]
colnames(Por_categoria) <- c("BRA.COD", "BRASIL", "VAR.COD", "VAR", "MES.COD", "DATA",
                             "GRUPOS.IND.COD", "GRUPOS.IND", "UNIDAD.VALOR", "UNIDAD.MEDIDA", "VALOR")

Categorias_especiais <- jsonlite::fromJSON("http://api.sidra.ibge.gov.br/values/t/6607/n1/all/v/3139,3140,3141/p/all/c25/all/d/v3139%201,v3140%201,v3141%201")
colnames(Categorias_especiais) <-  as.character(unlist(Categorias_especiais[1,]))
Categorias_especiais = Categorias_especiais[-1, ]
colnames(Categorias_especiais) <- c("BRA.COD", "BRASIL", "VAR.COD", "VAR", "MES.COD", "DATA",
                                    "GRUPOS.IND.COD", "GRUPOS.IND", "UNIDAD.VALOR", "UNIDAD.MEDIDA", "VALOR")
#---
# New Var:
#---  

Por_categoria$tab_name <- "Por categoria"
Por_grupo$tab_name <- "Por grupo"
Categorias_especiais$tab_name <- "Por categorias especiais"

#---
# Join data:
#---

INDUSTRIA <- rbind(Por_categoria, Por_grupo)
INDUSTRIA <- rbind(INDUSTRIA, Categorias_especiais)

rm(Categorias_especiais, Por_categoria, Por_grupo)

#---
# working values:
#---

INDUSTRIA$VALOR <- as.numeric(INDUSTRIA$VALOR)
INDUSTRIA$MES.COD <- as.character(INDUSTRIA$MES.COD)

Datas <- INDUSTRIA$MES.COD
meses <- str_sub(Datas, 5, 6)
str_sub(Datas, 5) <- "-"
str_sub(Datas, 6) <- meses
str_sub(Datas, 8) <- "-01"
Datas <- as.Date(Datas, '%Y-%m-%d')
INDUSTRIA$MES.COD <- Datas
names(INDUSTRIA$MES.COD) <- INDUSTRIA$DATA
rm(Datas, meses)

INDUSTRIA$MANDATO <- ifelse(INDUSTRIA$MES.COD < '2007-01-01', "Lula 1",
                            ifelse(INDUSTRIA$MES.COD >= '2007-01-01' & INDUSTRIA$MES.COD < '2011-01-01', "Lula 2",
                                   ifelse(INDUSTRIA$MES.COD >= '2011-01-01' & INDUSTRIA$MES.COD < '2015-01-01', "Dilma 1", 
                                          ifelse(INDUSTRIA$MES.COD >= '2015-01-01' & INDUSTRIA$MES.COD < '2016-08-01', "Dilma 2",
                                                 ifelse(INDUSTRIA$MES.COD >= '2016-08-01' & INDUSTRIA$MES.COD < '2019-01-01', "Temer", "Bolsonaro"
                                                 )))))

Por_Regiao <- jsonlite::fromJSON("http://api.sidra.ibge.gov.br/values/t/3653/n3/all/v/3139,3140,3141,4139/p/last%201/c544/all/d/v3139%201,v3140%201,v3141%201,v4139%201")
colnames(Por_Regiao) <-  as.character(unlist(Por_Regiao[1,]))
Por_Regiao = Por_Regiao[-1, ]
colnames(Por_Regiao) <- c("UNID.DA.FEDER.COD", "UF", "VAR.COD", "VAR", "MES.COD", "DATA", "GRUP.COD", "GRUP.IND", "MED.COD", "MED", "VALOR")
Por_Regiao$VALOR <- as.numeric(Por_Regiao$VALOR)
Por_Regiao$NIVEL <- readr::parse_number(Por_Regiao$GRUP.IND)
Por_Regiao <- Por_Regiao %>% 
   mutate(NIVEL, 
          ifelse(str_count(as.character(NIVEL)) == 1, "Área", 
                 ifelse(str_count(as.character(NIVEL)) == 3, "Sessão", "Atividade")))

#-APP--------------

ui <- navbarPage("Industria brasileira",
   
  
   tabPanel("Histórico",
   sidebarLayout(
      sidebarPanel("Escolha as variáveis",
         selectInput("var",
                     "Tipo de cálculo", 
                    choices =   unique(INDUSTRIA$VAR)),
         selectInput("ativ",
                     "Atividade industrial", 
                    choices = unique(INDUSTRIA$GRUPOS.IND))
      ),
      
      mainPanel(
         textOutput("tx1"),
         plotOutput("IND2"),
         plotOutput("IND1")
      
         )
   )
   ),
   tabPanel("Por estado e sessão", sidebarLayout( sidebarPanel(
            selectInput("cat", "Escolha a categoria", choices = unique(Por_Regiao$`ifelse(...)`))
            ),
            
            mainPanel(
               plotOutput("IND3"), 
               plotOutput("IND4")
               
            ))
      
   )
   
)


server <- function(input, output) {
  

  
   output$IND1 <- renderText({a <- "laala"
   a
   })
   output$IND1 <- renderPlot({
      INDUSTRIA %>% filter(VALOR != "NA", VAR == input$var, GRUPOS.IND == input$ativ) %>%
         ggplot(aes(x = MANDATO, y = VALOR)) + geom_boxplot() + 
         scale_x_discrete(limits = c("Lula 1", "Lula 2", "Dilma 1", "Dilma 2", 
                                     "Temer", "Bolsonaro")) +
         theme_minimal() + theme(axis.text.y = element_text(angle = 90))
      
   })
   
   output$IND2 <- renderPlot({ 
 INDUSTRIA %>% filter(VALOR != "NA", 
                      VAR == input$var, 
                      GRUPOS.IND == input$ativ) %>% 
     ggplot(aes(x = MES.COD, y = VALOR)) + geom_line( 
                                                       size = 1) + 
       geom_hline(yintercept = 0, color = 'black', size = 1, linetype = 3) +
       xlab("Tempo") +
       scale_x_date() + 
       theme(axis.text.x = element_text(angle = 90, size = 10)) + theme_light()
   
      
      
   })
   output$IND3 <- renderPlot({ a <-Por_Regiao %>% filter(`ifelse(...)` == input$cat) %>%
         ggplot(aes(x = "", y = VALOR, fill = UF)) + geom_col()
   
   })
   output$IND4 <- renderPlot({ b <- Por_Regiao %>% filter(`ifelse(...)` == input$cat) %>%
         ggplot(aes(x = "", y = VALOR, fill = GRUP.IND)) + 
         geom_bar(width = 1, stat = "identity")
   pie2 <- b + coord_polar("y", start=0)
   pie2
   })
   
}


shinyApp(ui = ui, server = server)

