require(tidyverse)
require(lubridate)
require(jsonlite)
require(DT)
require(shiny)
require(shinythemes)
require(memoise)

# lista de empresas
lista_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA",
                    "BSANTANDER", "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM", "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A", "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER", "ANDINA-B", "SONDA", "CAP", "ILC", "SALFACORP",
                    "SECURITY", "VAPORES", "ENELGXCH", "ANTARCHILE", "BANMEDICA",
                    "EMBONOR-B", "FORUS", "IAM", "MASISA", "ORO BLANCO", "SK", "SMSAAM")

#funcion para obtener las variables

obtener_indicadores <- memoise(function(empresa) {
  url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=",
                        empresa, "&time=10&indicador=2")

  df <- jsonlite::read_json(URLencode(url))$Data %>%
    stringr::str_split(";") %>%
    dplyr::first() %>%
    I() %>%
    readr::read_delim(delim = ",",
                      col_names = c("fecha", "precio", "vol"),
                      show_col_types = FALSE)

  df <- df %>%
    mutate(
      fecha = lubridate::ymd_hms(fecha),
      anio = lubridate::year(fecha)
    )

  df
})


# generation de shiny


# UI ----------------------------------------------------------------------


# Define UI for application 
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel(h1(strong("Valor de Acciones de Empresas"), align = "center"), windowTitle = "Indicadores empresa"),

    # Sidebar para seleccion de empresas 
    sidebarLayout(
        sidebarPanel(
                      # Sidebar para seleccion de empresas
                      selectInput(inputId ="var1", 
                                  label = "Seleccionar empresa:", 
                                  choices = sort(lista_empresas),
                                  selected = "FALABELLA"),
            
                      
                      # Sseleccionar año dinamico
                      uiOutput(outputId = "año")
                    ),
          
          
        mainPanel(
                    tabsetPanel(
                      tabPanel(title = "Gráfico", plotOutput("grafico_linea")),
                      tabPanel(h4(strong("Valor Anual Promedio de las Acciones"), align = "center"),
                               title = "Tabla", DT::dataTableOutput(outputId="tabla", width = "95%"))
                                )
                 )
        )    

) # Close UI
    


# Server ------------------------------------------------------------------

# Define server logic required to draw a charts and table
server <- function(input, output) {
  
    d <- reactive(obtener_indicadores(empresa = input$var1))

    # generar panle range dinamico
    output$año <- renderUI({
      sliderInput(inputId="año2", 
                  label="Seleccionar año", 
                  min = min(year(d()$fecha)),
                  max = max(year(d()$fecha)),
                  value = c(min(year(d()$fecha)),
                            max(year(d()$fecha))),
                  sep="",

                  )
                          })
    
    # geenrar filtros para grafico y trabla

    f_filter<- reactive({
              if(is.null(input$año2)) {
                  return(NULL)
                }

              d() %>%
                dplyr::filter(between(year(fecha), input$año2[1], input$año2[2]))
                      })

    table_f <- reactive({
               if(is.null(input$año2)) {
                return(NULL)
               }
                          
              f_filter() %>%
                        dplyr::group_by(Año = anio) %>%
                        dplyr::summarise(Valor = mean(precio))
                          
                      })


    # generar grafico de linea segun empresas
    output$grafico_linea <- renderPlot({
                                if(is.null(f_filter())) {
                                  return()
                                }
                                  ggplot(f_filter()) +
                                  geom_line(aes(fecha, precio)) +
                                  scale_x_datetime(limits = c(min(f_filter()$fecha),
                                                              max(f_filter()$fecha)+years(1)),
                                                   date_labels = "%Y",
                                                   breaks = seq(min(f_filter()$fecha),
                                                                max(f_filter()$fecha)+years(1), "2 year"),
                                                   expand = c(0,0)
                                                   ) +
                                  labs(x = "AÑOS",
                                       y = "VALORES (En pesos chilenos)",
                                       title = input$var1) +
                                  theme_bw() + # theme_classic() 
                                  theme(plot.title = element_text(size=rel(1.5),
                                                                  vjust=0.5, hjust=0.5, face="bold"),
                                        axis.title.x = element_text(face="bold",
                                                                    vjust=-1.5, hjust=0.5, size=rel(1.1)),
                                        axis.title.y = element_text(face="bold",
                                                                    vjust=2.5, hjust=0.5, size=rel(1.1))
                                        )
                            })

        # Generar tabla
    output$tabla <- renderDataTable({DT::datatable(table_f(),
                                          rownames = FALSE,
                                          colnames = c("Valor*" = "Valor"),
                                          caption = htmltools::tags$caption(
                                            style = "caption-side: bottom; text-align: left",
                                            "* Valores en pesos chilenos.")
                                          ) %>%
                                          formatCurrency("Valor*", digits = 2)
                      })


} # Close server



# Run ---------------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
