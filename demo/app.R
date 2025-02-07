library(collapse)
library(shiny)
library(bslib)
library(rlang)
library(plotly)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Cargar base de datos ----------------------------------------------------
rutaData <- "./data-raw/data_customer_experience_demo.xlsx"

# Leer bases --------------------------------------------------------------
wb <- openxlsx2::wb_load(rutaData)
names_sheet_dataRaw <- openxlsx2::wb_get_sheet_names(wb)

## Se cargan las bases por medio de las hojas del xlsx
list_sheets_dataRaw <- names_sheet_dataRaw  |>  set_names() |>  
  purrr::map(~ openxlsx2::read_xlsx(rutaData, sheet = .x) |>
               tibble::as_tibble() |> janitor::clean_names())

dataMetricas <- list_sheets_dataRaw$Métricas

## Vectores de aplicación
vect_drv <- c("Nacional",funique(dataMetricas$id_region))

## Cálculos por DRV y Subregion y por mes

calcular_metricas_drv <- function(data = NULL, 
                                  var_id = NULL, 
                                  nacional = F){
  
  if(!is.null(data)){
    
    if(nacional == T){
      data_nivel <- data |> dplyr::group_by(fecha)
    }else{
      data_nivel <- data |> dplyr::group_by({{var_id}},fecha)
    }
    
    data_sum <- data_nivel |> 
      fsummarise(across(promotores_pct:tasa_retencion,list(mu = fmean))) |> 
      fmutate(nps = promotores_pct - detractores_pct) |> 
      fungroup()
  }
  
  return(data_sum)
}

data_nacional <- calcular_metricas_drv(data = dataMetricas,nacional = T) |> fmutate(id_region = "Nacional")
data_regiones <- calcular_metricas_drv(data = dataMetricas,var_id = id_region)
data_subregiones <- calcular_metricas_drv(data = dataMetricas,var_id = id_subregion)

## Data para visualizaciones
data_gen <- rowbind(data_nacional,data_regiones)

## Valores de última fecha de levantamiento
list_data_gen <- split(data_gen,data_gen$id_region)

list_data_gen_ultimo_valor <- lapply(list_data_gen,function(x){
  
  x |> fsubset(fecha == fmax(x$fecha))

})

data_ultimo_valor <- rowbind(list_data_gen_ultimo_valor) |> 
  fselect(-c(promotores_pct:pasivos_pct)) 

# ui ----------------------------------------------------------------------

## Tema 
theme <- bs_theme(
  bg = "#003049", fg = "white",
  "input-border-color" = "white"
)

vbx <- list(
  
  value_box(
    title = tags$p("NPS", style = "font-size: 200%;font-weight: bold;"),
    value = uiOutput("value_nps"),
    showcase = plotlyOutput("graf_nps"),
    showcase_layout = "bottom"
  ),
  
  value_box(
    title = tags$p("CSAT", style = "font-size: 200%;font-weight: bold;"),
    value = uiOutput("value_csat"),
    showcase = plotlyOutput("graf_csat"),
    showcase_layout = "bottom"
  ),
  
  value_box(
    title = tags$p("CES", style = "font-size: 200%;font-weight: bold;"),
    value = uiOutput("value_ces"),
    showcase = plotlyOutput("graf_ces"),
    showcase_layout = "bottom"
  )
  
)

## UI
ui <- page_fillable(
  
  theme = theme,
  tags$style(HTML("
    .card {
      background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
      border: none; /* Opcional: quitar bordes */
      box-shadow: none; /* Opcional: quitar sombra */
    }
  ")),
  
  tags$style(HTML("
  .bslib-value-box {
    width: 450px !important;  /* Ajusta el ancho */
    height: 200px !important; /* Ajusta la altura */
    font-size: 1000px !important; /* Ajusta el tamaño del texto */
    padding: 5px !important; /* Ajusta el espacio interno */
    color: white !important;
  }
")),
  
  tags$style(HTML("
  .form-control, .selectize-input {
    background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
    border: none !important; /* Sin bordes */
    box-shadow: none !important; /* Sin sombra */
    color: white !important; /* Texto negro */
  }
")),
  
  layout_columns(
    col_widths = c(10, 2), 
    justify = "space-between", 
    align = "left",
    tags$h2("Indicadores de experiencia del cliente"),
    selectInput(inputId = "vect_drv", choices = vect_drv, label = "Selecciona tu región:", selected = "Nacional")
  ),
  
  layout_columns(
    col_widths = c(9,9),
    row_heights = c(1, 1, 1),
    vbx[[1]], vbx[[2]],
    vbx[[3]]
  )
)

## Server
server <- function(input, output){
  
  ## Sección de data reactiva
  
  ### Valor actual de componentes
  data_componentes <- reactive({
    
    data_ultimo_valor |> fsubset(id_region == input$vect_drv)
    
  })
  
  ## Data de evolución de componentes
  data_evo_componetes <- reactive({
    data_gen |> fsubset(id_region == input$vect_drv)
  })
  
  
  ## Creación de objetos
  
  ### Valor actual de componentes
  output$value_nps <- renderText({
    round(funique(data_componentes()$nps),2)
    })
  
  output$value_csat <- renderText({
   round(funique(data_componentes()$csat),2)
  })

  output$value_ces <- renderText({
    round(funique(data_componentes()$ces),2)
  })
  
  ### Gráficos de evolución de componentes
  
  output$graf_nps <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,nps)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~nps,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2
      ) %>%
      layout(
        xaxis = list(title = F,visible = T, showgrid = FALSE),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  output$graf_csat <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,csat)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~csat,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2
      ) %>%
      layout(
        xaxis = list(title = F,visible = T, showgrid = FALSE),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  output$graf_ces <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,ces)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~ces,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2
      ) |> 
      layout(
        xaxis = list(title = F,visible = TRUE, showgrid = FALSE),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
}

shinyApp(ui,server)
