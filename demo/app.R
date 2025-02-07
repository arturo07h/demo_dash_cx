library(collapse)
library(shiny)
library(bslib)
library(rlang)

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

data_nacional <- calcular_metricas_drv(data = dataMetricas,nacional = T) |> 
  fmutate(id_region = "Nacional")
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
  fselect(-c(promotores_pct:pasivos_pct)) #|> 
  #pivot(values = c(2:7),names = list("var","value"),how = "longer")

# ui ----------------------------------------------------------------------

## Tema 
theme <- bs_theme(
  bg = "#a3b18a", fg = "gray",
  "input-border-color" = "white"
)

## Cards de objetos en el housting
cards <- list(
  
  card(
    tags$h2("NPS"),
    value_box(
      title = "Enero 2025",
      value = uiOutput("value_nps"),
      height = 
    )
  ),
  card(
    tags$h2("Aquí va CES")
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
  
  # layout_columns(
  #   col_widths = c(7,8),
  #   row_heights = c(1,1),
  #   tags$h2("Indicadores de experiencia del cliente"),
  # ),
  tags$h2("Indicadores de experiencia del cliente"),
  selectInput(inputId = "vect_drv",choices = vect_drv,label = "",selected = "Nacional"),
  
  layout_columns(
    col_widths = c(3),
    row_heights = c(1,1),
    cards[[1]]
  ),
  cards[[2]]
  
)

## Server
server <- function(input, output){
  
  ## Sección de data reactiva
  data_componentes <- reactive({
    
    data_ultimo_valor |> fsubset(id_region == input$vect_drv)
    
  })
  
  ## Creación de objetos
  
  output$value_nps <- renderText({
    
    funique(data_componentes()$nps)
    
    })
  
}

shinyApp(ui,server)


