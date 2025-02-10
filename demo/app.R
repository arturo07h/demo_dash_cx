library(collapse)
library(shiny)
library(bslib)
library(rlang)
library(plotly)
library(echarts4r)

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
ui <- fluidPage(
  
  theme = theme,
  
  ## Estilo de values box
  tags$style(HTML("
    .card {
      background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
      border: none; /* Opcional: quitar bordes */
      box-shadow: none; /* Opcional: quitar sombra */
    }
  ")),
  
  ## Título y filtro regional
  fluidRow(
    column(
      9,
      tags$h2("Indicadores de experiencia del cliente"),
    ),
    column(
      3,
      selectInput(inputId = "vect_drv", choices = vect_drv, label = "Selecciona tu región:", selected = "Nacional")
    )
  ),
  
  ## Columns de values box
  fluidRow(
    column(
      4 ,
      value_box(
        title = tags$p("NPS", style = "font-size: 200%;font-weight: bold;"),
        value = uiOutput("value_nps"),
        showcase = plotlyOutput("graf_nps"),
        showcase_layout = "bottom"
      )
    ),
    column(
      4,
      value_box(
        title = tags$p("CSAT", style = "font-size: 200%;font-weight: bold;"),
        value = uiOutput("value_csat"),
        showcase = plotlyOutput("graf_csat"),
        showcase_layout = "bottom"
      )
    ),
    column(
      4,
      value_box(
        title = tags$p("CES", style = "font-size: 200%;font-weight: bold;"),
        value = uiOutput("value_ces"),
        showcase = plotlyOutput("graf_ces"),
        showcase_layout = "bottom"
      )
    )
  ),
  fluidRow(
    column(
      4,
      div(
        style = "
        background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
        border: none; /* Opcional: quitar bordes */
        box-shadow: none; /* Opcional: quitar sombra */
        border-radius: 15px; /* Esquinas redondeadas */
        padding: 10px; /* Opcional: espacio interno */
         height: 430px;
        ",
        echarts4rOutput("graf_nps_comp", height = "400px", width = "450px")
      )
    ),
    column(
      4,
      div(
        # class = "small-graph", 
        style = "
        background-color: rgba(255, 255, 255, 0.5) !important; /* Fondo semitransparente */
        border: none; /* Opcional: quitar bordes */
        box-shadow: none; /* Opcional: quitar sombra */
        border-radius: 15px; /* Esquinas redondeadas */
        padding: 10px; /* Opcional: espacio interno */
        height: 430px;
        ",
        echarts4rOutput("graf_evo_fcr", height = "240px", width = "450px"),
        echarts4rOutput("graf_fcr_gauge", height = "250px", width = "450px")
      )
    ),
    column(
      4,
      div(
        style = "
        display: flex; 
        flex-direction: column; 
        align-items: left; 
        height: 450px;
        width: 400;
        ",
        value_box(
          title = tags$p("Tiempo promedio de respuesta", style = "font-size: 120%;font-weight: bold;"),
          value = uiOutput("value_tiempo_resp"),
          showcase = plotlyOutput("graf_tiempo_res"),
          showcase_layout = showcase_bottom(height = "100px")
        ),
        value_box(
          title = tags$p("Tasa de retención", style = "font-size: 120%;font-weight: bold;"),
          value = uiOutput("value_tasa_ret"),
          showcase = plotlyOutput("graf_tasa_ret"),
          showcase_layout = showcase_bottom(height = "100px")
        )
      )
    )
  )
)

## Server
server <- function(input, output){
  
  ## Sección de data reactiva
  
  ### Valor actual de componentes
  data_componentes <- reactive({
    
    data_ultimo_valor |> fsubset(id_region == input$vect_drv)
    
  })
  
  ### Data de evolución de componentes
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
  
  output$value_tiempo_resp <- renderText({
    round(funique(data_componentes()$tiempo_respuesta),2)
  })
  
  output$value_tasa_ret <- renderText({
    round(funique(data_componentes()$tasa_retencion),2)
  })
  
  ### Gráficos de evolución de componentes
  
  ## NPS
  output$graf_nps <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,nps)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~nps,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>NPS :", round(data$nps,2))
      ) |>
      layout(
        xaxis = list(title = F,visible = T, showgrid = FALSE, color = "white"),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(displayModeBar = F)
  })
  
  ## CSAT
  output$graf_csat <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,csat)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~csat,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CSAT :", round(data$csat,2))
      ) |>
      layout(
        xaxis = list(title = F,visible = T, showgrid = FALSE, color = "white"),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(displayModeBar = F)
  })
  
  ## CES
  output$graf_ces <- renderPlotly({
    
    data <- data_evo_componetes() |> fselect(fecha,ces)
    
    plot_ly(data, height = 100) |>
      add_lines(
        x = ~fecha,
        y = ~ces,
        color = I("#c1121f"),
        fill = "tozeroy",
        alpha = 0.2,
        textposition = "auto",
        hoverinfo = "text",
        hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                          "<br>CES :", round(data$ces,2))
      ) |> 
      layout(
        xaxis = list(title = F,visible = TRUE, showgrid = FALSE, color = "white"),
        yaxis = list(visible = FALSE, showgrid = FALSE),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(displayModeBar = F)
  })
  
  ## Evolución Tiempo promedio de respuesta
  output$graf_tiempo_res <- renderEcharts4r({
    
    data <- data_evo_componetes() |> fselect(fecha,tiempo_respuesta)
    
    fecha_max <- max(data$fecha)
    fecha_min <- fecha_max - months(13)
    
    plot_ly(x = data$fecha, 
            y = data$tiempo_respuesta,
            type = "bar",
            marker = list(color = "#73d2de"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                              "<br>TPR :", round(data$tiempo_respuesta,2))) |> 
      layout(
        yaxis = list(
          color = "white",
          showticklabels = FALSE
        ),
        xaxis = list(
          color = "white",
          range = c(fecha_min, fecha_max),
          rangeselector = list(
            buttons = list(
              list(count = 6, label = "6 meses", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 año", step = "year", stepmode = "backward"),
              list(step = "all", label = "Todo")
            )
          ),
          rangeslider = list(visible = F, thickness = .1)
        ),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(locale = "es") 
  })
  
  ## Evolución de tasa de retención
  output$graf_tasa_ret <- renderEcharts4r({
    
    data <- data_evo_componetes() |> fselect(fecha,tasa_retencion)
    
    fecha_max <- max(data$fecha)
    fecha_min <- fecha_max - months(13)
    
    plot_ly(x = data$fecha, 
            y = data$tasa_retencion,
            type = "bar",
            marker = list(color = "#02c39a"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = paste("Fecha :", format(data$fecha,"%B %Y"),
                              "<br>TPR :", round(data$tasa_retencion,2))) |> 
      layout(
        yaxis = list(
          color = "white",
          showticklabels = FALSE
        ),
        xaxis = list(
          color = "white",
          range = c(fecha_min, fecha_max),
          rangeselector = list(
            buttons = list(
              list(count = 6, label = "6 meses", step = "month", stepmode = "backward"),
              list(count = 1, label = "1 año", step = "year", stepmode = "backward"),
              list(step = "all", label = "Todo")
            )
          ),
          rangeslider = list(visible = F, thickness = .1)
        ),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |> 
      config(locale = "es") 
  })
  
  ### Componentes NPS
  output$graf_nps_comp <- renderEcharts4r({
    
    data <- data_evo_componetes() |> 
      fselect(fecha,promotores_pct,detractores_pct,pasivos_pct) |> 
      frename(Promotores = promotores_pct,
              Detractores = detractores_pct,
              Pasivos = pasivos_pct)
    
    data |> 
      e_chart(x = fecha) |> 
      
      e_bar(serie = Detractores, stack = "grp", color = "#f95738") |> 
      e_bar(serie = Pasivos, stack = "grp", color = "#ffa62b") |> 
      e_bar(serie = Promotores, stack = "grp", color = "#2ec4b6") |> 
      
      e_title("Evolución de componentes NPS",
              textStyle = list(color = "white",fontSize = 20)) |> 
      e_tooltip(trigger = "axis") |> 
      e_legend(orient = "horizontal", 
               left = "center", 
               top = "bottom",
               textStyle = list(color = "white",fontSize = 16)) |> 
      e_x_axis(axisLabel = list(color = "white",fontSize = 16)) |> 
      e_y_axis(axisLabel = list(color = "white",fontSize = 16), max = 100 ) |> 
      
      e_datazoom(
        type = "slider",
        startValue = "2024-01-01",
        bottom = "8%"  
      ) |> 
      e_grid(
        bottom = "20%"  
      )
    
  })
  
  ## Gráfico gauge FCR
  output$graf_fcr_gauge <- renderEcharts4r({
    
    value_fil <- data_evo_componetes() |> fsubset(fecha == fmax(fecha))
    value <- funique(value_fil$fcr)
    
    e_charts() |> 
      e_gauge(
        round(value, 1),  # Valor del gauge
        name = round(value, 1),
        radius = "100%",
        startAngle = 180,
        endAngle = 0,
        itemStyle = list(color = "#a7c957"),
        axisLine = list(
          lineStyle = list(
            color = list(c(0.33, "#f95738"), c(0.67, "#c77dff"), c(1, "#2ec4b6")),
            width = 10
          )
        ),
        axisTick = list(lineStyle = list(width = 2, color = "white", fontSize = 16)),
        axisLabel = list(
          show = TRUE,
          color = "white",
          fontWeight = "bold",
          borderRadius = 5,
          fontSize = 15
        ),
        pointer = list(show = TRUE, icon = "triangle", length = "100%"),
        itemStyle = list(color = "white"),
        detail = list(
          show = TRUE,
          color = "white",
          fontSize = 0,
          fontWeight = "bold"
        ),
        title = list(
          show = TRUE,
          color = "white", 
          textStyle = list(
            color = "white",  
            fontSize = 22,    
            fontWeight = "bolder"
          )
        )
      ) |> 
      e_title("Enero 2025",
              textStyle = list(color = "white",fontSize = 16))
  })
  
  ### Gráfico de evolución de FCR
  output$graf_evo_fcr <- renderEcharts4r({

    data_evo_componetes() |> 
      fselect(fecha,fcr) |> 
      e_charts(x = fecha) |> 
      e_area(serie = fcr, symbol = "none", color = "red") |> 
      e_title("Resolución en el primer contacto (FCR)",
              textStyle = list(color = "white",fontSize = 20)) |> 
      e_legend(show = F) |> 
      e_x_axis(axisLabel = list(color = "white",fontSize = 14)) |> 
      e_y_axis(axisLabel = list(color = "white",fontSize = 14), max = 100)
    
  })
}

shinyApp(ui,server)