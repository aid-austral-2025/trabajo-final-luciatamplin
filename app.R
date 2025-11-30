library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(DT)
library(corrplot)

datos <- read_csv("datos.csv")

datos <- datos[, -ncol(datos)] %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(occupation = as.factor(occupation)) %>%
  mutate(work_mode = as.factor(work_mode)) %>%
  mutate(sleep_quality_1_5 = factor(sleep_quality_1_5,
                                    levels = c("1", "2", "3", "4", "5"),
                                    ordered = TRUE)) %>%
  mutate(gender = case_when(
    gender == "Female" ~ "Femenino",
    gender == "Male" ~ "Masculino",
    gender == "Non-binary/Other" ~ "No binario/Otro",
    TRUE ~ gender)) %>%
  mutate(occupation = case_when(
    occupation == "Employed" ~ "Empleado",
    occupation == "Student" ~ "Estudiante",
    occupation == "Self-employed" ~ "Autónomo",
    occupation == "Retired" ~ "Jubilado",
    occupation == "Unemployed" ~ "Desempleado",
    TRUE ~ occupation)) %>%
  mutate(work_mode = case_when(
    work_mode == "Hybrid" ~ "Híbrido",
    work_mode == "In-person" ~ "Presencial",
    work_mode == "Remote" ~ "Remoto",
    TRUE ~ work_mode)) %>%
  mutate(
    gender = as.factor(gender),
    work_mode = as.factor(work_mode),
    occupation = as.factor(occupation)
  )

ui <- dashboardPage(
  
  dashboardHeader(title = "AID: Trabajo Práctico"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Perfil de la muestra", tabName = "perfil", icon = icon("users")),
      menuItem("Horas de pantalla", tabName = "digital", icon = icon("mobile-screen")),
      menuItem("Sueño y Bienestar físico", tabName = "salud", icon = icon("bed")),
      menuItem("Productividad y Salud mental", tabName = "prod", icon = icon("brain"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "perfil",
              fluidRow(
                box(
                  width = 12,
                  title = "Características generales de la muestra",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_ocupacion")),
                    box(width = 6, plotlyOutput("plot_genero"))
                  ),
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_workmode")),
                    box(width = 6, plotlyOutput("plot_edad"))
                  ),
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_mode_occupation")),
                    box(width = 6, plotlyOutput("plot_age_vs_ocu"))
                  )
                )
              )
      ),
      
      tabItem(tabName = "digital",
              fluidRow(
                box(
                  width = 3,
                  title = "Filtros",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  checkboxGroupInput("filtro_genero_d", "Género",
                                     choices = levels(datos$gender),
                                     selected = levels(datos$gender)),
                  
                  checkboxGroupInput("filtro_workmode_d", "Modalidad de trabajo",
                                     choices = levels(datos$work_mode),
                                     selected = levels(datos$work_mode)),
                  
                  checkboxGroupInput("filtro_ocupacion_d", "Ocupación",
                                     choices = levels(datos$occupation),
                                     selected = levels(datos$occupation))
                ),
                
                box(
                  width = 9,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_screen_total")),
                    box(width = 6, plotlyOutput("plot_screen_lab"))
                  ),
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_screen_leisure")),
                    box(width = 6, plotlyOutput("plot_screen_vs_mode"))
                  )
                )
              )
      ),
      
      tabItem(tabName = "salud",
              fluidRow(
                box(
                  width = 3,
                  title = "Filtros",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  checkboxGroupInput("filtro_genero_s", "Género",
                                     choices = levels(datos$gender),
                                     selected = levels(datos$gender)),
                  
                  checkboxGroupInput("filtro_workmode_s", "Modalidad de trabajo",
                                     choices = levels(datos$work_mode),
                                     selected = levels(datos$work_mode)),
                  
                  checkboxGroupInput("filtro_ocupacion_s", "Ocupación",
                                     choices = levels(datos$occupation),
                                     selected = levels(datos$occupation))
                ),
                
                box(
                  width = 9,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_sleep_hours")),
                    box(width = 6, plotlyOutput("plot_sleep_quality"))
                  ),
                  fluidRow(
                    box(width = 6, plotlyOutput("plot_stress")),
                    box(width = 6, plotlyOutput("plot_ejercicio"))
                  )
                )
              )
      ),
      
      tabItem(tabName = "prod",
              fluidRow(
                box(width = 6, plotlyOutput("plot_productividad")),
                box(width = 6, plotlyOutput("plot_saludmental"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("plot_prod_vs_stress")),
                box(width = 6, plotlyOutput("plot_salud_vs_screen"))
              )
      )
    )
  )
) 


server <- function(input, output) {
  
  datos_filtrados <- reactive({
    
    datos %>%
      filter(
        if (length(input$filtro_genero) > 0) gender %in% input$filtro_genero else TRUE,
        if (length(input$filtro_workmode) > 0) work_mode %in% input$filtro_workmode else TRUE,
        if (length(input$filtro_ocupacion) > 0) occupation %in% input$filtro_ocupacion else TRUE
      )
  })
  
  output$plot_genero <- renderPlotly({
    datos_contados <- datos_filtrados() %>%
      count(gender, name = "Frecuencia")
    
    ggplotly(ggplot(datos_contados, aes(x = gender, y = Frecuencia)) +
               geom_col(fill = "#F7A94B", color = "#F17A22",
                        aes(text = paste0("<b>Género:</b> ", gender,
                                          "<br><b>Cantidad:</b> ", Frecuencia))
               ) +
               labs(title = "Género de los individuos", x = "Género", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")), 
             tooltip = "text")
  })
  
  output$plot_ocupacion <- renderPlotly({
    datos_contados <- datos_filtrados() %>%
      count(occupation, name = "Frecuencia")
    
    ggplotly(ggplot(datos_contados, aes(x = occupation, y = Frecuencia)) +
               geom_col(fill = "#81B274", color = "#2A873D",
                        aes(text = paste0("<b>Ocupación:</b> ", occupation,
                                          "<br><b>Cantidad:</b> ", Frecuencia))
               ) +
               labs(title = "Ocupación de los individuos", x = "Ocupación", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")),
             tooltip = "text")
  })
  
  output$plot_workmode <- renderPlotly({
    datos_contados <- datos_filtrados() %>%
      count(work_mode, name = "Frecuencia")
    
    ggplotly(
      ggplot(datos_contados, aes(x = work_mode, y = Frecuencia)) +
        geom_col(fill = "#7AC5CD", color = "#53868B",
                 aes(text = paste0("<b>Modalidad:</b> ", work_mode,
                                   "<br><b>Cantidad:</b> ", Frecuencia))
        ) +
        labs(title = "Modalidad de trabajo", x = "Modalidad", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")),
      tooltip = "text")
  })
  
  output$plot_edad <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = age)) +
               geom_histogram(bins = 8, fill = "#EED2EE", color = "#CDB5CD") +
               labs(title = "Edad de los individuos", x = "Edad", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_mode_occupation <- renderPlotly({
    ggplotly(ggplot(datos) + 
               geom_bar(position = "dodge") +
               aes(x = occupation, fill = work_mode) +
               scale_fill_brewer(palette = "Dark2") +
               labs(title = "Cantidad de individuos según ocupación \ny modalidad de trabajo",
                    x = "Ocupación", 
                    y = "Cantidad de individuos", 
                    fill = "Modalidad de Trabajo") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.margin = margin(t = 30))) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.1,
          title = list(
            text = "<b>Modalidad de Trabajo:</b>"
          )
        ),
        margin = list(t = 120,
                      l = 80)
      )
  })
  
  output$plot_age_vs_ocu <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = occupation, y = age)) +
        geom_boxplot(
          fill = "#EEDC82",
          color = "#8B814C",
          outlier.colour = "#8B814C",
          outlier.stroke = 0.8
        ) +
        labs(
          title = "Edad de los individuos según \nla ocupación",
          x = "Ocupación",
          y = "Edad de los individuos"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 30)
        ),
      tooltip = "none"
    ) %>%
      layout(
        margin = list(l = 80),
        xaxis = list(title = list(standoff = 20))
      )
  })
  
  datos_filtrados2 <- reactive({
    
    datos %>%
      filter(
        if (length(input$filtro_genero_d) > 0) gender %in% input$filtro_genero_d else TRUE,
        if (length(input$filtro_workmode_d) > 0) work_mode %in% input$filtro_workmode_d else TRUE,
        if (length(input$filtro_ocupacion_d) > 0) occupation %in% input$filtro_ocupacion_d else TRUE
      )
  })
  
  output$plot_screen_total <- renderPlotly({
    ggplotly(ggplot(datos_filtrados2(), aes(x = screen_time_hours)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Horas de pantalla (totales)", x = "Horas", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_screen_lab <- renderPlotly({
    ggplotly(ggplot(datos_filtrados2(), aes(x = work_screen_hours)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Horas de pantalla laboral", x = "Horas", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_screen_leisure <- renderPlotly({
    ggplotly(ggplot(datos_filtrados2(), aes(x = leisure_screen_hours)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Horas de pantalla recreativa", x = "Horas", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_screen_vs_mode <- renderPlotly({
    ggplotly(ggplot(datos_filtrados2(), aes(x = work_mode, y = screen_time_hours)) +
               geom_boxplot(fill = "#2E608B") +
               labs(title = "Horas de pantalla según \nla modalidad de trabajo", 
                    x = "Modalidad de trabajo", 
                    y = "Horas de pantalla") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.margin = margin(t = 30)))
  })
  
  output$plot_sleep_hours <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = sleep_hours)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Horas de sueño", x = "Horas de sueño", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_sleep_quality <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = sleep_quality_1_5)) +
               geom_bar(fill = "#2E608B", color = "#26456E") +
               labs(title = "Calidad de sueño", x = "Calidad del sueño (1-5)", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_stress <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = stress_level_0_10)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Nivel de estrés", x = "Nivel de Estrés (1-10)", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_ejercicio <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = exercise_minutes_per_week)) +
               geom_histogram(bins = 10, fill = "#2E608B", color = "#26456E") +
               labs(title = "Minutos de ejercicio por semana", 
                    x = "Minutos de ejercicio", 
                    y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_productividad <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = productivity_0_100)) +
               geom_histogram(bins = 10, fill = "#FFDEAD", color = "#EECFA1") +
               labs(title = "Productividad", x = "Nivel de Productividad (0-100)", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_saludmental <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = mental_wellness_index_0_100)) +
               geom_histogram(bins = 10, fill = "#EEA2AD", color = "#CD8C95") +
               labs(title = "Salud mental", x = "Índice de Salud Mental (0-100)", y = "Frecuencia") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_prod_vs_stress <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = stress_level_0_10, y = productivity_0_100)) +
               geom_point(alpha = 0.4) +
               geom_smooth(method = "lm", color = "#FFDEAD") +
               labs(title = "Productividad vs Nivel de Estrés", 
                    x = "Nivel de estrés (0-10)", 
                    y = "Productividad (0-100)") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
  output$plot_salud_vs_screen <- renderPlotly({
    ggplotly(ggplot(datos, aes(x = stress_level_0_10, y = mental_wellness_index_0_100)) +
               geom_point(alpha = 0.4) +
               geom_smooth(method = "lm", color = "#EEA2AD") +
               labs(title = "Salud mental vs Nivel de Estrés", 
                    x = "Nivel de estrés (0-10)",
                    y = "Índice de Salud mental (0-100)") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold")))
  })
  
}

shinyApp(ui, server)
