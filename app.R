library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(DT)
library(corrplot)
library(shinyWidgets)

datos <- read_csv("datos.csv")

datos <- datos[, -ncol(datos)] %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(occupation = as.factor(occupation)) %>%
  mutate(work_mode = as.factor(work_mode)) %>%
  mutate(sleep_quality_1_5 = factor(
    sleep_quality_1_5,
    levels = c("1", "2", "3", "4", "5"),
    ordered = TRUE
  )) %>%
  mutate(
    gender = case_when(
      gender == "Female" ~ "Femenino",
      gender == "Male" ~ "Masculino",
      gender == "Non-binary/Other" ~ "No binario/Otro",
      TRUE ~ gender
    )
  ) %>%
  mutate(
    occupation = case_when(
      occupation == "Employed" ~ "Empleado",
      occupation == "Student" ~ "Estudiante",
      occupation == "Self-employed" ~ "Autónomo",
      occupation == "Retired" ~ "Jubilado",
      occupation == "Unemployed" ~ "Desempleado",
      TRUE ~ occupation
    )
  ) %>%
  mutate(
    work_mode = case_when(
      work_mode == "Hybrid" ~ "Híbrido",
      work_mode == "In-person" ~ "Presencial",
      work_mode == "Remote" ~ "Remoto",
      TRUE ~ work_mode
    )
  ) %>%
  mutate(
    gender = as.factor(gender),
    work_mode = as.factor(work_mode),
    occupation = as.factor(occupation)
  )

ui <- dashboardPage(
  dashboardHeader(title = "AID: Trabajo Práctico"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Perfil de la muestra",
      tabName = "perfil",
      icon = icon("users")
    ),
    menuItem(
      "Horas de pantalla",
      tabName = "digital",
      icon = icon("mobile-screen")
    ),
    menuItem(
      "Sueño",
      tabName = "sueño",
      icon = icon("bed")
    ),
    menuItem(
      "Bienestar físico y mental",
      tabName = "salud",
      icon = icon("stethoscope")
    )
  )),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "perfil", fluidRow(
      box(
        width = 12,
        title = "Características generales de la muestra",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(box(width = 6, plotlyOutput("plot_ocupacion")), 
                 box(width = 6, plotlyOutput("plot_genero"))),
        fluidRow(box(width = 6, plotlyOutput("plot_workmode")), 
                 box(width = 6, plotlyOutput("plot_edad"))),
        fluidRow(box(
          width = 6, plotlyOutput("plot_mode_occupation")
        ), box(width = 6, plotlyOutput("plot_age_vs_ocu")))
      )
    )),
    
    tabItem(tabName = "digital",
      fluidRow(
        box(
          width = 3,
          title = "Filtros",
          status = "primary",
          solidHeader = TRUE,
          
          sliderInput(
            inputId = "filtro_edad_d",
            label = "Edad",
            min = 16,
            max = 60,
            value = c(16, 60)
          ),
          
          checkboxGroupInput(
            inputId = "filtro_genero_d",
            label = "Género",
            choices = levels(datos$gender),
            selected = levels(datos$gender)
          ),
          
          checkboxGroupInput(
            inputId = "filtro_workmode_d",
            label = "Modalidad de trabajo",
            choices = levels(datos$work_mode),
            selected = levels(datos$work_mode)
          ),
          
          checkboxGroupInput(
            inputId = "filtro_ocupacion_d",
            label = "Ocupación",
            choices = levels(datos$occupation),
            selected = levels(datos$occupation)
          )
        ),
        
        box(
          width = 9,
          status = "primary",
          solidHeader = TRUE,
          
          div(
            style = "text-align:center; font-weight:bold; font-size:18px; color:#2E608B",
            radioGroupButtons(
              inputId = "filtro_tipo_horas",
              label = "Tipo de horas de pantalla:",
              choices = c(
                "Totales" = "screen_time_hours",
                "Laborales" = "work_screen_hours",
                "Recreativas" = "leisure_screen_hours"
              ),
              selected = "screen_time_hours",
              justified = TRUE,
              status = "primary",
              checkIcon = list(
                yes = icon("check")
              )
            )
          ),
        
          fluidRow(
            box(
              width = 6,
              status = NULL,
              solidHeader = TRUE,
              plotlyOutput("plot_screen_hist")
            ),
            box(
              width = 6,
              status = NULL,
              solidHeader = TRUE,
              plotlyOutput("plot_screen_vs_mode")
            ),
            box(
              width = 6,
              status = NULL,
              solidHeader = TRUE,
              plotlyOutput("plot_screen_vs_social")
            ),
            box(
              width = 6,
              status = NULL,
              solidHeader = TRUE,
              plotlyOutput("plot_screen_vs_prod")
              )
          )
        )
      )
    ),
    
    tabItem(tabName = "sueño", fluidRow(
      box(
        width = 3,
        title = "Filtros",
        status = "primary",
        solidHeader = TRUE,
        
        sliderInput(
          inputId = "filtro_edad_s",
          label = "Edad",
          min = 16,
          max = 60,
          value = c(16, 60)
        ),
        
        checkboxGroupInput(
          "filtro_genero_s",
          "Género",
          choices = levels(datos$gender),
          selected = levels(datos$gender)
        ),
        
        checkboxGroupInput(
          "filtro_workmode_s",
          "Modalidad de trabajo",
          choices = levels(datos$work_mode),
          selected = levels(datos$work_mode)
        ),
        
        checkboxGroupInput(
          "filtro_ocupacion_s",
          "Ocupación",
          choices = levels(datos$occupation),
          selected = levels(datos$occupation)
        )
      ),
      
      box(
        width = 9,
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(box(
          width = 6, plotlyOutput("plot_sleep_hours")
        ), box(
          width = 6, plotlyOutput("plot_sleep_quality")
        )),
        fluidRow(box(width = 6, plotlyOutput("plot_stress")),
                 box(width = 6, plotlyOutput("plot_calsue_vs_stress"))),
        
        fluidRow(box(width = 12, plotlyOutput("plot_sue_vs_stress")))
        )
      )),
    
    tabItem(tabName = "salud", fluidRow(
      box(
        width = 3,
        title = "Filtros",
        status = "primary",
        solidHeader = TRUE,
        
        sliderInput(
          inputId = "filtro_edad_p",
          label = "Edad",
          min = 16,
          max = 60,
          value = c(16, 60)
        ),
        
        checkboxGroupInput(
          "filtro_genero_p",
          "Género",
          choices = levels(datos$gender),
          selected = levels(datos$gender)
        ),
        
        checkboxGroupInput(
          "filtro_workmode_p",
          "Modalidad de trabajo",
          choices = levels(datos$work_mode),
          selected = levels(datos$work_mode)
        ),
        
        checkboxGroupInput(
          "filtro_ocupacion_p",
          "Ocupación",
          choices = levels(datos$occupation),
          selected = levels(datos$occupation)
        )
      ),
      
      box(
        width = 9,
        status = "primary",
        solidHeader = TRUE,
        fluidRow(box(
          width = 6, plotlyOutput("plot_productividad")
        ), box(
          width = 6, plotlyOutput("plot_saludmental")
        )),
        fluidRow(box(
          width = 6, plotlyOutput("plot_prod_vs_stress")
        ), box(
          width = 6, plotlyOutput("plot_salud_vs_screen")
        )),
        fluidRow(box(
          width = 6, plotlyOutput("plot_ejercicio")),
          box(width = 6, plotlyOutput("plot_social"))),
        fluidRow(box(
          width = 6, plotlyOutput("plot_prod_vs_ejercicio")
        ),
        box(width = 6, plotlyOutput("plot_salud_vs_social")))
        ))
      )
    ))
  )


server <- function(input, output) {
  
  # Perfil de la muestra
  
  output$plot_genero <- renderPlotly({
    datos_contados <- datos %>%
      count(gender, name = "Frecuencia")
    
    ggplotly(
      ggplot(datos_contados, aes(x = gender, y = Frecuencia)) +
        geom_col(
          fill = "#F7A94B",
          color = "#F17A22",
          aes(
            text = paste0(
              "<b>Género:</b> ",
              gender,
              "<br><b>Cantidad:</b> ",
              Frecuencia
            )
          )
        ) +
        labs(title = "Género de los individuos", x = "Género", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")),
      tooltip = "text"
    )
  })
  
  output$plot_ocupacion <- renderPlotly({
    datos_contados <- datos %>%
      count(occupation, name = "Frecuencia")
    
    ggplotly(
      ggplot(datos_contados, aes(x = occupation, y = Frecuencia)) +
        geom_col(
          fill = "#81B274",
          color = "#2A873D",
          aes(
            text = paste0(
              "<b>Ocupación:</b> ",
              occupation,
              "<br><b>Cantidad:</b> ",
              Frecuencia
            )
          )
        ) +
        labs(title = "Ocupación de los individuos", x = "Ocupación", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")),
      tooltip = "text"
    )
  })
  
  output$plot_workmode <- renderPlotly({
    datos_contados <- datos %>%
      count(work_mode, name = "Frecuencia")
    
    ggplotly(
      ggplot(datos_contados, aes(x = work_mode, y = Frecuencia)) +
        geom_col(
          fill = "#7AC5CD",
          color = "#53868B",
          aes(
            text = paste0(
              "<b>Modalidad:</b> ",
              work_mode,
              "<br><b>Cantidad:</b> ",
              Frecuencia
            )
          )
        ) +
        labs(title = "Modalidad de trabajo", x = "Modalidad", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")),
      tooltip = "text"
    )
  })
  
  output$plot_edad <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = age)) +
        geom_histogram(
          bins = 8,
          fill = "#EED2EE",
          color = "#CDB5CD"
        ) +
        labs(title = "Edad de los individuos", x = "Edad", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_mode_occupation <- renderPlotly({
    ggplotly(
      ggplot(datos) +
        geom_bar(position = "dodge") +
        aes(x = occupation, fill = work_mode) +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = "Cantidad de individuos según ocupación \ny modalidad de trabajo",
          x = "Ocupación",
          y = "Cantidad de individuos",
          fill = "Modalidad de Trabajo"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 30)
        )
    ) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.1,
          title = list(text = "<b>Modalidad de Trabajo:</b>")
        ),
        margin = list(t = 120, l = 80)
      )
  })
  
  output$plot_age_vs_ocu <- renderPlotly({
    ggplotly(
      ggplot(datos, aes(x = occupation, y = age)) +
        geom_boxplot(
          fill = "#CCE283",
          color = "#A8CF60",
          outlier.colour = "#A8CF60",
          outlier.stroke = 0.8
        ) +
        labs(title = "Distribución de la edad de los \nindividuos según ocupación", x = "Ocupación", y = "Edad de los individuos") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 30)
        ),
      tooltip = "none"
    ) %>%
      layout(margin = list(l = 80), xaxis = list(title = list(standoff = 20)))
  })
  
  # Horas de pantalla
  
  datos_filtrados2 <- reactive({
    datos %>%
      filter(
        if (length(input$filtro_genero_d) > 0) gender %in% input$filtro_genero_d else TRUE,
        if (length(input$filtro_workmode_d) > 0) work_mode %in% input$filtro_workmode_d else TRUE,
        if (length(input$filtro_ocupacion_d) > 0) occupation %in% input$filtro_ocupacion_d else TRUE,
        age >= input$filtro_edad_d[1],
        age <= input$filtro_edad_d[2]
      )
  })
  
  variable_horas <- reactive({
    input$filtro_tipo_horas
  })
  
  titulo_hist_r <- reactive({
    tipo <- input$filtro_tipo_horas
    
    if (tipo == "screen_time_hours") {
      "Distribución de las horas de pantalla \ntotales"
    } else if (tipo == "work_screen_hours") {
      "Distribución de las horas de pantalla \nlaborales"
    } else {
      "Distribución de las horas de pantalla \nrecreativas"
    }
  })
  
  titulo_vs_mode_r <- reactive({
    tipo <- input$filtro_tipo_horas
    
    if (tipo == "screen_time_hours") {
      "Horas de pantalla totales según \nla modalidad de trabajo"
    } else if (tipo == "work_screen_hours") {
      "Horas de pantalla laborales según \nla modalidad de trabajo"
    } else {
      "Horas de pantalla recreativas según \nla modalidad de trabajo"
    }
  })
  
  titulo_vs_social_r <- reactive({
    tipo <- input$filtro_tipo_horas
    
    if (tipo == "screen_time_hours") {
      "Horas de pantalla totales según \nla cantidad de horas sociales"
    } else if (tipo == "work_screen_hours") {
      "Horas de pantalla laborales según \nla cantidad de horas sociales"
    } else {
      "Horas de pantalla recreativas según \nla cantidad de horas sociales"
    }
  })
  
  titulo_vs_prod_r <- reactive({
    tipo <- input$filtro_tipo_horas
    
    if (tipo == "screen_time_hours") {
      "Horas de pantalla totales según \nel nivel de productividad"
    } else if (tipo == "work_screen_hours") {
      "Horas de pantalla laborales según \nel nivel de productividad"
    } else {
      "Horas de pantalla recreativas según \nel nivel de productividad"
    }
  })
  
  output$titulo_hist <- renderText({
    titulo_hist_r()
  })
  
  output$titulo_vs_mode <- renderText({
    titulo_vs_mode_r()
  })
  
  output$titulo_vs_social <- renderText({
    titulo_vs_social_r()
  })
  
  output$titulo_vs_prod <- renderText({
    titulo_vs_prod_r()
  })
  
  output$plot_screen_hist <- renderPlotly({
    var <- variable_horas()
    
    ggplotly(
      ggplot(datos_filtrados2(), aes(x = .data[[var]])) +
        geom_histogram(bins = 10, fill = "#7AC5CD", color = "#53868B") +
        labs(
          title = titulo_hist_r(),
          x = "Horas",
          y = "Frecuencia"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.margin = margin(t = 20)
              )
    )
  })
  
  
  output$plot_screen_vs_mode <- renderPlotly({
    var <- variable_horas()
    
    ggplotly(
      ggplot(datos_filtrados2(), aes(x = work_mode, y = .data[[var]])) +
        geom_boxplot(fill = "#CCE283", color = "#A8CF60") +
        labs(
          title = titulo_vs_mode_r(),
          x = "Modalidad de trabajo",
          y = "Horas de pantalla"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.margin = margin(t = 25))
    )
  })
  
  output$plot_screen_vs_social <- renderPlotly({
    var <- variable_horas()
    
    ggplotly(
      ggplot(datos_filtrados2(), aes(x = social_hours_per_week, y = .data[[var]])) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#CD8C95") +
        labs(
          title = titulo_vs_social_r(),
          x = "Horas sociales",
          y = "Horas de pantalla"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.margin = margin(t = 25))
    )
  })
  
  output$plot_screen_vs_prod <- renderPlotly({
    var <- variable_horas()
    
    ggplotly(
      ggplot(datos_filtrados2(), aes(x = productivity_0_100, y = .data[[var]])) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#CD8500") +
        labs(
          title = titulo_vs_prod_r(),
          x = "Nivel de productividad",
          y = "Horas de pantalla"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.margin = margin(t = 25))
    )
  })
  
  # Sueño y bienestar físico
  
  datos_filtrados3 <- reactive({
    datos %>%
      filter(
        if (length(input$filtro_genero_s) > 0)
          gender %in% input$filtro_genero_s
        else
          TRUE,
        if (length(input$filtro_workmode_s) > 0)
          work_mode %in% input$filtro_workmode_s
        else
          TRUE,
        if (length(input$filtro_ocupacion_s) > 0)
          occupation %in% input$filtro_ocupacion_s
        else
          TRUE,
        
        age >= input$filtro_edad_s[1],
        age <= input$filtro_edad_s[2]
      )
  })
  
  output$plot_sleep_hours <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados3(), aes(x = sleep_hours)) +
        geom_histogram(
          bins = 10,
          fill = "#2E608B",
          color = "#26456E"
        ) +
        labs(title = "Horas de sueño", x = "Horas de sueño", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_sleep_quality <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados3(), aes(x = sleep_quality_1_5)) +
        geom_bar(fill = "#2E608B", color = "#26456E") +
        labs(title = "Calidad de sueño", x = "Calidad del sueño (1-5)", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_stress <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados3(), aes(x = stress_level_0_10)) +
        geom_histogram(
          bins = 10,
          fill = "#2E608B",
          color = "#26456E"
        ) +
        labs(title = "Nivel de estrés", x = "Nivel de Estrés (1-10)", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_sue_vs_stress <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados3(),
        aes(x = stress_level_0_10, y = sleep_hours)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#B4EEB4") +
        labs(title = "Horas de sueño vs Nivel de Estrés", x = "Nivel de estrés (0-10)", y = "Horas de sueño") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_calsue_vs_stress <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados3(),
        aes(x = sleep_quality_1_5, y = stress_level_0_10)
      ) +
        geom_boxplot(fill = "#2E608B") +
        labs(title = "Nivel de estrés según \nla calidad del sueño", x = "Calidad del sueño", y = "Nivel de estrés") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 30)
        )
    )
  })
  
  # Productividad y salud mental
  
  datos_filtrados4 <- reactive({
    datos %>%
      filter(
        if (length(input$filtro_genero_p) > 0)
          gender %in% input$filtro_genero_p
        else
          TRUE,
        if (length(input$filtro_workmode_p) > 0)
          work_mode %in% input$filtro_workmode_p
        else
          TRUE,
        if (length(input$filtro_ocupacion_p) > 0)
          occupation %in% input$filtro_ocupacion_p
        else
          TRUE,
        
        age >= input$filtro_edad_p[1],
        age <= input$filtro_edad_p[2]
      )
  })
  
  output$plot_productividad <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados4(), aes(x = productivity_0_100)) +
        geom_histogram(
          bins = 10,
          fill = "#BCEE68",
          color = "#A2CD5A"
        ) +
        labs(title = "Productividad", x = "Nivel de Productividad (0-100)", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_saludmental <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados4(), aes(x = mental_wellness_index_0_100)) +
        geom_histogram(
          bins = 10,
          fill = "#EEA2AD",
          color = "#CD8C95"
        ) +
        labs(title = "Salud mental", x = "Índice de Salud Mental (0-100)", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_prod_vs_stress <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados4(),
        aes(x = stress_level_0_10, y = productivity_0_100)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#FFDEAD") +
        labs(title = "Productividad vs Nivel de Estrés", x = "Nivel de estrés (0-10)", y = "Productividad (0-100)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_salud_vs_screen <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados4(),
        aes(x = stress_level_0_10, y = mental_wellness_index_0_100)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#EEA2AD") +
        labs(title = "Salud mental vs Nivel de Estrés", 
             x = "Nivel de estrés (0-10)", 
             y = "Índice de Salud Mental (0-100)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_ejercicio <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados4(), aes(x = exercise_minutes_per_week)) +
        geom_histogram(
          bins = 8,
          fill = "#B4CDCD",
          color = "#7A8B8B"
        ) +
        labs(title = "Minutos de ejercicio por semana", 
             x = "Minutos de ejercicio", 
             y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_social <- renderPlotly({
    ggplotly(
      ggplot(datos_filtrados4(), aes(x = social_hours_per_week)) +
        geom_histogram(
          bins = 8,
          fill = "#EEC591",
          color = "#CDAA7D"
        ) +
        labs(title = "Horas sociales por semana", 
             x = "Horas sociales", 
             y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_prod_vs_stress <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados4(),
        aes(x = stress_level_0_10, y = productivity_0_100)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#FFDEAD") +
        labs(title = "Productividad vs Nivel de Estrés", 
             x = "Nivel de estrés (0-10)", 
             y = "Productividad (0-100)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_prod_vs_ejercicio <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados4(),
        aes(x = exercise_minutes_per_week, y = productivity_0_100)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#7EC0EE") +
        labs(title = "Productividad vs Minutos de ejercicio", 
             x = "Minutos de ejercicio por semana", 
             y = "Productividad (0-100)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
  output$plot_salud_vs_social <- renderPlotly({
    ggplotly(
      ggplot(
        datos_filtrados4(),
        aes(x = social_hours_per_week, y = mental_wellness_index_0_100)
      ) +
        geom_point(alpha = 0.4, color = "#8B8B7A") +
        geom_smooth(method = "lm", color = "#EEAEEE") +
        labs(title = "Salud mental vs Horas sociales", 
             x = "Horas sociales por semana", 
             y = "Índice de Salud Mental (0-100)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  })
  
}

shinyApp(ui, server)
