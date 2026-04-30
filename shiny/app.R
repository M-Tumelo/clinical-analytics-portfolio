# =========================================================
# CLINICAL STUDY REVIEW DASHBOARD (v4 - Premium)
# =========================================================

# ---------------------------
# 1. PACKAGES
# ---------------------------
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")

library(shiny)
library(tidyverse)
library(DT)

# ---------------------------
# 2. DATA
# ---------------------------
setwd("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio")

population_summary <- read.csv("data/outputs/population_summary.csv")
ae_by_arm <- read.csv("data/outputs/ae_by_arm.csv")
ae_serious <- read.csv("data/outputs/ae_serious.csv")
top_ae_terms <- read.csv("data/outputs/top_ae_terms.csv")
lab_change <- read.csv("data/outputs/lab_change.csv")

# ---------------------------
# CLEAN DATA (HANDLE NA)
# ---------------------------
ae_by_arm <- ae_by_arm %>%
  mutate(ARM = ifelse(is.na(ARM), "Missing", ARM))

ae_serious <- ae_serious %>%
  mutate(
    ARM = ifelse(is.na(ARM), "Missing", ARM),
    SERIOUS_FLAG = ifelse(is.na(SERIOUS_FLAG), "Missing", SERIOUS_FLAG)
  )

lab_change <- lab_change %>%
  mutate(
    ARM = ifelse(is.na(ARM), "Missing", ARM),
    LBTEST = ifelse(is.na(LBTEST), "Missing", LBTEST)
  )

# =========================================================
# 3. UI
# =========================================================
ui <- navbarPage(
  
  title = "Clinical Study Dashboard",
  
  # ---------------------------
  # TAB 1: POPULATION
  # ---------------------------
  tabPanel("Population",
           
           fluidPage(
             
             br(),
             h2("Study Population Overview"),
             hr(),
             
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Total Subjects"),
                        h2(n_distinct(population_summary$USUBJID))
                      )),
               column(4,
                      wellPanel(
                        h4("Treatment Arms"),
                        h2(n_distinct(population_summary$ARM))
                      )),
               column(4,
                      wellPanel(
                        h4("Mean Age"),
                        h2(round(mean(population_summary$AGE, na.rm = TRUE), 1))
                      ))
             ),
             
             br(),
             
             fluidRow(
               column(6,
                      plotOutput("pop_bar", height = "300px")),
               column(6,
                      DTOutput("pop_table"))
             )
           )
  ),
  
  # ---------------------------
  # TAB 2: SAFETY
  # ---------------------------
  tabPanel("Safety",
           
           fluidPage(
             
             br(),
             h2("Adverse Event Analysis"),
             hr(),
             
             fluidRow(
               column(4,
                      selectInput(
                        "arm_filter",
                        "Drill-down by Arm:",
                        choices = c("All", unique(ae_by_arm$ARM)),
                        selected = "All"
                      ))
             ),
             
             br(),
             
             fluidRow(
               column(6,
                      plotOutput("ae_bar", height = "300px")),
               column(6,
                      plotOutput("ae_serious", height = "300px"))
             ),
             
             br(),
             
             fluidRow(
               column(12,
                      DTOutput("ae_table"))
             ),
             
             br(),
             
             fluidRow(
               column(12,
                      DTOutput("ae_top_table"))
             )
           )
  ),
  
  # ---------------------------
  # TAB 3: LABS
  # ---------------------------
  tabPanel("Labs",
           
           fluidPage(
             
             br(),
             h2("Laboratory Analysis"),
             hr(),
             
             fluidRow(
               column(4,
                      selectInput(
                        "lbtest_filter",
                        "Select Lab Test:",
                        choices = unique(lab_change$LBTEST)
                      ))
             ),
             
             br(),
             
             fluidRow(
               column(6,
                      plotOutput("lab_bar", height = "300px")),
               column(6,
                      plotOutput("lab_trend", height = "300px"))
             ),
             
             br(),
             
             fluidRow(
               column(12,
                      DTOutput("lab_table"))
             )
           )
  )
)

# =========================================================
# 4. SERVER
# =========================================================
server <- function(input, output) {
  
  # ---------------------------
  # POPULATION
  # ---------------------------
  output$pop_bar <- renderPlot({
    ggplot(population_summary, aes(x = ARM)) +
      geom_bar(fill = "#2C3E50") +
      theme_minimal(base_size = 13) +
      labs(title = "Subjects per Treatment Arm",
           x = "Treatment Arm",
           y = "Subjects")
  })
  
  output$pop_table <- renderDT({
    datatable(population_summary, options = list(pageLength = 5))
  })
  
  # ---------------------------
  # SAFETY
  # ---------------------------
  
  # FILTERED TABLE ONLY
  filtered_ae <- reactive({
    if (input$arm_filter == "All") {
      ae_by_arm
    } else {
      ae_by_arm %>% filter(ARM == input$arm_filter)
    }
  })
  
  output$ae_bar <- renderPlot({
    ggplot(ae_by_arm, aes(x = ARM, y = total_events)) +
      geom_col(fill = "#E74C3C") +
      theme_minimal(base_size = 13) +
      labs(title = "AE Count by Treatment Arm",
           x = "Treatment Arm",
           y = "Number of Events")
  })
  
  output$ae_serious <- renderPlot({
    ggplot(ae_serious, aes(x = ARM, y = count, fill = SERIOUS_FLAG)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("#3498DB", "#E74C3C", "grey")) +
      theme_minimal(base_size = 13) +
      labs(title = "Serious vs Non-Serious AEs",
           x = "Treatment Arm",
           y = "Count",
           fill = "Severity")
  })
  
  output$ae_table <- renderDT({
    datatable(filtered_ae(), options = list(pageLength = 5))
  })
  
  output$ae_top_table <- renderDT({
    datatable(top_ae_terms, options = list(pageLength = 5))
  })
  
  # ---------------------------
  # LABS
  # ---------------------------
  filtered_lb <- reactive({
    lab_change %>% filter(LBTEST == input$lbtest_filter)
  })
  
  output$lab_bar <- renderPlot({
    ggplot(filtered_lb(), aes(x = ARM, y = mean_change)) +
      geom_col(fill = "#27AE60") +
      theme_minimal(base_size = 13) +
      labs(title = "Mean Lab Change by Arm",
           x = "Treatment Arm",
           y = "Change from Baseline")
  })
  
  output$lab_trend <- renderPlot({
    ggplot(filtered_lb(), aes(x = ARM, y = mean_change)) +
      geom_point(size = 3, color = "#2980B9") +
      geom_line(group = 1, color = "#2980B9") +
      theme_minimal(base_size = 13) +
      labs(title = "Lab Trend Across Arms",
           x = "Treatment Arm",
           y = "Change")
  })
  
  output$lab_table <- renderDT({
    datatable(filtered_lb(), options = list(pageLength = 5))
  })
}

# =========================================================
# 5. RUN
# =========================================================
shinyApp(ui, server)