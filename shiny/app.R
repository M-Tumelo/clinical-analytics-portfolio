# =========================================================
# MDR CLINICAL REVIEW DASHBOARD (INTERVIEW READY vFinal FIXED)
# =========================================================

# ---------------------------
# PACKAGES
# ---------------------------
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# ---------------------------
# DATA
# ---------------------------
adsl <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/processed/adsl.csv")
population_summary <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/outputs/population_summary.csv")
ae_by_arm <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/outputs/ae_by_arm.csv")
ae_serious <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/outputs/ae_serious.csv")
lab_change <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/outputs/lab_change.csv")
signal_summary <- read.csv("C:/Users/Tumel/OneDrive/Documents/clinical-analytics-portfolio/data/outputs/signal_summary.csv")

# ---------------------------
# COLORS
# ---------------------------
arm_colors <- c(
  "Treatment 1" = "#1F77B4",
  "Treatment 2" = "#FF7F0E",
  "Treatment 3" = "#2CA02C",
  "Treatment 4" = "#D62728"
)

# ---------------------------
# CLEAN SIGNAL LABELS
# ---------------------------
signal_summary$severity <- toupper(signal_summary$severity)

# =========================================================
# UI
# =========================================================
ui <- dashboardPage(
  
  dashboardHeader(title = "Clinical Review Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("mode", "Mode",
                  choices = c("Executive", "Analyst")),
      menuItem("Population", tabName = "pop", icon = icon("users")),
      menuItem("Safety", tabName = "safety", icon = icon("exclamation-triangle")),
      menuItem("Labs", tabName = "labs", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    
    # ---------------------------
    # SIGNAL STRIP
    # ---------------------------
    fluidRow(
      valueBox(sum(signal_summary$severity == "HIGH"),
               "High Risk Signals", color = "red"),
      valueBox(sum(signal_summary$severity == "MEDIUM"),
               "Medium Risk Signals", color = "yellow"),
      valueBox(sum(signal_summary$severity == "LOW"),
               "Low Risk Signals", color = "green")
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Active Study Signals",
        status = "primary",
        solidHeader = TRUE,
        DTOutput("signal_table")
      )
    ),
    
    tabItems(
      
      # =====================================================
      # POPULATION (TABLE ONLY - FIXED)
      # =====================================================
      tabItem(tabName = "pop",
              
              fluidRow(
                box(
                  width = 12,
                  title = "Study Population Overview (By Treatment Arm)",
                  status = "info",
                  solidHeader = TRUE,
                  DTOutput("pop_table")
                )
              )
      ),
      
      # =====================================================
      # SAFETY
      # =====================================================
      tabItem(tabName = "safety",
              
              fluidRow(
                box(width = 6,
                    title = "Adverse Events by Arm",
                    status = "danger",
                    solidHeader = TRUE,
                    plotOutput("ae_plot")
                ),
                
                box(width = 6,
                    title = "Serious Adverse Events",
                    status = "warning",
                    solidHeader = TRUE,
                    plotOutput("ae_serious_plot")
                )
              )
      ),
      
      # =====================================================
      # LABS
      # =====================================================
      tabItem(tabName = "labs",
              
              fluidRow(
                box(width = 12,
                    title = "Laboratory Changes by Arm",
                    status = "success",
                    solidHeader = TRUE,
                    plotOutput("lab_plot")
                )
              )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ---------------------------
  # SIGNAL TABLE
  # ---------------------------
  output$signal_table <- renderDT({
    
    datatable(
      signal_summary %>% select(ARM, signal, severity),
      options = list(pageLength = 5, dom = "t"),
      rownames = FALSE
    ) %>%
      formatStyle(
        "severity",
        target = "row",
        backgroundColor = styleEqual(
          c("HIGH", "MEDIUM", "LOW"),
          c("#f8d7da", "#fff3cd", "#d4edda")
        )
      )
  })
  
  # ---------------------------
  # POPULATION TABLE (ONLY VIEW)
  # ---------------------------
  output$pop_table <- renderDT({
    
    datatable(
      population_summary,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ---------------------------
  # SAFETY
  # ---------------------------
  output$ae_plot <- renderPlot({
    
    ggplot(ae_by_arm,
           aes(x = ARM, y = total_events, fill = ARM)) +
      geom_col() +
      scale_fill_manual(values = arm_colors) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$ae_serious_plot <- renderPlot({
    
    ggplot(ae_serious,
           aes(x = ARM, y = count, fill = SERIOUS_FLAG)) +
      geom_col() +
      theme_minimal()
  })
  
  # ---------------------------
  # LABS
  # ---------------------------
  output$lab_plot <- renderPlot({
    
    ggplot(lab_change,
           aes(x = ARM, y = mean_change, fill = ARM)) +
      geom_col() +
      scale_fill_manual(values = arm_colors) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# =========================================================
# RUN APP
# =========================================================
shinyApp(ui, server)