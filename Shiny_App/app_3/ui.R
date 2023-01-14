library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  navbarPage(
    "English longitudinal study of ageing (2008-2019)",
    tabPanel(
      "Latent Classes", # tab panel 1
      mainPanel(tags$img(
        src = "Latent_Classes.png",
        width = 1100, height = 600
      ), style = "text-align: center;")
    ),
    tabPanel(
      "Wave 9 Outcomes", # tab panel 2
      sidebarPanel(
        width = 4,
        selectInput(
          inputId = "sel_Outcome",
          label = "Choose Outcome (Wave 9)",
          choices = list(
            "Multimorbidity", "Complex Multimorbidity",
            "Eye disorders", "Circulatory disorders",
            "Endocrine, nutritional and metabolic disorders",
            "Musculoskeletal and connective system disorders",
            "Respiratory disorders", "Neoplasms",
            "Nervous system disorders", "Mental and behavioural issues"
          )
        )
      ),
      mainPanel(
        width = 8, h5("Proportion of individuals with outcome conditional on latent class membership"),
        plotOutput("plot_W9", width = "100%", height = 700)
      )
    ),
    tabPanel(
      "Wave 9 Outcomes (Adjusted for covariates)", # tab panel 3
      sidebarPanel(
        width = 4,
        selectInput(
          inputId = "sel_Outcome_Cov",
          label = "Choose Outcome (Wave 9)", choices = list(
            "Multimorbidity", "Complex Multimorbidity",
            "Eye disorders", "Circulatory disorders",
            "Endocrine, nutritional and metabolic disorders",
            "Musculoskeletal and connective system disorders",
            "Respiratory disorders", "Neoplasms",
            "Nervous system disorders", "Mental and behavioural issues"
          )
        )
      ),
      mainPanel(
        width = 8, h5("Proportion of individuals with outcome conditional on latent class membership (adjusted for covariates)"),
        plotOutput("plot_W9_Cov", width = "100%", height = 700)
      )
    ),
    tabPanel(
      "Wave 4 Outcomes",
      sidebarPanel(
        width = 4,
        selectInput(
          inputId = "sel_Outcome_4",
          label = "Choose Outcome (Wave 4)",
          choices = list(
            "Multimorbidity", "Complex Multimorbidity",
            "Eye disorders", "Circulatory disorders",
            "Endocrine, nutritional and metabolic disorders",
            "Musculoskeletal and connective system disorders",
            "Respiratory disorders", "Neoplasms",
            "Nervous system disorders", "Mental and behavioural issues"
          )
        )
      ),
      mainPanel(
        width = 8, h5("Proportion of individuals with outcome conditional on latent class membership"),
        plotOutput("plot_W4", width = "100%", height = 700)
      )
    ), # tab panel 4
    tabPanel(
      "Wave 4 Outcomes (Adjusted for covariates)",
      sidebarPanel(
        width = 4,
        selectInput(
          inputId = "sel_Outcome_Cov_4",
          label = "Choose Outcome (Wave 4)",
          choices = list(
            "Multimorbidity", "Complex Multimorbidity",
            "Eye disorders", "Circulatory disorders", "Endocrine, nutritional and metabolic disorders",
            "Musculoskeletal and connective system disorders",
            "Respiratory disorders", "Neoplasms",
            "Nervous system disorders", "Mental and behavioural issues"
          )
        )
      ),
      mainPanel(
        width = 8, h5("Proportion of individuals with outcome conditional on latent class membership (adjusted for covariates)"),
        plotOutput("plot_W4_Cov", width = "100%", height = 700)
      )
    ), # tab panel 5
    tabPanel(
      "Lung Cancer (Wave 9)",
      mainPanel(
        width = 8, h5("Proportion of individuals with outcome conditional on latent class membership (adjusted for covariates)"),
        plotOutput("plot_Lung_Cov", width = "100%", height = 700)
      )
    ) # tab panel 6
  )
)
