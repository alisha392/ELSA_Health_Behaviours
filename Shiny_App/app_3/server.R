library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

server <- function(input, output, session) {
  proportions <- read.csv("Outcome_proportions.csv", header = TRUE, sep = ",")
  data <- reactive({
    req(input$sel_Outcome)
    if (input$sel_Outcome == "Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Multi)
    } else if (input$sel_Outcome == "Complex Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected = proportions$CMulti)
    } else if (input$sel_Outcome == "Eye disorders") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Eye)
    } else if (input$sel_Outcome == "Endocrine, nutritional and metabolic disorders") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Endocrine)
    } else if (input$sel_Outcome == "Musculoskeletal and connective system disorders") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Muscular)
    } else if (input$sel_Outcome == "Respiratory disorders") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Respiratory)
    } else if (input$sel_Outcome == "Mental and behavioural issues") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Mental)
    } else if (input$sel_Outcome == "Nervous system disorders") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Nervous)
    } else if (input$sel_Outcome == "Neoplasms") {
      df <- proportions %>% mutate(Outcome_selected = proportions$Neoplasm)
    } else {
      df <- proportions %>% mutate(Outcome_selected = proportions$Circulatory)
    }
  })

  data_4 <- reactive({
    req(input$sel_Outcome_4)
    if (input$sel_Outcome_4 == "Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Multi_4)
    } else if (input$sel_Outcome_4 == "Complex Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$CMulti_4)
    } else if (input$sel_Outcome_4 == "Eye disorders") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Eye_4)
    } else if (input$sel_Outcome_4 == "Endocrine, nutritional and metabolic disorders") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Endocrine_4)
    } else if (input$sel_Outcome_4 == "Musculoskeletal and connective system disorders") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Muscular_4)
    } else if (input$sel_Outcome_4 == "Respiratory disorders") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Respiratory_4)
    } else if (input$sel_Outcome_4 == "Mental and behavioural issues") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Mental_4)
    } else if (input$sel_Outcome_4 == "Nervous system disorders") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Nervous_4)
    } else if (input$sel_Outcome_4 == "Neoplasms") {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Neoplasm_4)
    } else {
      df <- proportions %>% mutate(Outcome_selected_4 = proportions$Circulatory_4)
    }
  })

  data_Cov <- reactive({
    req(input$sel_Outcome_Cov)
    if (input$sel_Outcome_Cov == "Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Multi_cov)
    } else if (input$sel_Outcome_Cov == "Complex Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$CMulti_cov)
    } else if (input$sel_Outcome_Cov == "Eye disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Eye_cov)
    } else if (input$sel_Outcome_Cov == "Endocrine, nutritional and metabolic disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Endocrine_cov)
    } else if (input$sel_Outcome_Cov == "Musculoskeletal and connective system disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Muscular_cov)
    } else if (input$sel_Outcome_Cov == "Respiratory disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Respiratory_cov)
    } else if (input$sel_Outcome_Cov == "Mental and behavioural issues") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Mental_cov)
    } else if (input$sel_Outcome_Cov == "Nervous system disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Nervous_cov)
    } else if (input$sel_Outcome_Cov == "Neoplasms") {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Neoplasm_cov)
    } else {
      df <- proportions %>% mutate(Outcome_selected_Cov = proportions$Circulatory_cov)
    }
  })

  data_Cov_4 <- reactive({
    req(input$sel_Outcome_Cov_4)
    if (input$sel_Outcome_Cov_4 == "Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Multi_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Complex Multimorbidity") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$CMulti_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Eye disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Eye_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Endocrine, nutritional and metabolic disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Endocrine_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Musculoskeletal and connective system disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Muscular_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Respiratory disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Respiratory_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Mental and behavioural issues") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Mental_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Nervous system disorders") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Nervous_cov_4)
    } else if (input$sel_Outcome_Cov_4 == "Neoplasms") {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Neoplasm_cov_4)
    } else {
      df <- proportions %>% mutate(Outcome_selected_Cov_4 = proportions$Circulatory_cov_4)
    }
  })

  output$plot_W9 <- renderPlot({
    g <- ggplot(data(), aes(y = Outcome_selected, x = Latent_Class))
    g + geom_bar(stat = "identity") + scale_x_discrete(
      limits = c(
        "Latent Class 1", "Latent Class 2", "Latent Class 3", "Latent Class 4",
        "Latent Class 5", "Latent Class 6"
      ),
      labels = c(
        "Hazardous drinkers with healthy lifestyle",
        "Unhealthy consumers",
        "All-round health conscious",
        "Smokers",
        "Inactive moderate drinkers",
        "Risk-avoidant"
      )
    ) +
      labs(x = "Latent Classes", y = "Proportion") +
      geom_col(fill = "skyblue") +
      geom_text(aes(label = Outcome_selected, vjust = 2)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title = element_text(size = 14, face = "bold"))
  })

  output$plot_W4 <- renderPlot({
    g <- ggplot(data_4(), aes(y = Outcome_selected_4, x = Latent_Class))
    g + geom_bar(stat = "identity") + scale_x_discrete(
      limits = c(
        "Latent Class 1", "Latent Class 2", "Latent Class 3", "Latent Class 4",
        "Latent Class 5", "Latent Class 6"
      ),
      labels = c(
        "Hazardous drinkers with healthy lifestyle",
        "Unhealthy consumers",
        "All-round health conscious",
        "Smokers",
        "Inactive moderate drinkers",
        "Risk-avoidant"
      )
    ) +
      labs(x = "Latent Classes", y = "Proportion") +
      geom_col(fill = "skyblue") +
      geom_text(aes(label = Outcome_selected_4, vjust = 2)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title = element_text(size = 14, face = "bold"))
  })

  output$plot_W9_Cov <- renderPlot({
    g_Cov <- ggplot(data_Cov(), aes(y = Outcome_selected_Cov, x = Latent_Class))
    g_Cov + geom_bar(stat = "identity") + scale_x_discrete(
      limits = c(
        "Latent Class 1", "Latent Class 2", "Latent Class 3", "Latent Class 4",
        "Latent Class 5", "Latent Class 6"
      ),
      labels = c(
        "Hazardous drinkers with healthy lifestyle",
        "Unhealthy consumers",
        "All-round health conscious",
        "Smokers",
        "Inactive moderate drinkers",
        "Risk-avoidant"
      )
    ) +
      labs(x = "Latent Classes", y = "Proportion") +
      scale_y_continuous(limits = c(0, 1)) +
      geom_col(fill = "skyblue") +
      geom_text(aes(label = Outcome_selected_Cov, vjust = 2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title = element_text(size = 14, face = "bold"))
  })

  output$plot_W4_Cov <- renderPlot({
    g_Cov <- ggplot(data_Cov_4(), aes(y = Outcome_selected_Cov_4, x = Latent_Class))
    g_Cov + geom_bar(stat = "identity") + scale_x_discrete(
      limits = c(
        "Latent Class 1", "Latent Class 2", "Latent Class 3", "Latent Class 4",
        "Latent Class 5", "Latent Class 6"
      ),
      labels = c(
        "Hazardous drinkers with healthy lifestyle",
        "Unhealthy consumers",
        "All-round health conscious",
        "Smokers",
        "Inactive moderate drinkers",
        "Risk-avoidant"
      )
    ) +
      labs(x = "Latent Classes", y = "Proportion") +
      scale_y_continuous(limits = c(0, 1)) +
      geom_col(fill = "skyblue") +
      geom_text(aes(label = Outcome_selected_Cov_4, vjust = 2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title = element_text(size = 14, face = "bold"))
  })

  output$plot_Lung_Cov <- renderPlot({
    g_Cov <- ggplot(proportions, aes(y = Lung_cancer_cov, x = Latent_Class))
    g_Cov + geom_bar(stat = "identity") + scale_x_discrete(
      limits = c(
        "Latent Class 1", "Latent Class 2", "Latent Class 3", "Latent Class 4",
        "Latent Class 5", "Latent Class 6"
      ),
      labels = c(
        "Hazardous drinkers with healthy lifestyle",
        "Unhealthy consumers",
        "All-round health conscious",
        "Smokers",
        "Inactive moderate drinkers",
        "Risk-avoidant"
      )
    ) +
      labs(x = "Latent Classes", y = "Proportion") +
      geom_col(fill = "skyblue") +
      geom_text(aes(label = Lung_cancer_cov, vjust = 2)) +
      scale_y_continuous(limits = c(0, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title = element_text(size = 14, face = "bold"))
  })
}
