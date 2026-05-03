library(shiny)
library(tidyverse)

data_ckd <- read.csv("IHME-GBD_CKD_20260130.csv")

data_ckd <- data_ckd %>%
  mutate(
    year = as.integer(year),
    val = as.numeric(val),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  )

ui <- fluidPage(
  titlePanel("Chronic Kidney Disease (CKD) Mortality and Burden in Older Adults in China"),
  
  sidebarLayout(
    sidebarPanel(
      h4("App Goal"),
      p("This app is used to look at CKD trends among adults aged 60+ in China. Users can choose the CKD measure, sex group, and year range. The app also gives a simple comparison between males and females."),
      
      selectInput(
        "measure",
        "Choose CKD measure:",
        choices = c(
          "Deaths",
          "DALYs (Disability-Adjusted Life Years)",
          "Prevalence"
        ),
        selected = "Deaths"
      ),
      
      checkboxGroupInput(
        "sex",
        "Choose sex:",
        choices = c("Male", "Female"),
        selected = c("Male", "Female")
      ),
      
      sliderInput(
        "years",
        "Choose year range:",
        min = 2013,
        max = 2023,
        value = c(2013, 2023),
        step = 1,
        sep = ""
      ),
      
      actionButton("run_test", "Run Statistical Test")
    ),
    
    mainPanel(
      h3("Question"),
      p("Do CKD burden rates among adults aged 60+ in China differ by sex over time?"),
      
      h3("Trend Plot"),
      plotOutput("trend_plot"),
      
      h3("Summary Statistics"),
      tableOutput("summary_table"),
      
      h3("Statistical Test Results"),
      verbatimTextOutput("stat_result"),
      
      h3("Interpretation"),
      verbatimTextOutput("interpretation")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data_ckd %>%
      filter(
        location_name == "China",
        age_name == "60+ years",
        measure_name == input$measure,
        sex_name %in% input$sex,
        year >= input$years[1],
        year <= input$years[2]
      )
  })
  
  output$trend_plot <- renderPlot({
    plot_data <- filtered_data()
    
    ggplot(plot_data, aes(x = year, y = val, color = sex_name, group = sex_name)) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper, fill = sex_name),
        alpha = 0.18,
        color = NA
      ) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      theme_minimal() +
      labs(
        title = paste("CKD", input$measure, "Rate Among Adults Aged 60+ in China"),
        subtitle = "The shaded area shows the uncertainty range from the dataset",
        x = "Year",
        y = "Rate per 100,000",
        color = "Sex",
        fill = "Sex"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top"
      )
  })
  
  output$summary_table <- renderTable({
    filtered_data() %>%
      group_by(sex_name) %>%
      summarise(
        Mean = round(mean(val, na.rm = TRUE), 2),
        SD = round(sd(val, na.rm = TRUE), 2),
        Min = round(min(val, na.rm = TRUE), 2),
        Max = round(max(val, na.rm = TRUE), 2),
        .groups = "drop"
      )
  })
  
  test_result <- eventReactive(input$run_test, {
    df <- filtered_data()
    
    if (length(unique(df$sex_name)) < 2) {
      return(NULL)
    }
    
    t.test(val ~ sex_name, data = df)
  })
  
  output$stat_result <- renderPrint({
    if (input$run_test == 0) {
      cat("Click 'Run Statistical Test' to see the comparison.")
    } else {
      res <- test_result()
      
      if (is.null(res)) {
        cat("Please select both Male and Female.")
      } else {
        cat("Welch two-sample t-test\n\n")
        cat("Female mean:", round(res$estimate[1], 2), "\n")
        cat("Male mean:", round(res$estimate[2], 2), "\n")
        cat("p-value:", round(res$p.value, 4), "\n")
        cat("95% CI:", round(res$conf.int[1], 2), "to", round(res$conf.int[2], 2), "\n")
      }
    }
  })
  
  output$interpretation <- renderText({
    if (input$run_test == 0) {
      return("Click the button above to compare males and females.")
    }
    
    res <- test_result()
    
    if (is.null(res)) {
      return("Both Male and Female need to be selected for this comparison.")
    }
    
    p_val <- res$p.value
    ci <- res$conf.int
    female_mean <- res$estimate[1]
    male_mean <- res$estimate[2]
    
    paste0(
      "In the selected years, the average rate is ",
      round(female_mean, 2), " for females and ",
      round(male_mean, 2), " for males. The p-value is ",
      round(p_val, 4), ". The 95% confidence interval for the difference is ",
      round(ci[1], 2), " to ", round(ci[2], 2), ". ",
      ifelse(
        p_val < 0.05,
        "This suggests that the difference between the two groups is statistically meaningful.",
        "This means the difference is not statistically significant at the 0.05 level, but the pattern can still be useful for describing sex differences in CKD burden."
      )
    )
  })
}

shinyApp(ui = ui, server = server)