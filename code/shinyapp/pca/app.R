library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(rlang)
library(rsconnect)
library(DBI)
library(RSQLite)
library(bslib)

#loading in data with some contigencies to avoid loading errors in shiny
pca_path <- file.path(getwd(), "pca_combined.db")
stopifnot(file.exists(pca_path))
con <- dbConnect(SQLite(), pca_path)
print(dbGetQuery(con, "PRAGMA database_list;"))
tables <- dbListTables(con)
print(tables)
table_to_read <- if ("pca_combined" %in% tables) "pca_combined" else tables[[1]]
pca_combined <- dbReadTable(con, table_to_read)
dbDisconnect(con)

#making sure these are numeric for this plot
pca_combined <- pca_combined |>
  mutate(
    PC1 = suppressWarnings(as.numeric(PC1)),
    age = suppressWarnings(as.numeric(age))
  )

#making age groups
pca_combined <- pca_combined |>
  mutate(
    age_group = case_when(
      is.na(age) ~ NA_character_,
      age <= 24 ~ "18–24",
      age <= 34~ "25–34",
      age <= 44 ~ "35–44",
      age <= 54 ~ "45–54",
      age <= 64 ~ "55–64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c("18–24","25–34","35–44","45–54","55–64","65+")
    )
  )

#setting variable labels
var_labels <- c(
  "Region" = "region",
  "Education" = "education",
  "Gender" = "gender",
  "Locality level" = "locality_level",
  "Cluster" = "cluster",
  "Age group" = "age_group"
)

#setting response answers
var_responses <- list(
  region = c("1" = "Manila NCR", "2" = "Luzon", "3" = "Visayas", "4" = "Mindanao"),
  gender = c("1" = "Male", "2" = "Female"),
  cluster = c("1" = "Cluster 1", "2" = "Cluster 2"),
  locality_level = c(
    "1" = "Capital / Megacity (1M+)",
    "2" = "Major city (100k+)",
    "3" = "Small city / town (<100k)",
    "4" = "Village / countryside"
  ),
  education = c(
    "1"  = "No formal education",
    "2"  = "Incomplete primary",
    "3"  = "Complete primary",
    "4"  = "Incomplete secondary (tech/voc)",
    "5"  = "Complete secondary (tech/voc)",
    "6"  = "Incomplete secondary",
    "7"  = "Complete secondary",
    "8"  = "Some university",
    "9"  = "University completed",
    "10" = "Post-graduate degree"
  ),
  age_group = c("18–24","25–34","35–44","45–54","55–64","65+")
)

x_orders <- list(
  region = c("Manila NCR", "Luzon", "Visayas", "Mindanao"),
  age_group = c("18–24","25–34","35–44","45–54","55–64","65+")
)

#creating the theme for the shiny UI
ui <- fluidPage(
  theme = bs_theme(
    bg = "#222222",
    fg = "#ffffff",
    primary = "#50ab75",
    base_font = font_google("Inter")
  ),
  titlePanel("PC1 by Demographics (Mean with 95% CI)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X variable:", choices = var_labels, selected = "region")
    ),
    mainPanel(
      plotOutput("pcPlot", height = "550px")
    )
  )
)

#function to generate shiny app plot
server <- function(input, output, session) {
  
  output$pcPlot <- renderPlot({
    
    xvar <- input$xvar
    
    #doing some reformatting of the data to add responses/labels
    df <- pca_combined |>
      filter(!is.na(PC1), !is.na(.data[[xvar]])) |>
      mutate(
        x_raw = .data[[xvar]],
        x = if (xvar %in% names(var_responses)) {
          mapped <- var_responses[[xvar]][as.character(x_raw)]
          ifelse(is.na(mapped), as.character(x_raw), mapped)
        } else {
          as.character(x_raw)
        }
      )
    
    if (xvar %in% names(x_orders)) {
      df <- df |> mutate(x = factor(x, levels = x_orders[[xvar]]))
    } else {
      df <- df |> mutate(x = factor(x))
    }
    
    #calculating my own confidence intervals because I kept running into some rendering errors with what I was doing before
    summ <- df |>
      dplyr::group_by(x) |>
      dplyr::summarise(
        n    = sum(!is.na(PC1)),
        mean = mean(PC1, na.rm = TRUE),
        sd   = sd(PC1, na.rm = TRUE),
        se   = sd / sqrt(n),
        t    = ifelse(n > 1, stats::qt(0.975, df = n - 1), NA_real_),
        ci   = t * se,
        ymin = ifelse(n > 1, mean - ci, mean),
        ymax = ifelse(n > 1, mean + ci, mean),
        .groups = "drop"
      )
    
    #making title
    x_title <- names(var_labels)[match(xvar, var_labels)]
    
    #the actual plot
    ggplot(summ, aes(x = x, y = mean)) +
      geom_point(size = 4, color = "#FF6B6B") +
      geom_errorbar(
        aes(ymin = ymin, ymax = ymax),
        width = 0.2,
        linewidth = 2,
        color = "#FF6B6B"
      ) +
      labs(
        title = paste("Mean PC1 by", x_title, "(with 95% CI)"),
        x = x_title,
        y = "Mean PC1"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.background  = element_rect(fill = "#222222", color = NA),
        plot.background   = element_rect(fill = "#222222", color = NA),
        legend.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major  = element_line(color = "grey"),
        panel.grid.minor  = element_blank(),
        axis.text         = element_text(color = "white"),
        axis.title        = element_text(color = "white"),
        strip.text        = element_text(color = "white"),
        plot.title        = element_text(color = "white", face = "bold", size = 13, hjust = 0.5),
        axis.text.x       = element_text(angle = 45, hjust = 1))
    
  })
}


shinyApp(ui, server)