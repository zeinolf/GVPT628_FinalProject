library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(rlang)
library(rsconnect)
library(DBI)
library(RSQLite)
library(bslib)

#reading in cluster data
con <- dbConnect(SQLite(), "asia_final_cluster.db")
asia_final_cluster <- dbReadTable(con, "asia_final_cluster.db")
dbDisconnect(con)


#label variable choices for the drop down selector
var_choices <- c(
  "China influence on our country"           = "china_inf_op",
  "China influence on world affairs"         = "china_world_inf",
  "China harm vs. good to Asia"              = "china_harm",
  "China democratic scale (1–10)"            = "china_dem",
  "China influence on our country (degree)"  = "china_deg_inf",
  "Best model for development"               = "model_development",
  "Which country has most influence in Asia" = "asia_influence",
  "In 10 years, most influence in Asia"      = "asia_future_inf",
  "Way of life statement"                    = "way_of_life",
  "Aspect of major power influence"          = "aspect_power",
  "Immigration: inflow of foreigners"        = "foreign_immigration",
  "K-Means Clustering Group"                 = "cluster"
)

#labeling different demographic facets
facet_choices <- c(
  "None (all respondents)" = "none",
  "Cluster"                = "cluster",
  "Gender"                 = "gender",
  "Education level"        = "education",
  "Locality level"         = "locality_level"
)

#labeling titles for each variable question
var_titles <- list(
  way_of_life      = "Should our country defend our way of life or learn from others?",
  aspect_power     = "Which aspect of power do you think of when you refer to powerful countries?",
  china_world_inf  = "Generally speaking, the influence China today has on world affairs is…",
  foreign_immigration = "Should the government increase or decrease inflow of foreign workers?",
  china_deg_inf    = "How much influence does China have on our country?",
  china_inf_op     = "Generally speaking, the influence China has on our country is…",
  model_development = "Which country should be a model for our country’s future development?",
  asia_future_inf  = "In ten years, which country will have the most influence in Asia?",
  china_harm       = "Does China do more good or harm to Asia?",
  asia_influence   = "Which country has the most influence in Asia?",
  china_dem        = "Where would you place China today on the democracy scale?",
  cluster          = "K-Means Clustering Group"
)

#labeling the responses to each question
response_labels <- list(
  way_of_life = c(
    "1" = "Defend our way of life",
    "2" = "Learn from other countries"
  ),
  aspect_power = c(
    "1" = "Politics",
    "2" = "Economy",
    "3" = "(Military) Security",
    "4" = "Culture",
    "5" = "Other"
  ),
  china_world_inf = c(
    "1" = "Very positive",
    "2" = "Positive",
    "3" = "Somewhat positive",
    "4" = "Somewhat negative",
    "5" = "Negative",
    "6" = "Very negative"
  ),
  china_inf_op = c(
    "1" = "Very positive",
    "2" = "Positive",
    "3" = "Somewhat positive",
    "4" = "Somewhat negative",
    "5" = "Negative",
    "6" = "Very negative"
  ),
  china_harm = c(
    "1" = "Much more good than harm",
    "2" = "Somewhat more good than harm",
    "3" = "Somewhat more harm than good",
    "4" = "Much more harm than good"
  ),
  china_deg_inf = c(
    "1" = "A great deal of influence",
    "2" = "Some influence",
    "3" = "Not much influence",
    "4" = "No influence at all"
  ),
  model_development = c(
    "1" = "United States",
    "2" = "China",
    "3" = "India",
    "4" = "Japan",
    "5" = "Singapore",
    "6" = "Other"
  ),
  asia_influence = c(
    "1" = "China",
    "2" = "Japan",
    "3" = "India",
    "4" = "United States",
    "5" = "Other"
  ),
  asia_future_inf = c(
    "1" = "China",
    "2" = "Japan",
    "3" = "India",
    "4" = "United States",
    "5" = "Other"
  ),
  foreign_immigration = c(
    "1" = "Increase inflow",
    "2" = "Maintain current inflow",
    "3" = "Reduce inflow",
    "4" = "No more foreigners"
  ),
  china_dem = c(
    "1" = "Completely Undemocratic",
    "10" = "Completely Democratic"
  ),
  cluster = c(
    "1" = "Cluster 1",
    "2" = "Cluster 2"
  )
)

#labeling responses for demographic breakdowns
facet_label_maps <- list(
  gender = c(
    "1" = "Male",
    "2" = "Female"
  ),
  cluster = c(
    "1" = "Cluster 1",
    "2" = "Cluster 2"
  ),
  education = c(
    "1" = "No formal education",
    "2" = "Incomplete primary",
    "3" = "Complete primary",
    "4" = "Incomplete secondary (tech/voc)",
    "5" = "Complete secondary (tech/voc)",
    "6" = "Incomplete secondary",
    "7" = "Complete secondary",
    "8" = "Some university",
    "9" = "University completed",
    "10" = "Post-graduate degree"
  ),
  locality_level = c(
    "1" = "Megacity (1M+ people)",
    "2" = "Major city (100k+)",
    "3" = "Small town (<100k)",
    "4" = "Village / countryside"
  )
)

#creating a functionthat will creating labels for each respective demographic facet
facet_labeller <- function(facet_var) {
  function(value) {
    labels <- facet_label_maps[[facet_var]]
    if (is.null(labels)) return(value)
    out <- labels[as.character(value)]
    out[is.na(out)] <- value[is.na(out)]
    out
  }
}

#creating a function that will creating labels for each respective attitude question response
label_fun_for_var <- function(var) {
  function(x) {
    labs <- response_labels[[var]]
    if (is.null(labs)) return(x)
    out <- labs[as.character(x)]
    out[is.na(out)] <- x[is.na(out)]
    out
  }
}

#creating the GUI for the shiny app dashboard
ui <- fluidPage(
  theme = bs_theme(
    bg = "#222222",
    fg = "#ffffff",
    primary = "#50ab75",
    secondary = "#49456e",
    base_font = font_google("Inter")
  ),
  
  titlePanel("Asian Barometer – Attitudes & China Questions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "var",
        label    = "Select question / variable:",
        choices  = var_choices,
        selected = "china_inf_op"
      ),
      selectInput(
        inputId  = "facet",
        label    = "Facet by (optional):",
        choices  = facet_choices,
        selected = "none"
      )
    ),
    
    mainPanel(
      plotOutput("stackPlot", height = "550px")
    )
  )
)

#function to create the whole shiny app dashboard with graph
server <- function(input, output, session) {
  
  output$stackPlot <- renderPlot({
    var        <- input$var
    facet_var  <- input$facet
    
    plot_title <- var_titles[[var]]
    if (is.null(plot_title)) plot_title <- var
    
    df <- asia_final_cluster |>
      filter(!is.na(.data[[var]])) |>
      mutate(
        facet_group = if (facet_var == "none") {
          "All respondents"
        } else {
          as.factor(.data[[facet_var]])
        }
      ) |>
      count(wave, facet_group, value = .data[[var]]) |>
      group_by(wave, facet_group) |>
      mutate(prop = n / sum(n))
    
    ggplot(
      df,
      aes(
        x    = factor(wave),
        y    = prop,
        fill = factor(value)
      )
    ) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(
        palette  = "RdYlGn",
        direction = -1,
        labels   = label_fun_for_var(var)
      ) +
      labs(
        title = plot_title,
        x     = "Asian Barometer Wave",
        y     = "Percentage of Respondents",
        fill  = "Response"
      ) +
      {
        if (facet_var == "none") {
          theme()
        } else {
          facet_wrap(
            ~ facet_group,
            labeller = labeller(
              facet_group = facet_labeller(input$facet)
            )
          )
        }
      } +
      theme_minimal(base_size = 14) +
      theme(
        panel.background  = element_rect(fill = "#222222", color = NA),
        plot.background   = element_rect(fill = "#222222", color = NA),
        legend.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major  = element_line(color = "gray"),
        panel.grid.minor  = element_blank(),
        axis.text         = element_text(color = "white"),
        axis.title        = element_text(color = "white"),
        legend.text       = element_text(color = "white"),
        legend.title      = element_text(color = "white"),
        strip.text        = element_text(color = "white"),
        plot.title        = element_text(color = "white", face = "bold", size = 13, hjust = 0.5),
        plot.subtitle     = element_text(color = "white", size = 13, hjust = 0.5),
        axis.text.x       = element_text(angle = 45, hjust = 1)
      )
  })
}

#deploy app
shinyApp(ui, server)