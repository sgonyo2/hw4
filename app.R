

#################
# Load Packages #
#################

library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(shinycssloaders)

###############
# Import data #
###############

data_subset <-
  readRDS(file = "../data/processed/data_subset.rds")
codebook <-
  read_csv("../data/processed/codebook.csv")

##########
# Graphs #
##########

graph <- function(data, overall) {
  {
    {
      data
    }
  } %>%
    ggplot(aes(Label, Value, group = 1)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    geom_point(
      data = {
        {
          overall
        }
      },
      aes(Label, Value, group = 1),
      size = 1,
      color = "black"
    ) +
    geom_line(
      data = {
        {
          overall
        }
      },
      aes(Label, Value, group = 1),
      size = 1,
      color = "black"
    ) +
    coord_flip() +
    labs(y = "Average value", x = "Question")
}

##############
# Statistics #
##############

#Democracy
dem_overall <- data_subset %>%
  summarise_at(vars(V228A:V228I),
               ~ mean(., na.rm = T)) %>%
  gather(key = var) %>%
  left_join(codebook) %>%
  rename_all( ~ str_to_sentence(.)) %>%
  mutate(
    Value = round(Value, 2),
    Label = str_remove(Label, "How often in country's elections: ")
  )

#News
news_vars <- str_c("V2", 17:24)
news_overall <- data_subset %>%
  select(news_vars) %>%
  gather(key = var) %>%
  count(var, value) %>%
  group_by(var) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    value = as.factor(value),
    value = fct_relevel(value, "Daily", "Weekly", "Monthly") %>%
      fct_rev()
  ) %>%
  left_join(codebook) %>%
  rename_all( ~ str_to_sentence(.)) %>%
  mutate(Prop = round(Prop, 2))

#Science
science_vars <- str_c("V19", 2:7)
science_overall <- data_subset %>%
  summarise_at(vars(science_vars),
               ~ mean(., na.rm = T)) %>%
  gather(key = var) %>%
  left_join(codebook) %>%
  rename_all( ~ str_to_sentence(.)) %>%
  mutate(Value = round(Value, 2))


################
# Other inputs #
################

countries <- data_subset$country %>% unique()
intro_text <-
  list(
    "This app lets readers explore data from the World Value Study (WVS).",
    "Select a country from the drop-down menu below and click between the tabs to explore the attitudes related to democracy, news consumption, and attitudes related to science.",
    "For more information on WVS, visit http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp"
  )

#############
# Shiny App #
#############

ui <- dashboardPage(
  dashboardHeader(title = "World Value Study"),
  
  ## Sidebar
  dashboardSidebar(
    sidebarMenu(
      selectInput("country",
                  label = "Select a country",
                  choices = countries),
      menuItem("Overview", tabName = "intro"),
      menuItem("Democracy", tabName = "democracy"),
      menuItem("News", tabName = "news"),
      menuItem("Science", tabName = "science")
    )
  ),
  
  ## Body content
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            h2("Overview"),
            map(intro_text, p)),
    
    
    tabItem(
      tabName = "democracy",
      h2("Views on democracy"),
      fluidRow(
        withSpinner(plotlyOutput("plot_dem")),
        h3("Views on democracy at country level"),
        withSpinner(dataTableOutput("tab_dem")),
        h3("Views on democracy overall in WVS"),
        withSpinner(dataTableOutput("tab_dem_overall"))
      )
    ),
    
    
    tabItem(
      tabName = "news",
      h2("News consumption"),
      fluidRow(
        withSpinner(plotlyOutput("plot_news")),
        h3("News consumption at country level"),
        withSpinner(dataTableOutput("tab_news")),
        h3("News consumption overall in WVS"),
        withSpinner(dataTableOutput("tab_news_overall"))
      )
    ),
    
    tabItem(
      tabName = "science",
      h2("Views on science"),
      fluidRow(
        withSpinner(plotlyOutput("plot_science")),
        h3("Views on science at country level"),
        withSpinner(dataTableOutput("tab_science")),
        h3("Views on science overall in WVS"),
        withSpinner(dataTableOutput("tab_science_overall"))
      )
    )
  ))
)



server <- function(input, output) {
  ## Country level data
  data <- reactive(data_subset %>%
                     subset(country == input$country))
  
  # Democracy views
  dem_country <- reactive({
    data() %>%
      summarise_at(vars(V228A:V228I),
                   ~ mean(., na.rm = T)) %>%
      gather(key = var) %>%
      left_join(codebook) %>%
      select(label, var, value) %>%
      mutate(value = round(value, 2)) %>%
      rename_all( ~ str_to_sentence(.))
  })
  
  # News consumption
  news_country <- reactive({
    data() %>%
      select(news_vars) %>%
      gather(key = var) %>%
      count(var, value) %>%
      group_by(var) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      mutate(
        value = as.factor(value),
        value = fct_relevel(value, "Daily", "Weekly", "Monthly") %>%
          fct_rev()
      ) %>%
      left_join(codebook) %>%
      mutate(prop = round(prop, 2)) %>%
      select(label, value, everything()) %>%
      rename_all( ~ str_to_sentence(.))
  })
  
  # Science views
  science_country <- reactive({
    data() %>%
      summarise_at(vars(science_vars),
                   ~ mean(., na.rm = T)) %>%
      gather(key = var) %>%
      left_join(codebook) %>%
      select(label, var, value) %>%
      mutate(value = round(value, 2)) %>%
      rename_all( ~ str_to_sentence(.))
  })
  
  
  # Tables
  output$tab_dem <- renderDataTable(dem_country())
  output$tab_news <- renderDataTable(news_country(),
                                     options = list(pageLength = 10))
  output$tab_science <- renderDataTable(science_country())
  
  # overall tables
  output$tab_dem_overall <- renderDataTable({
    dem_overall %>%
      select(Label, Var, Value) %>%
      mutate(Value = round(Value, 2))
  })
  output$tab_news_overall <- renderDataTable({
    news_overall %>%
      mutate(Prop = round(Prop, 2)) %>%
      select(Label, Value, everything())
  }, options = list(pageLength = 10))
  
  output$tab_science_overall <- renderDataTable({
    science_overall %>%
      select(Label, Var, Value) %>%
      mutate(Value = round(Value, 2))
  })
  
  # figures
  output$plot_dem <- renderPlotly(print(
    ggplotly(graph(dem_country(), dem_overall)) %>%
      layout(title = 'Blue = country; Black = overall')
  ))
  output$plot_science <- renderPlotly(print(
    ggplotly(graph(science_country(),
                   science_overall)) %>%
      layout(title = 'Blue = country; Black = overall')
  ))
  
  output$plot_news <- renderPlotly({
    print(
      ggplotly(
        news_country() %>%
          na.omit() %>%
          ggplot(aes(Value, Prop)) +
          geom_bar(stat = "identity") +
          geom_point(
            data = news_overall %>% na.omit(),
            stat = "identity",
            color = "blue"
          ) +
          facet_wrap(~ Label) +
          theme_bw() +
          coord_flip() +
          labs(y = "Proportion", x = "Frequency")
      ) %>%
        layout(title = list(text = '<sup> Blue = overall</sup>'))
    )
    
  })
}

shinyApp(ui, server)
