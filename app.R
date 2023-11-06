library(shiny)
library(shinyWidgets)
library(dplyr)
library(shinythemes)
library(ggplot2)

# CA DoF data is processed in prep-data.R
pop_data <- readRDS("data/pop_data.rds")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      # Slider controling range of population
      sliderInput(
        inputId = "pop_range",
        label = "Population range (in millions)",
        # Set to the min and max in the data,
        # rounded down and up to the nearest million
        value = c(
          floor(min(pop_data$population)), 
          ceiling(max(pop_data$population))
          ),
        min = 0,
        max = 45
        ),
      # Slider controling range of years
      sliderInput(
        inputId = "year_range",
        label = "Date range",
        value = c(2020, 2040),
        min = min(pop_data$year),
        max = max(pop_data$year),
        sep = ""
      ),
      # Checkbox for fitlering groups from population
      checkboxGroupInput(
        inputId = "groups",
        label = "Population section",
        choices = c(
          "Total",
          "Juvenile",
          "18-25",
          "65+"
          ),
        selected = c(
          "Total",
          "Juvenile",
          "18-25",
          "65+"
          )
        ),
      # Color pickers for each group
      colourpicker::colourInput(
        inputId = "color_total",
        label = "Color for Total",
        value = "black"
        ),
      colourpicker::colourInput(
        inputId = "color_juvenile",
        label = "Color for Juvenile",
        value = "darkcyan"
        ),
      colourpicker::colourInput(
        inputId = "color_18_25",
        label = "Color for 18-25",
        value = "pink1"
        ),
      colourpicker::colourInput(
        inputId = "color_65_plus",
        label = "Color for 65+",
        value = "skyblue"
        ),
      # Checkbox crontrolling whether to plot population 
      # projections using dotted line
      checkboxInput(
        inputId = "lines",
        label = "Uncheck to make trend lines solid",
        value = TRUE
        ),
      # Button with link to source code on GitHub
      actionButton(
        inputId = "about",
        label = "Click for source code",
        onclick = "window.open('https://github.com/dcaldwellphd/ca-population', '_blank')"
      ),
      downloadButton("save_plot", "Download plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  # Plot, subtitle and caption to pass in ggplot
  title = "California Population Trends"
  subtitle = "Projections calculated by the California Department of Finance, incorporating the latest historical population, birth, death, and migration data available as of July 1, 2022."
  caption = "Source: State of California, Department of Finance. Demographic Research Unit. Sacramento, California. July 2023"
  
  # Automatically adds 1 unit to upper or lower slider when they are equal
  # Prevents slider inputs getting trapped behind one another
  observeEvent(input$year_range, {
    if (input$year_range[2] - input$year_range[1] == 0) {
      updateSliderInput(inputId = "year_range",
                        value = c(input$year_range[1]-1, input$year_range[2]+1)
      )
    }
  })
  observeEvent(input$pop_range, {
    if (input$pop_range[2] - input$pop_range[1] == 0) {
      updateSliderInput(inputId = "pop_range",
                        value = c(input$pop_range[1]-1, input$pop_range[2]+1)
      )
    }
  })
  # Filter data based on user inputs
  input_data <- reactive(
    pop_data |>
      filter(
        age_group %in% input$groups, 
        year >= input$year_range[1],
        year <= input$year_range[2],
        population >= input$pop_range[1],
        population <= input$pop_range[2]
      )
    )
  # Function generating plot output
  plot_input <- function() {
      p <- ggplot(
        input_data(),
        aes(
          x = year, 
          y = population,
          col = age_group
          )
        ) +
      geom_line(linewidth = 1) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(),
        expand = expansion(mult = c(0.01, 0.01))
        ) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(),
        expand = expansion(mult = c(0.01, 0.01))
        ) +
      # Allows user to set the color of each group in UI
      scale_color_manual(
        values = c(
          "Total" = input$color_total,
          "Juvenile" = input$color_juvenile,
          "18-25" = input$color_18_25,
          "65+" = input$color_65_plus
          ),
        breaks = c("Total", "Juvenile", "18-25", "65+"),
        name = "Population section: "
        ) +
      guides(linetype = "none") +
      labs(
        y = "Population (in millions)",
        title = title,
        subtitle = subtitle,
        caption = caption
        ) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        panel.border = element_rect(linewidth = 1.5, colour = "black"),
        axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.position = "right",
        plot.title = ggtext::element_textbox_simple(
          face = "bold", size = 19, margin = margin(b = 15)
          ),
        plot.subtitle = ggtext::element_textbox_simple(
          color="#444444", size = 12, margin = margin(b = 15)
          ),
        plot.caption = ggtext::element_textbox_simple(
          color = "#444444", size = 12, margin = margin(t = 15)),
        ) +
      # Allows user to set the range of the x and y axes in UI
      coord_cartesian(
        ylim = c(input$pop_range[1], input$pop_range[2]),
        xlim = c(input$year_range[1], input$year_range[2])
      )
      # Allows user to control whether to plot projections 
      # as dotted lines in the UI
      if (input$lines) {
        p <- p +
          aes(linetype = fig_type) +
          scale_linetype_manual(
            values = c(
              "estimate" = "solid",
              "projection" = "dotted"
            )
          )
      }
      
      print(p)
    
  }

  # Render the plot and set its dimensions
  output$plot <- renderPlot({
    plot_input()
  }, width = 650, height = 500)
  
  output$save_plot <- downloadHandler(
    filename = "shinyplot.png",
    content = function(file) {
      ggsave(
        file,
        plot_input(),
        width = 10,
        height = 7.5,
        units = "in",
        device = png
        )
    })    
  
}

# Run the application 
shinyApp(ui = ui, server = server)



