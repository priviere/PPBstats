# PPBstats_interface 
# Agro ui

ui <- dashboardPage(
  dashboardHeader(title = "PPBstats"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Design experiment", tabName = "design_experiment", icon = icon("th")),
      menuItem("Format data", tabName = "format_data", icon = icon("th")),
      menuItem("Describe data", tabName = "describe_data", icon = icon("th")),
      menuItem("Family 1", icon = icon("th"),
               menuSubItem("Theroy", tabName = "family_1_theory", icon = icon("th")),
               menuSubItem("Run model", tabName = "family_1_run_model", icon = icon("th")),
               menuSubItem("Check model", tabName = "family_1_check_model", icon = icon("th"))
      ),
      menuItem("Family_2", tabName = "family_2", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "intro",
              h2("Introduction"),
              fluidRow(
                box("Bla bla bla")
              )
      ),
      
      tabItem(tabName = "design_experiment",
              h2("Design experiment"),
              fluidRow(
                box(
                  selectInput("design_experiment_expe.type", 
                              "expe.type", 
                              c("satellite-farm", "regional-farm", "row-column", 
                                "fully-repicated", "IBD")
                  ),
                  textInput("design_experiment_location", "location", ""),
                  textInput("design_experiment_year", "year", ""),
                  textInput("design_experiment_germplasm", "germplasm", ""),
                  textInput("design_experiment_controls", "controls", ""),
                  textInput("design_experiment_nb.controls.per.block", "nb.controls.per.block", ""),
                  textInput("design_experiment_nb.blocks", "nb.blocks", ""),
                  textInput("design_experiment_nb.cols", "nb.cols", ""),
                  selectInput("design_experiment_return.format", "return.format", c("standard", "shinemas"))
                  ),
                
                box(plotOutput("design_experiment_plot")),
                box(plotOutput("design_experiment_data"))
              )
      ),
      
      tabItem(tabName = "format_data",
              h2("Format data")
      ),
      
      tabItem(
        tabName = "describe_data",
        h2("Describe data"),
        fluidRow(
          box(
            selectInput("plot_type", 
                        "plot_type", 
                        c("pam", "histogramm", "barplot", "boxplot",
                          "interaction", "biplot", "radar", "raster")
            ),
            selectInput("x_axis", "x_axis", c("germplasm", "location", "year")),
            selectInput("in_col", "in_col", c("germplasm", "location", "year")),
            selectInput("vec_variables", "vec_variables", c("y1", "y2", "y3")),
            sliderInput("nb_parameters_per_plot_x_axis", "nb_parameters_per_plot_x_axis :", 1, 30, 15),
            sliderInput("nb_parameters_per_plot_in_col", "nb_parameters_per_plot_in_col :", 1, 30, 15),
            if(FALSE){ selectInput("labels_on", "labels_on", c("germplasm", "location", "year")) },
            if(FALSE){ selectInput("labels_size", "labels_size", c("germplasm", "location", "year")) }
          ),
          
          box(plotOutput("plot1"))
        )
      ),
      
      
      tabItem(tabName = "family_1_theory",
              h2("Theory")
      ),
      
      tabItem(tabName = "family_1_run_model",
              h2("Run the model"),
              
              fluidRow(
                box(
                  selectInput("variable", "variable", c("y1", "y2", "y3")),
                  selectInput("gxe_analysis", "gxe_analysis", c("AMMI", "GGE")),
                  submitButton("Run model")
                )
              )
              
      ),
      
      tabItem(tabName = "family_1_check_model",
              h2("Check the model")
      )
      
      
    )
    
  )
  
)


