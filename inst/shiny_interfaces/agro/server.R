# PPBstats_interface 
# Agro server

server <- function(input, output) {
  
  # Design experiment ----------
  out_design_experiment = reactive(
    design_experiment(
      expe.type = input$design_experiment_expe.type,
      location = input$design_experiment_location,
      year = input$design_experiment_year,
      germplasm = input$design_experiment_germplasm,
      controls = input$design_experiment_controls,
      nb.controls.per.block = input$design_experiment_nb.controls.per.block,
      nb.blocks = input$design_experiment_nb.blocks,
      nb.cols = input$design_experiment_nb.cols,
      return.format = input$design_experiment_return.format
    )
  ) 
  
  output$design_experiment_plot = renderPlot({out_design_experiment$design})
  output$design_experiment_data = renderPlot({out_design_experiment$data.frame})
  
  # AMMI GGE ----------
  
  data("data_GxE")
  data_GxE = format_data_PPBstats(data_GxE, type = "data_agro")
  
  output$plot1 <- renderPlot({
    describe_data(
      data = data_GxE, 
      plot_type = input$plot_type, 
      x_axis = input$x_axis,
      in_col = input$in_col,
      vec_variables = input$vec_variables, 
      nb_parameters_per_plot_x_axis = input$nb_parameters_per_plot_x_axis,
      nb_parameters_per_plot_in_col = input$nb_parameters_per_plot_in_col,
      labels_on = input$labels_on,
      labels_size = input$labels_size
    )
  })
}

