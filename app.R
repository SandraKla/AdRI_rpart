####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2024) #####################################
###################################################################################################

####################################### Load Script and Example-Dataset ###########################

source("rpart.R")

####################################### Libraries #################################################

if("dplyr" %in% rownames(installed.packages())){
  library(dplyr)} else{
    install.packages("dplyr")
    library(dplyr)}

if("ggplot2" %in% rownames(installed.packages())){
  library(ggplot2)} else{
    install.packages("ggplot2")
    library(ggplot2)}

if("reflimR" %in% rownames(installed.packages())){
  library(reflimR)} else{
    install.packages("reflimR")
    library(reflimR)}

if("rpart" %in% rownames(installed.packages())){
  library(rpart)} else{
    install.packages("rpart")
    library(rpart)}

if("rpart.plot" %in% rownames(installed.packages())){
  library(rpart.plot)} else{
    install.packages("rpart.plot")
    library(rpart.plot)}

if("shinydashboard" %in% rownames(installed.packages())){
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

####################################### User Interface ############################################

ui <- dashboardPage(
  dashboardHeader(title = "AdRI_rpart", titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebarid",
      
      div(
        style = "text-align:center",
        br(),
        "Tool for automated sex and age partitioning", br(), "for the estimation of reference intervals!"
      ),
      
      selectInput("dataset", "Select preinstalled dataset:", choice = list.files(pattern = c(".csv"), recursive = TRUE)),
      uiOutput("dataset_file"),
      actionButton('reset', 'Reset Input', icon = icon("trash")),
      checkboxInput("xlog", "Logarithmic scale for the x-axis", value = FALSE),
      hr(),
      
      numericInput(
        "tree_minsplit",
        "rpart: minsplit",
        20,
        min = 20,
        max = 100),

      numericInput(
        "tree_cp",
        "rpart: Complexity parameter",
        0.01,
        min = 0.01,
        max = 0.5),
      
      numericInput(
        "reflim_n.min",
        "reflimR: n.min",
        40,
        min = 30,
        max = 200),
      
      hr(),
      
      div(
        style = "text-align:center",
        "For further information visit our",
        a("GitHub site", href = "https://github.com/SandraKla/AdRI_rpart"),
        "or",
        br(),
        "read the publication",
        a("doi: ", href = ""),
        "!"
      )
    )
  ),
  
  dashboardBody(fluidRow(
    tabBox(
      title = "AdRI_rpart",
      id = "tabselected",
      width = 12,
      
      tabPanel(
        "Plot",
        icon = icon("calculator"),
      
        p("This Shiny App computes age groups using the regression tree model rpart, 
          and then calculates reference intervals based on them using reflimR."),
        
        plotOutput("plot", height = "700px")
      ),
      
      tabPanel(
        "rpart",
        icon = icon("tree"),
        
        p("This Shiny App computes age groups using the regression tree model rpart, 
          and then calculates reference intervals based on them using reflimR."),
        
        plotOutput("rpart_plot", height = "700px")
      )
    )
  ))
)

####################################### Server ####################################################

server <- function(input, output, session) {
  
  options(shiny.sanitize.errors = TRUE)
  
  ##################################### Reactive Expressions ######################################
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL)
  })
  
  values_lis <- reactiveValues(
    upload_state_lis = NULL
  )
  
  get_data_file <- reactive({
    
    input$xlog
    input$dataset
    
    # Read the data
    data_data <- read.csv2(input$dataset, header = TRUE, 
                           stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "") %>% na.omit()
    
    # Read the data (from the CALIPER study or from the generator)
    if(is.null(dataset_input()))
      {data_data <- read.csv2(input$dataset, header = TRUE, 
                             stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "") %>% na.omit()}
    if(!is.null(dataset_input()))
      {data_data <- read.csv2(dataset_input()[["datapath"]], header = TRUE, 
                             stringsAsFactors = FALSE, sep = ";", dec = ",", na.strings = "") %>% na.omit()}
    
    data_data$VALUE <- as.numeric(data_data$VALUE)
    return(data_data)
  })
  
  ##################################### Observe Events ############################################
  
  ##################################### Output ####################################################
  
  output$plot <- renderPlot({
    
    data_analyte <- get_data_file()
    
    tree_cp <- input$tree_cp
    tree_minsplit <- input$tree_minsplit
    
    if(tree_minsplit == 0 || tree_minsplit == "" || is.na(tree_minsplit)){
      tree_minsplit = 20
    }
    
    if(tree_cp == 0 || tree_cp == "" || is.na(tree_cp)){
      tree_cp = 0.01
    }
  
    sex_age_rpart <- rpart(VALUE ~ AGE_DAYS + SEX, data = data_analyte, control = rpart.control(cp = tree_cp,
                                                                                                minsplit = tree_minsplit))
    data_analyte$leaf <- sex_age_rpart$where
    
    #get the current analyte
    var_analyte <- data_analyte$ANALYTE %>% unique()
    
    # create ri plots
    gr <- ggplot(data_analyte, aes(x=AGE_DAYS, y=VALUE, color = SEX)) + geom_point() + theme_bw() + 
      ggtitle(var_analyte) + labs(x = 'Age (days)', y = var_analyte) + 
      scale_color_manual(values = c("F" = "indianred", "M" = "cornflowerblue"))
    
    if (input$xlog) {
      gr <- gr + scale_x_continuous(trans = 'log')
    }
    
    gr <- draw_ri_rpart(data_analyte, gr, sex = 'M', is_unisex = TRUE, xlog = input$xlog, reflim_n.min = input$reflim_n.min)
    gr <- draw_ri_rpart(data_analyte, gr, sex = 'M', is_unisex = FALSE, xlog = input$xlog, reflim_n.min = input$reflim_n.min)
    gr <- draw_ri_rpart(data_analyte, gr, sex = 'F', is_unisex = FALSE, xlog = input$xlog, reflim_n.min = input$reflim_n.min)
    
    print(gr)
  })
  
  output$rpart_plot <- renderPlot({
    
    data_analyte <- get_data_file()
    
    tree_cp <- input$tree_cp
    tree_minsplit <- input$tree_minsplit
    
    if(tree_minsplit == 0 || tree_minsplit == ""){
      tree_minsplit = 20
    }
    
    if(tree_cp == 0 || tree_cp == ""){
      tree_cp = 0.01
    }
    
    sex_age_rpart <- rpart(VALUE ~ AGE_DAYS + SEX, data = data_analyte, control = rpart.control(cp = tree_cp,
                                                                                                minsplit = tree_minsplit))
    
    #get the current analyte
    var_analyte <- data_analyte$ANALYTE %>% unique()
    
    rpart.plot(sex_age_rpart, box.palette = "RdBu", main = var_analyte)
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)