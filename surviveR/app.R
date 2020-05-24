library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(survminer) # for KM plotting
library(survival)  # for KM plotting
library(DT) #fancy data tables in shiny
library(data.table) # mostly for fread
library(shinyjs) #JavaScript control in shiny (such as disabling buttons)




ui <- dashboardPage( 

  dashboardHeader(title="surviveR"), 
  dashboardSidebar( 
    # create sidebar menus
    sidebarMenu( id="sidebar", # id is needed for the link button
                 menuItem("Introduction", tabName="intro", icon=icon("info")),
                 menuItem("Data input", tabName="input", icon=icon("file-upload")),
                 menuItem("KM survival curve", tabName="results", icon=icon("chart-area"))
    )
  ),
  dashboardBody(
    titlePanel(tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon-32x32.png"),
                         tags$title("2"))),

    useShinyjs(), #enable shinyjs() functions
    #tab from the sidemenu
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(img(src = "logo.png", height = 162, width = 250), width=12, HTML('<hr style="color: black;">'),h1('Introduction'), 
                    p('The surviveR app has been developed by the FGG group in Queens University Belfast to easily visualise survival data using Kaplan Meier graphs through R. This app is part of cracR home made package, a platform including more shiny apps to analyse, annotate and visualize various datasets.'),
                    h3('1. Data Input'),
                    p("The data input is a csv table containing the lifetime parameter, the survival outcome and the optional stratification values in columns, while the patients are stored in rows.
                      After uploading the table, the user has to select the appropriate columns for the descriptors using the dropdown menus, and then the symbol for the outcome has to be defined."),
                    h3('2. Kaplan-Meier curve '),
                    h4('2.1 Survival function'),
                    p('The Kaplan Meier curve is a survival function that describes patient survival over time. It is using non-parametric statistic, meaning that it is not based on the assumption of an underlying probability distribution (survival data has a skewed distribution). This statistic gives the probability that an individual patient will survive past a particular time t. At t = 0, the Kaplan-Meier estimator is 1 and with t going to infinity, the estimator goes to 0. '),
                    p('It is further based on the assumption that the probability of surviving past a certain time point t (S(t)) is equal to the product of the observed survival rates until time point t (p.t). '),
                    p(pre('S(t) = p.1 * p.2 * … * p.t', style="text-align:center")),
                    p('The generated Kaplan-Meier curve can be downloaded as a pdf file by clicking on the download button.'),
                    h4('2.2 Risk and Cumulative Event table'),
                    p('After plotting the surviveR curve, the risk and the cumulative event table is shown to the right. The first contains the number of patients at risk (number of patients alive) at a given time point, while the cumultaive number of events shows how many patient deaths were registered until the shown time point.'),
                    p('The visualized tables can be downloaded as a pdf image file using the download button.'),
                    h4('2.3 Stratification'),
                    p('After the primary selection, it is possible to stratify the patients based on subgroups, using categorical values if it is already provided in the uploaded table. By selecting the column, after pressing the ‘Go’ button, the graph will be stratified by the different subgroups defined in the given column. It is possible to select the distinct subgroups to draw or selecting ‘rest’ to merge the non selected subgroups into one.'),
                    h3('3. Statistics'),
                    h4('3.1 Log-rank test'),
                    p('The log rank test is a non-parametric test performed without survival distribution assumptions. The log rank test compares the observed number of events in each group to what would be expected if the null hypothesis were true (i.e., if the survival curves were identical). The log rank statistic is approximately distributed as a chi-square test.'),
                    p('The pairwise p-values can be downloaded az a comma separated file by clicking the download button.'),
                    h4("3.2 COX proportional hazards regression model"),
                    p('The hazard regression model can be used to examine how specified factors (x) influence the rate of a particular event happening (patient death) at a particular point in time. This rate is commonly referred as the hazard rate. The Cox model is expressed by the hazard function denoted by h(t). Briefly, the hazard function can be interpreted as the risk of dying at time t, but it is not a cumulative probability in time. It can be estimated as follow:'),
                    p(pre('h(t)=h(t.0)×exp(b.1*x.1+b.2*x.2+...+b.p*x.p)', style="text-align:center")),
                    p('where h0 is the baseline hazard, (hazard value when all covarietes (x) are equal to zero) and b.n is the weight of each covariants. The Hazard Ratio (HR) will describe the ratio of the risk of outcome in one group versus the risk of outcome in another group and it can be calculated as,'),
                    p(pre('HR=[risk of outcome group.1]/[risk of outcome group.2]=log(b.n)', style='text-align:center')),
                    p('This means that if HR is equal to 1, the effect is independent of the associated covariate(b.n), if HR<1 there is a reduction in the hazard and if HR>1 there is an increase in hazard. The used algorithm will calculate the Cox proportional hazards model where the upper and lower 95% confidence intervals for the hazard ratio (exp(coef)) is reported, and the first group is used as reference (HR=1).'),
                    p('Also global statistics are reported, such as the Concordance, which describes the goodness-of-fit in survival models (should be above 0.55 te be at least a decent fit). The Global p-values includes three tests: likelihood-ratio test, Wald test, and Score (log-rank) test. These three methods test the null hypothesis that all beta values are zero and are asymptotically equivalent.'),
                    p('The generated graph of the COX regression model can be downloaded as a png image file by clicking on the download button.'),
                    h3('4. Demo table'),
                    p('The provided Demo file contains the datasets available on Gene Expression Omnibus, under GSE39582 accession number. For easier access, the column containing time points is renamed ‘time’ and the outcome has been change to ‘Event’. In the latter column dead patients are denoted as 1, living patients marked with 0. These are pre-selected when the "Demo" button is clicked.'),
                    h3('5. Contact'),
                    p('The app is still under development, if you have any question or suggestions, please contact me at ', a('t.sessler@qub.ac.uk',href='mailto::t.sessler@qub.ac.uk'),'.')
                    ))),
      
      tabItem(tabName="input",
              fluidRow(
                column(6,
                #file upload
                fluidRow(
                box(title = "File upload", solidHeader = T, collapsible = T, width=12,
                    fluidRow(
                      column(6,
                             "Submit a file",br(),
                             fileInput("file1", label=NULL, multiple = F,
                                       accept=c("text/csv","text/comma-separated-values,text/plain",".csv"))),
                      # submit button 
                      column(2, "", br(),
                             actionButton("submit", "Submit"))),
                    #demo button
                    column(3, "Use a Demo file", br(),
                           actionButton("demo", "Demo"))
                )),
                fluidRow(
                  box(title = "Select columns containing event time and patient outcome", solidHeader=T, collapsible=T, width=12,
                      uiOutput("col"),
                      h4("Select patient outcome description"),
                      uiOutput("liveDead")
                  ))
                ),
                box(title = "Instructions", solidHeader = T, collapsible = T,
                    "1. Upload a table containing the data to visualize or chose Demo for a pre-existing data file (see introductions for description).", br(),br(),
                    "2. Select which column contain the event time and the patient outcome.", br(),br(),
                    "3. Select how the patient outcome is determined in the selected column (Dead/Alive)", br(),br(),
                    "4. In Additional options it is possible to change the time units and/or filter the database prior graphing.",br(),
                    "The pre-set time units in SurviveR is month. In case the dataset contains different time frame or other time units is preferred 
                    to be shown on the KM plot, please tick the boxes and select the preferred time units in the drop down menus.",br(),
                    "In case some of the patients needs to be excluded, it is possible to pre-filter the data prior graphing.
                    After ticking the box and selecting a desired column, in the appearing box it is possible to select the desired categorical 
                    values to be included in the KM plots. (Note that further filtering and multiple level of straticition will be available.)",br(),br(),
                    "5. Click the 'Graph Me' button to visualise the graph and for further options."
                )
              ),
              
              # event time, outcome and Live/Dead selection and options (time frame and pre-filtering)
              fluidRow(
                column(6,
                       fluidRow(
                         box(title = "Additional options", solidHeader=T, collapsible=T, width=12,
                             checkboxInput("timeOption", label="tick the box to change the time parameters"), 
                             uiOutput("changeTime"),
                             checkboxInput("filterOption", label="tick the box to filter the input table"),
                             fluidRow(
                               uiOutput("preFiltering")
                               ),
                             fluidRow(uiOutput("add_filter"))
                             )),
                       fluidRow(
                         box(title = "Submit the selection", solidHeader=T, collapsible=T, width=12,
                             actionButton("calc", "Graph Me"),
                             uiOutput("error1")))
                ),
                # input table
                
                box(title = "Submitted table", solidHeader = T, collapsible = T, width=6,
                    div(style = "overflow-x: scroll", DT::dataTableOutput("table")))
              )      # add scrollin
      ), # end of tab Data input
      
      tabItem(tabName="results", 
              fluidRow(
                column(6,
                       fluidRow(box(title="Instructions", solidHeader=T, collapsible=T, width=12,
                                    "Endpoint: the displayed maximum observation time point can be changed using the slider.",br(),br(),
                                    "Stratification/filtering: a column from the uploaded table can be selected to display the differences",
                                    "between the contained subgroups or individual subgroups can be selected.",
                                    "By selecting 'All', no startification will be done, 'Rest' will draw an additional line rapresenting the non selected groups together.",
                                    br(),"The subgroups will be visualised after pressing the 'go' button."
                       )),
                       fluidRow(box(title="settings", solidHeader=T, collapsible=T, width=12,
                                    #to add a slider
                                    sliderInput(inputId="setKMtime", label="graph endpoint", value=60, min=1, max=60, step=1),
                                    textInput(inputId="title", label="graph title", placeholder = "insert graph title here")
                       ))),
                
                column(6,
                       fluidRow(box(title="filtering", solidHeader=T, collapsible=T, width=12,
                                    uiOutput("subset1"),
                                    uiOutput("subset2"))),
                       
                       fluidRow(box(title="stratification", solidHeader=T, collapsible=T, width=12,
                                    uiOutput("strata1"),
                                    uiOutput("strata2"),
                                    uiOutput("strata3"))))),
              fluidRow(
                box(title=NULL, solidHeader=T, collapsible=T, width=6,
                    plotOutput("KM"),
                    downloadButton('saveKM', '')),
                
                box(title=NULL, solidHeader=T, collapsible=T, width=6,
                    plotOutput("riskCumTable"),
                    downloadButton('saveRiskCum', ''))),
              
              fluidRow(
                box(title="COX proportional hazard regression model", solidHeader=T, collapsible=T, width=6,
                    uiOutput("COXselect"),
                    plotOutput("CHR"),
                    downloadButton('saveCOX', '')),
                
                column(6,
                       fluidRow(box(title="pairwise log-rank test", solidHeader=T, collapsible=T, width=12,
                                    verbatimTextOutput("logRank_test"),
                                    downloadButton('saveLogRank', ''))),
                       fluidRow(box(title = "Submitted table", solidHeader = T, collapsible = T, width=12,
                                    div(style = "overflow-x: scroll", DT::dataTableOutput("table2"))))))

      ) # end of tab results
      
    )),
  
  #color scheme change
  tags$head(tags$style(HTML("/* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #D6000D;
                            }
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #D6000D;
                            }
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #D6000D;
                            }
                            /* toggle button when hovered (=)  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #000000;
                            }
                            /* make button red line when clicked  */
                            .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a{
                            border-left-color: #D6000D;
                            ")))
  ) # end of ui


server <- function(input, output, session) {  
  # will create a list for storing the reactive values
  values <- reactiveValues(upload = NULL,   # contains the table pre-filtering
                           input = NULL,    # contains the loaded table after Graph Me
                           table = NULL,    # table AFTER filtering
                           columns = NULL,  # contains the colnames of the uploaded table
                           demo = NULL,     # turns to 1 if demo has been selected
                           liveDead = NULL, # contains the column names were the Outcome is stored
                           time = NULL,     # contains the column name of the Event time
                           outcome = NULL,  # contains the column name of type of outcome 
                           live = NULL,     # value denoting Live patients
                           dead = NULL,     # value denoting Dead patients
                           number = c(1),      # stores how many times the "add filtering" button had been clicked
                           strat = NULL,    # stratification of the survival analysis
                           filter = 0,      # 1 if DB filtering required for strata
                           strata3=NULL,    # helps ori.Table() not to react to input$strata3
                           stratName=NULL,  # will create the vector containing the categorical values for double stratification
                           strata1=NULL,    #contain the previously selected strata1 value
                           strata2=NULL,    #contain the previously selected strata2 value
                           sub1=NULL,       #contain the previously selected subset1 value
                           sub2=NULL,       #contain the previously selected subset2 value
                           go = 0)          # value that changes after button clicks (Graph Me, strata Go)
  
  ### DEMO upload
  observeEvent(input$demo,{
    #Load demo files
    demoTable<-read.csv('www/demo.csv', header = TRUE, sep=",")  # load the demo file
    values$go <- 0 # to reset the graphing settings (if there is a loaded graph, SurviveR will crash)
    values$upload <- demoTable  # add the input to a reactive value 
    values$columns<- colnames(demoTable) # store the colnames in reactive value for the selection of Event time and outcome
    values$demo<-1
    #values$number<-1
  })
  
  ### File upload
  observeEvent(input$submit,{  # file upload will be executed only if the submit button is clicked
    req(input$file1)
    inputText<-NULL
    inputSelect<-NULL
    #values$number<-0
    values$go<-0 # to reset the graphing settings (if there is a loaded graph, SurviveR will crash)
    values$upload <- data.frame(fread(input$file1$datapath)) # add the input to a reactive value 
    values$columns <- colnames(values$upload) # store the colnames in reactive value for the selection of Event time and outcome
    values$demo<-NULL
  })
  
  # For Event time and patient outcome selection
  output$col <- renderUI({
    if(is.null(values$columns))
      h5("no file loaded")
    else
      if(is.null(values$demo)) # file has been selected
        fluidRow(
          column(6, selectInput("time","Event time", c(select_time="", values$columns))),           
          column(6, selectInput("outcome", "Event outcome", c(select_outcome="",values$columns))))
    else
      fluidRow(
        column(6, selectInput("time","Event time", selected="time", c(values$columns))),           
        column(6, selectInput("outcome", "Event outcome", selected="Event",c(values$columns))))})
  
  #time unit selection
  output$changeTime<-renderUI({
    
    if(input$timeOption==T){
      if(is.null(values$upload)){ h5("please upload a table")}
         else{
      fluidRow(
        column(2),
        column(3, selectInput("fromTime", "time units in table", c(month="m", year="y", day="d"))),
        column(2),
        column(3, selectInput("toTime", "time units to graph", c(month="m", year="y", day="d"))),
        column(2))
    }
  }})
  
  # Determine Live Dead category
  # load in the options based on previous selection
  observeEvent(input$outcome,{if(input$outcome!="")
    values$liveDead<- unique(values$upload[,input$outcome])
  values$outcome<-input$outcome      # store the selected value
  })
  observeEvent(input$time,{
    values$time<-input$time       # store the selected value
  })
  #select what is used to describe patient outcome
  output$liveDead <- renderUI({
    if(is.null(values$liveDead))
      h5("no column for patient outcome has been selected yet")
    else
      if(is.null(values$demo)) # file has been selected
        fluidRow(
          column(6, selectInput("live", "Live patients", c(live="",values$liveDead))),
          column(6, selectInput("dead", "Dead patients", c(dead="",values$liveDead))))
    else     # DEMO has been selected
      fluidRow(
        column(6, selectInput("live", "Live patients", selected = "0", c(values$liveDead))),
        column(6, selectInput("dead", "Dead patients", selected = "1", c(values$liveDead))))})
  
  observeEvent(input$live,{
    values$live<-input$live    # store the selected value
  })
  observeEvent(input$dead,{
    values$dead<-input$dead    # store the selected value
  })
  # input table 
  output$table<- DT::renderDataTable(values$upload, option=list(searching=F, pageLength=5))
  output$table2<-DT::renderDataTable(values$upload, option=list(searching=F, pageLength=5))  # tables have to have different output ID's, even if the same table axists on 2 diff places
 
  #####PRE-FILTERING 

  
  # table pre-filtering selection
  observe({
  output$preFiltering<-renderUI({
    if(input$filterOption==T){  #if checkbox is ticked
      if(is.null(values$columns)){
        column(12,h5("please upload a table"))}
      else{
            rows <- 
              lapply(values$number, function(i){
              fluidRow(
                column(6,  selectInput(paste0('drop',i),
                                       label = "",
                                        choices = colnames(values$upload),
                                        selected = input[[paste0("drop",i)]]
                                       )),
                column(6,  selectInput(paste0('select',i),
                                       label = "",
                                        choices = values$upload[1],
                                       multiple = TRUE,
                                       selectize = TRUE))
              ) # end of FluidRow
            }) # end of lapply
            do.call(shiny::tagList, rows)
            #}) # end of renderUI
        }
    } # end of tick box on
  })
  }) #end of observe (renderUI)
  
  #update the search fields with previous selection
  observeEvent(lapply(paste0("drop", values$number), function(x) input[[x]]), {
    if (length(values$number)>0){
      for (i in values$number) {
        updateSelectInput(session,
                          paste0('select', i),
                          choices = values$upload[input[[paste0('drop', i)]]],
                          selected = input[[paste0("select",i)]])}} })
  
  output$add_filter<-renderUI({
    if(input$filterOption==T & !is.null(values$columns)){
      column(12, actionButton(inputId = "add", label = "add another field"),
             actionButton(inputId = "reset", label = "Reset"))
    }
  })
  
  #pressing the add button
  observeEvent(input$add, {
    values$number <- c(values$number, max(values$number)+1)
    cat("adding to c")
  })
  
  #pressing the reset button
  # observeEvent(input$reset,{
  #   values$input<-values$upload
  #   values$number <- c(1)})
  
  ##### upon pressing the 'Graph Me' button
  observeEvent(input$calc,{
    if(is.null(values$upload))
      output$error1 <- renderUI({h5("please load a file or select the Demo table")}) else
    if(input$time=="")
      output$error1 <- renderUI ({h5("please select Event time column")}) else
    if(!is.numeric(values$upload[,input$time]))
      output$error1 <- renderUI ({h5("please select numeric values for event time")}) else
    if(values$outcome=="") 
       output$error1 <- renderUI ({h5("please select patient outcome column")}) else
    if(input$time==input$outcome)
       output$error1 <- renderUI ({h5("please select different columns for event time and patient outcome")}) else
    if(input$live=="") 
       output$error1 <- renderUI ({h5("please select descriptor for Live patients")}) else
    if(input$dead=="")
      output$error1 <- renderUI ({h5("please select descriptor for Dead patients")}) else
    if(input$live==input$dead)
      output$error1 <- renderUI ({h5("descriptor for Live and Dead patients can't be the same")})
    else{
      updateTabsetPanel(session, "sidebar", selected="results") # Jump to the results tab
      
      # apply pre-filtering 
      values$input <- NULL
      for (i in 1:length(values$number)) {
        if(!is.null(input[[paste0("select",i)]])) {
          if(is.null(values$input)) {
            values$input <- filter(values$upload, values$upload[,input[[paste0("drop",i)]]] %in% input[[paste0("select",i)]])
          }
          else {
            values$input <- filter(values$input, values$input[,input[[paste0("drop",i)]]] %in% input[[paste0("select",i)]])
          }
        }
      }
      if (is.null(values$input)) { values$input <- values$upload }
      
      
      # will allow the graph to be plotted
      values$table<-values$input
      values$go<-1
      output$error1 <- renderUI ({}) # will cancel any error message at calc button
  
  }}) #end of input "calc" button


# extract the time and change the time unit depending the selected option
  KMtime<-reactive({if(values$go!=0){
    if(input$timeOption==F){
      return(values$table[,values$time])}
    else{
      req(input$fromTime)
      req(input$toTime)
      if (input$toTime=="m" & input$fromTime=="m"){return(values$table[,values$time])}
      if(input$toTime=="m"){
        if (input$fromTime=="y"){return(values$table[,values$time]*12)}
        if (input$fromTime=="d"){return(values$table[,values$time]/30.5)}}
      if(input$toTime=="y"){
        if (input$fromTime=="d"){return(values$table[,values$time]/365)}
        if (input$fromTime=="m"){return(values$table[,values$time]/12)}
        if (input$fromTime=="y"){return(values$table[,values$time])}}
      if(input$toTime=="d"){
        if (input$fromTime=="d"){return(values$table[,values$time])}
        if (input$fromTime=="m"){return(values$table[,values$time]*30.5)}
        if (input$fromTime=="y"){return(values$table[,values$time]*365)}}
        }
    }})
# set the initial maximum to 5 year (display 5 year survival as start)
observe({ 
  time5year<-reactive({ if(input$timeOption==F){return(60)}
                      else{
                        req(input$toTime)
                        if(input$toTime=="d"){return(1800)}
                        if(input$toTime=="m"){return(60)}
                        if(input$toTime=="y"){return(5)}}
                    })
  updateSliderInput(session, "setKMtime", value = time5year())})
 
#set the label of the graph depending on the selected time unit
timeLabel<-reactive({ if(input$timeOption==F){return("Time [month]")}
                      else{
                        req(input$toTime)
                        if(input$toTime=="d"){return("Time [days]")}
                        if(input$toTime=="m"){return("Time [month]")}
                        if(input$toTime=="y"){return("Time [years]")}}
                    })

  # change the maximum value of the slider to the highest time point
  observe({    # Control the max value of the slider - depends on the input
    val <- max(KMtime(), na.rm = T)
    updateSliderInput(session, "setKMtime", max = val)})
  
  # change the max time point based on the slider input
  maxKMtime<-reactive({if(values$go!=0)ifelse(KMtime()>input$setKMtime,input$setKMtime, KMtime())})
  
  # transform the Event outcome column to 0=alive, 1=dead, depending on the setKMtime
  outcome<-reactive({if(values$go!=0){
    ifelse(KMtime()>input$setKMtime,0,
    ifelse(values$table[,values$outcome]==values$dead,1,
           ifelse(values$table[,values$outcome]==values$live,0,NA)))}})
  
  
    ##### SUBSETTING
  #subset1 -> select a column
  output$subset1 <- renderUI({
    if(values$go != 0){   #strata1 will be visible as soon as the input is loade
      if (is.null(values$sub1))
        fluidRow(column(12,selectInput("subset1", "select column for subsetting", c(subsetting="",NO_SUBSECTION=1, values$columns))))
      else
        fluidRow(column(12,selectInput("subset1", "select column for subsetting", selected=values$sub1 , c(NO_SUBSECTION=1, values$columns))))
    }})
  #subset2 -> select the categorical value(s)
  output$subset2 <- renderUI({
    if(input$subset1==1)
    {actionButton("goSub", "subset")}# if subset1 is not selected or All is selected
    else
      if (input$subset1!="")
      {fluidRow(column(12,selectInput("subset2","group selection",
                                      c(na.omit(unique(as.vector(values$input[,input$subset1])))), multiple=TRUE, selectize=TRUE),
                       actionButton("goSub", "subset")))}
  })
  
  ## Subset filtering
  observeEvent(input$goSub,{
    if (input$subset1==1) {values$sub1<- NULL} else {values$sub1<- input$subset1}
    if (input$subset1==1 )  # if All is selected -> no filtering  | (input$subset!=1 & is.null(input$subset2))
    {values$table <- values$input
     click("goStrata") 
      }
    else
     if (!is.null(input$subset2)) 
     {values$table <- filter(values$input, values$input[,input$subset1] %in% input$subset2)
       click("goStrata")}
  })
  
  #####STRAT renderUIs
  # contains the first strata selection and button
  output$strata1 <- renderUI({
    if(values$go != 0){   #strata1 will be visible as soon as the input is loaded
      if (is.null(values$strata1)) # if something has been selected previously, used the stored the values as preselection
        fluidRow(column(12,selectInput("strata1", "select column for stratification", c(stratifier="",NO_STRATIFICATION=1, values$columns))))
      else
        fluidRow(column(12,selectInput("strata1", "select column for stratification", selected = values$strata1 , c(NO_STRATIFICATION=1, values$columns))))
    }})
  
  #strata2 for second stratification
  output$strata2 <- renderUI({
    #show strata 2 as soon as something is selected on strata1 (except all)
    if(input$strata1=="" | input$strata1==1) {return()}
    else
    {if (is.null(values$strata2))
      fluidRow(column(12,selectInput("strata2", "select column for stratification", c(stratatifier="", values$columns))))
      else
        fluidRow(column(12,selectInput("strata2", "select column for stratification", selected = values$strata2 , c(NO_STRATIFICATION=1, values$columns))))
    }})
  
  # group strata3 visible when strata1 is selected
  output$strata3 <- renderUI({
    # if nothing is selected at strata1
    if(input$strata1!=""){
      if(input$strata1==1){ # if All is selected -> no strata3
        fluidRow(column(12, actionButton("goStrata", "go")))}  #button that will send the strata information to the graph
      else{
        # if something BUT All was selected at strata1 AND strata2 is NOT selected or All is selected at strata2
        if(input$strata2=="" | input$strata1==input$strata2 | input$strata2==1) # if strata2 is not selected or strata1 = strata2
        {fluidRow(column(12,selectInput("strata3","group selection",
                                        c(na.omit(unique(as.vector(values$table[,input$strata1]))), rest="rest" 
                                        ), multiple=TRUE, selectize=TRUE),
                         #button that will send the strata information to the graph
                         actionButton("goStrata", "go")))}
        else{ #there is an error message prior input because input$strata1 does not exist
          fluidRow(column(12,selectInput("strata3","group selection",
                                         c(as.vector(outer(na.omit(unique(values$table[,input$strata1])), 
                                                           na.omit(unique(values$table[,input$strata2])), paste, sep=" || ")), rest="rest" 
                                         ), multiple=TRUE, selectize=TRUE),
                          #button that will send the strata information to the graph
                          actionButton("goStrata", "go")))}}
    }})  
  
  ####     STRATA  
  #button for stratification will change the values$strat that goes into strat() 
  #values$filter if 0 no filtering, if one, filtering required
  observeEvent(input$goStrata,{
    if (input$strata1==1) {values$strata1<- NULL} else {values$strata1<- input$strata1}
    if (!is.null(input$strata2)){
      if (input$strata2=="" | input$strata2==1 | input$strata1==1) {values$strata2<- NULL} else {values$strata2<- input$strata2}}
    values$strata3<-input$strata3
    
    # if "all" is selected 
    if (input$strata1==1) 
    {values$filter<-0
    return(values$go<-1)} # if "All" is selected at strata1 input, it should be treated as go<-1 (no strata)
    
    values$go<-2
    #if strata2 is not selected or strata2 == strata1
    if (is.null(input$strata2) | input$strata2=="" | input$strata2==1 | input$strata1==input$strata2){
      # if no strata3 is selected
      if (is.null(input$strata3) )
      {values$strat<-as.vector(values$table[,input$strata1])
      values$filter<-0}
      else{ # 1 or more strata3 is selected
        # only one strata3 subgroup was selected
        if(length(input$strata3==1))
          # only "rest" was selected treat as plot all (no strata)
        {if(input$strata3=="rest") 
        {values$filter<-0 
        return(values$go<-1)} 
          else # only 1 subgroup was selected show only that group -> filter the originalTable
          {values$filter<-1
          values$strat<-as.vector(values$table[,input$strata1])}  # filter at OriginalTable, keep the strata column
        }
        # if in strata 3 something else AND 'rest' was selected
        if(length(input$strata3>1)){
          values$strat<-ifelse(as.vector(values$table[,input$strata1]) %in% input$strata3 |
                                 is.na(as.vector(values$table[,input$strata1])) ,as.vector(values$table[,input$strata1]),"rest")
        }}} # end of no strata2 selection
    else{ # strata2 is selected
      # if no strata3 is selected
      if (is.null(input$strata3))
      {values$strat<-as.vector(ifelse(is.na(values$table[,input$strata1]) | is.na(values$table[,input$strata2]), NA,
                                      paste(values$table[,input$strata1], values$table[,input$strata2], sep=" || ")))
      values$filter<-0}
      else{ # something is selected in strata3
        if(length(input$strata3==1))
          # only "rest" was selected treat as plot all (double strata)
        {if(input$strata3=="rest") 
        {values$strat<-as.vector(ifelse(is.na(values$table[,input$strata1]) | is.na(values$table[,input$strata2]), NA,
                                        paste(values$table[,input$strata1], values$table[,input$strata2], sep=" || ")))
        values$filter<-0} 
          else # only 1 subgroup was selected show only that group -> filter the originalTable
          {values$strat<-as.vector(ifelse(is.na(values$table[,input$strata1]) | is.na(values$table[,input$strata2]), NA,
                                          paste(values$table[,input$strata1], values$table[,input$strata2], sep=" || ")))
          values$filter<-1}}  # filter at OriginalTable, keep the strata column
        # if in strata 3 something else AND 'rest' was selected
        if(length(input$strata3>1)){
          values$stratName<-as.vector(ifelse(is.na(values$table[,input$strata1]) | is.na(values$table[,input$strata2]), NA,
                                             paste(values$table[,input$strata1], values$table[,input$strata2], sep=" || ")))
          values$strat<-ifelse(values$stratName %in% input$strata3, values$stratName, "rest")}
      }}
  }) # end of input$go strata
  
  
  # vector for stratification 1-> after the data input, 2-> strata selection
  strat<-reactive({if(values$go==1){ rep(1, length(maxKMtime))}
    else {
      if(values$go==2){values$strat}}})
  
  
  ###### table input for ggsurv
  originalTable<-reactive({
    if(values$go!=0 & values$filter==0 )
    {data.frame(outcome=outcome(), maxKMtime=maxKMtime(), strat=strat())}
    else
      if(values$go!=0 & values$filter==1) {
        # to avoid the error message if strata3 is empty
        if(length(values$strata3)==0)
        {return(data.frame(outcome=outcome(), maxKMtime=maxKMtime(), strat=strat()))}
        else
          return(filter(data.frame(outcome=outcome(), maxKMtime=maxKMtime(), strat=strat()), strat %in% values$strata3))

      }
  })
  
  fit <- reactive({
    if(values$go != 0){
      fit<-do.call(survfit, list(formula=Surv(maxKMtime, outcome) ~ strat, data=originalTable()))}})

  # strata legend for the graph
  legends<- reactive({
    legend.labs<- levels(originalTable()$strat)[levels(originalTable()$strat) %in% unique(originalTable()$strat)]

  })

  # only show the p value when showing more then 1 group.
  pv<-reactive({if (length(unique(originalTable()$strat))<2) F else T })
  
  #####KM curve
  KMplot<-function(){ggsurvplot(fit()
                                #,conf.int=T              # would add the confidence intervall on the graph, but in case n>2, it's messy
                                , legend.title=""
                                , legend.labs=legends()   #
                                , pval=pv()               # adds a log-p value in case strata is on
                                , title=input$title
                                , xlab=timeLabel()
                                , palette = "jco"

  ) 
    #+ guides(col=guide_legend(nrow=2))     # legends in 2 row
  }

  output$KM<-renderPlot({KMplot()})

  output$saveKM <- downloadHandler(
    filename = "SurviveR_KMplot.pdf" ,
    content = function(file) {
      ggsave(file, plot=print(KMplot(), newpage = F), device="pdf")
    })
  
  
  ##### Risk table and cumulative number of events
  riskCumPlot<-function(){ggarrange(
    ggrisktable(fit(), risk.table.title = "", ylab = "risk table"
                , legend.labs=legends()
                )+
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank()),
    ggcumevents(fit(), cumevents.title = "", ylab = "cumulative events", xlab=timeLabel() 
                , legend.labs=legends()
                ),
    nrow=2)}

  output$riskCumTable<-renderPlot({riskCumPlot()})

  output$saveRiskCum <- downloadHandler(
    filename = "SurviveR_RiskCumEvTable.pdf" ,
    content = function(file) {
      ggsave(file, plot=riskCumPlot(), device="pdf")
    })
  
  ###### COX HAZARD RATIO
  #ref select
  output$COXselect <- renderUI({
    if(values$go != 0){
      fluidRow(column(12,selectInput("COXselect", "select value for reference", c(levels(originalTable()$strat)))))
    }})

  COXtable<-reactive({data.frame(outcome=originalTable()$outcome, maxKMtime=originalTable()$maxKMtime,
                                 strat= relevel(originalTable()$strat, ref=input$COXselect)
                    )})

  #COX Hazard Ratio model
  chr<-reactive({
    if(values$go != 0 & length(levels(originalTable()$strat))>1){
      do.call(coxph, list(formula=Surv(maxKMtime, outcome) ~ strat, data= COXtable()))}})
  #COX plot
  COXplot<-function(){ggforest(chr(), fontsize=1)}

  output$CHR<-renderPlot({if(values$go != 0 & length(levels(originalTable()$strat))>1) COXplot()})

  output$saveCOX <- downloadHandler(
    filename = "SurviveR_COXplot.png",
    content = function(file) {
      ggsave(file, print(COXplot()),  device = "png")
    })


  # log-rank test output
  logRank <- function(){pairwise_survdiff(Surv(maxKMtime, outcome) ~ strat, data=originalTable())$p.value}

  output$logRank_test<-renderPrint({ logRank() })

  output$saveLogRank <- downloadHandler(
    filename = "SurviveR_LogRankTest.csv",
    content = function(file) {
      write.csv(logRank(), file)

    })

  
} # end of server

shinyApp(ui = ui, server = server)

# to publish the app on the shinyapps.io webpage (un: drfluff pw:same) C:/DOCS/R/KM shiny
#  library(rsconnect)
# rsconnect::setAccountInfo(name='drfluff',
#                           token='ADDD654AE5E3E9DCD0ED0FC7CFF94E15',
#                           secret='oWp8iCWx+wb1XA6t0eKFWISWM5lrwd+R4ysMwrny')
#  rsconnect::deployApp('/Users/tamas/Documents/DOCS/R/KM shiny')
