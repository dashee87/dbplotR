# utils.R contains the code that checks for and loads the necessary packages
# It also contains two supplemenatary functions densityplotly (generates a 
# density graph in plotly) and cumultplotly (generates a culative distribution
# curve in plotly)

source("utils.R")

# This allows control of what is considered a factor.
options(stringsAsFactors = FALSE)

# This simple function formats the axis of a plotly graph. Future editions of the app may
# allow the user to dynamically alter these parameters For now, the axis text font, size and colour
# are static and defined here.

format_axis <- function(plottitle,size=18,colour="black",font = "Arial, sans-serif"){
  list(
    title = plottitle,
    titlefont = list(
      family = font,
      size = size,
      color = colour
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "plotdbR"),
  dashboardSidebar(
    br(),
    tagList(
      tags$style("a:hover {background-color: grey;}"),
      tags$link( a("Feedback/Issues", target="_blank",    href="http://www.github.com/dashee87/dbplotR",
                 style="width:90%; margin-left: 10px;"))
    ),
    selectInput("db_type", label = "DB Package",choices = c("RODBC")),
    radioButtons("connect_type",label="Connection Function",
                 choices=c("odbcConnect"="odbcC","odbcDriverConnect"="odbcDC")),
    br(),
    uiOutput("database_input"),
    uiOutput("username_input"),
    uiOutput("pass_input"),
    br(),
    bsButton("login", label = "Login",style="info",disabled=FALSE),
    uiOutput("login_fail"),
    tags$style(type='text/css', "#login { width:90%; margin-left: 10px;}")
    ),
  
  dashboardBody(fluidRow(
    box(title="Type SQL Query", status = "info", width=12, solidHeader = T,collapsible = T,
        tagList(
          tags$style(type="text/css", "textarea {width:100%; margin-top: 5px; resize: vertical;}"),
          tags$textarea(id = "sql_query", placeholder = "SELECT * from table limit 10", rows = 4, value="")
          ),
        bsButton("do_sql", label = "Run",disabled=TRUE,style="primary", icon = icon("ban"))
    )),
    fluidRow(
      tabBox(width = 12,id="tabset1",
             tabPanel("Table", 
                      DT::dataTableOutput("view") 
             ),
             tabPanel("Plots",
                      fluidRow(
                        box(title = "Plot Data", 
                            status = "warning", width = 12, solidHeader = T,
                            fluidRow(column(9,selectInput("plot_type", "Plot Type",
                                                          choices = c("Line/Scatter/Bar Graph" = "ts",
                                                                      "Distribution" = "dist",
                                                                      "Pie" = "pie",
                                                                      "3D" = "ddd"))),
                                     column(3,uiOutput("plotbutton"))
                                     ),
                            fluidRow(
                              column(2,uiOutput("xaxislist")),
                              column(2,uiOutput("yaxislist")),
                              column(2,uiOutput("zaxislist")),
                              column(2,uiOutput("factorlist")),
                              column(2,uiOutput("sizelist"))
                              ),
                            fluidRow(
                              column(2,
                                            fluidRow(column(12,uiOutput("option1"))),
                                            fluidRow(column(12,uiOutput("option2"))),
                                            fluidRow(column(12,uiOutput("option3"))),
                                            fluidRow(column(12,uiOutput("option4"))))
                                     ,column(10,plotlyOutput("plot1"))
                            ),
                            br(),
                            fluidRow(
                              column(2,uiOutput("xaxisinput")),
                              column(2,uiOutput("yaxisinput")),
                              column(2,uiOutput("zaxisinput")),
                              column(2,uiOutput("titleinput")),
                              column(2,uiOutput("legendinput"))
                              )
                            )
                      )
             )
      )               
    )
  )
)

server <- function(input, output, session) {
  
# We use three reactive values to keep track of the login status, plot status and login errors.
# The login status is initially set to -1 i.e. we're not logged in.
# If login is successful, then the values$loginState changes to the active connection
# It's essentially used to change the behaviour of the login and run query button
# As for plotState, it's initially set to 0 i.e. no plot is visible.
# When a plot is visible it changes to 1. Changing the plot type (e.g. Pie to 3D), will reset
# values$plotState to 0 and hide the plot. A new will be produced the next time the PLOT button 
# is pressed. This provides a measure of control over when the plot is updated, as the user is
# changing the settings.
# Finally, failed RODBC connections don't throw warnings by default (instead it returns -1 with
# a corresponding warning). We use values$warns to capture these warnings and post them to the user
  
  values <- reactiveValues(loginState=-1, plotState=0, warns=0)
  
# User input of database name/details  
  
  output$database_input =  renderUI({
    switch(input$connect_type,
           odbcC={textInput("database", label = "Database Name", placeholder = "my_database",value = "")},
           odbcDC={
             tagList(
             tags$label(id = "database","Connection String"),
             tags$style(type='text/css', "#database { width:90%; margin-left: 10px;color:black}"),
             tags$textarea(id = "database", placeholder = 
                             "(No Quotations Marks!)  e.g. driver={SQL Server};
                           server=servername\\\\instancename,port;database=testing;trusted_connection=true"
                           ,rows=5, value="asdad"))
             },
           return()
           )
  })

# User input of database username  
  
  output$username_input =  renderUI({
    switch(input$connect_type,
           odbcC={textInput("username", label = "Username", placeholder = "dashee87",value = "")},
           return()
    )
  })
  
# User input of database password  
  
  output$pass_input =  renderUI({
    
    switch(input$connect_type,
           odbcC={passwordInput("pass", label="Password", value = "", width = NULL)},
           return()
    )
  
    })
  
# In the event of login failure, the error message is posted below the login button  
  
  output$login_fail =  renderUI({
    if(values$warns[1]!=0){
      helpText(paste0("Login Failed: ",values$warns))
    }else{return()}
  })

# The following block of code runs when the shiny session is terminated
# Any active db connection is closed
  
  session$onSessionEnded(function() {
    observe({
      if(values$loginState!=-1){
        odbcClose(login())
      }
    })
  })

    
# Logging in  
  
  login <- eventReactive(input$login, {
    if(input$login==0){
      return(-1)}
    isolate({
      if(values$loginState==-1){
        switch(isolate(input$connect_type),
               odbcC = {con <- tryCatch(
                          RODBC::odbcConnect(input$database , uid = input$username, pwd = input$pass),
                          warning=function(c) c$message)
                        },
               odbcDC= {con <- tryCatch(RODBC::odbcDriverConnect(input$database),
                        warning=function(c) c$message)
               }
              )
        if(is.character(con)){
          values$warns=con
          con=-1
          # it's slightly ugly, but I'm more comfortable with the convention that a failed login
          # equates to -1
        }
        validate(
          need(con!=-1, paste0("Login Failed"))
        )
      }else{RODBC::odbcClose(values$loginState)
        return(-1)}
      con})
  },ignoreNULL=FALSE)
 
# When the user changes the plot type (say from Distribution to Pie), the previous graph on the page
# is removed (i.e plotState is set to zero).
# This step isn't completely necessary, I just want to user to be able to change the plot without
# any distractions. Plus, I like the temporary whitespace on the screen that this process generates.
  
  observeEvent(input$plot_type,({
    values$plotState=0
  })
  )

# Changing the login and sql buttons (e.g. blue <-> red/green) after login/logout
      
  observeEvent(login(), ({
    b <- login()
    values$loginState <- login()
    values$warns <- 0
    
    if(b==-1){
      updateButton(session, "login",label="Login" ,disabled = FALSE, style = "info")
      updateButton(session, "do_sql", disabled = TRUE, style = "primary")
    }else{
      session$sendInputMessage("database", list(value=""))
      updateTextInput(session,"username",value="")
      updateTextInput(session,"pass",value="")
      updateButton(session, "login", label="Logout",disabled = FALSE, style = "danger")
      updateButton(session, "do_sql", disabled = FALSE, style = "success",icon=icon("refresh"))
    }
  }))
  
# Gathering the data from a sql query  
    
  sqldata <- eventReactive(input$do_sql, {
    # remove whitespace from start and end of string
    query_input <- gsub("^\\s+|\\s+$", "", input$sql_query)
    # These are my attempts to prevent malicious SQL injections/errors
    validate(
      need(!grepl(";",query_input), "For security reason, queries including a semi-colon are not allowed!")
    )
    # No compound queries (using ;) and only queries starting with select
    # I realise this prevents queries like "with tab1 as (...) select * from tab1"
    validate(
      need(tolower(gsub(" .*$", "",query_input))=="select", 
           "Only queries starting with a select (case insensitive) are allowed")
    )
    withProgress(message = 'Querying...',{ query_output=sqlQuery(login(),paste0(query_input,";"))})
    validate(
      need(!is.character(query_output), paste0("Query failed: ",query_output[1]))
    )
    query_output
  })

# Producing a table from the sql query data
      
  output$view = DT::renderDataTable({
    table.data <- sqldata()
    print(head(table.data))
    table.data[sapply(table.data, is.character)] <- lapply(table.data[sapply(table.data, is.character)], 
                                             as.factor)
    datatable(table.data,filter="top",rownames=FALSE,extensions="Buttons",
              selection = list(target = 'column'),options = list(paging = TRUE, scrollX = T,
              dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf')))
  })

# Dropdown menu of columns from which to select the x-axis
      
  output$xaxislist =  renderUI({
    if (!is.data.frame(sqldata()))
      return()
    if(input$plot_type %in% c("ts","ddd")){
      selectInput("xaxis", label = "x-Axis", choices = colnames(sqldata()))
    }else if(input$plot_type == "dist"){
      selectInput("xaxis", label = "x-Axis", choices = c("------",colnames(sqldata())))
    }else{selectInput("xaxis", label = "x-Axis", choices = c("------"))}
  })
  
# Dropdown menu of columns from which to select the y-axis  
  
  output$yaxislist =  renderUI({
    if (!is.data.frame(sqldata()))
      return()
    if(input$plot_type %in% c("ts","ddd","pie")){
      selectInput("yaxis", label = "y-Axis", choices = colnames(sqldata()))
    }else if(input$plot_type == "dist"){
      selectInput("yaxis", label = "y-Axis", choices = c("------",colnames(sqldata())))
    }else{selectInput("yaxis", label = "y-Axis", choices = c("------"))}
  })

# Dropdown menu of columns from which to select the z-axis    
    
  output$zaxislist =  renderUI(
    if (is.data.frame(sqldata())){
      switch(input$plot_type,
      ddd=selectInput("zaxis", label = "z-Axis", choices = colnames(sqldata())),
      selectInput("zaxis", label = "z-Axis", choices = c("------"))
      )
    }else{return()}
  )

# Dropdown menu of columns from which to select the grouping/colour/factor column 
    
  output$factorlist =  renderUI(
    if (is.data.frame(sqldata())){
      if(input$plot_type=="pie"){
      selectInput("factor", label = "Colour", choices = colnames(sqldata()))
      }else{selectInput("factor", label = "Colour", choices = c("------",colnames(sqldata())))}
    }else{return()}
  )

# Dropdown menu of columns from which to select the size column (for scatter plots)     
  
  output$sizelist =  renderUI(
    if (is.data.frame(sqldata()) & input$plot_type %in% c("ts","ddd")){
      selectInput("size", label = "Size",choices = c("------",colnames(sqldata())))
    }else{return()}
  )

# The PLOT button is only rendered when the sql query returned a data frame 
  
  output$plotbutton =  renderUI({
    if (!is.data.frame(sqldata()))
      return()
    actionButton("plotButton", "PLOT")
  })
  
# Determines label of the x-axis on the plot 
  
  output$xaxisinput =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|is.null(isolate(input$plot_type))){
      return()}
    if(input$xaxis=="------")
      textInput("xaxislabel", label = "x-axis Label", value = "")
    else{textInput("xaxislabel", label = "x-axis Label", value = input$xaxis)}
    })

# Determines label of the y-axis on the plot   
    
  output$yaxisinput =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|is.null(isolate(input$plot_type))){
      return()}
    if(input$yaxis=="------")
      textInput("yaxislabel", label = "y-axis Label", value = "")
    else{textInput("yaxislabel", label = "y-axis Label", value = input$yaxis)}
  })

# Determines label of the z-axis on the plot 
      
  output$zaxisinput =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|input$zaxis=="------"){
      return()
    }else{textInput("zaxislabel", label = "z-axis Label", value = input$zaxis)}
  })

# Determines the title of the plot
     
  output$titleinput =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|is.null(isolate(input$plot_type))){
      return()}
    textInput("title", label = "Title", value = "")
  })
  
# Determines whether the legend accompanies the plot
  
  output$legendinput =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|is.null(isolate(input$plot_type))){
      return()}
    checkboxInput("legend", "Show Legend?", value = (input$factor != "------"), width = NULL)
  })

# Options for the plot
  
  output$option1 =  renderUI({
    if (!is.data.frame(plot.data())|values$plotState==0|is.null(isolate(input$plot_type))){
      return()}
    switch(isolate(input$plot_type),
           ts= {radioButtons("opt1","Graph Type",c("Line"="line","Bar"="bar",
                                                   "Scatter"="scatter","Area"="area"))},
           dist= {radioButtons("opt1","Graph Type",
                               c("Histogram"="hist","2D Histogram"="hist2d",
                                 "Density"="dense","Cumulative"="cumult","Box"="box"))},
           pie= {sliderInput("opt1", "Doughnut Size:", min = 0, max = 1, value = 0.0, step= 0.1)},
           ddd = {radioButtons("opt1","Graph Type",c("3D Line Plot"="line3d", "3D Scatter"="scatter3d"))},
           return()
    )
  })

# Additional options for the plot  
    
  output$option2 =  renderUI({
    if (!is.data.frame(plot.data()) | is.null(input$opt1) | values$plotState==0)
      return()
    switch(input$opt1,
            area= {radioButtons("opt2","Area Fill",
                                c("Zero y"="tozeroy", "Zero y"="tozerox", "Next y"="tonexty",
                                  "Next x"="tonextx","Self"="toself", "Next"="tonext"))},
            hist= {radioButtons("opt2","Histogram Format",c("Grouped"="group","Overlay"="overlay"))},
            bar= {radioButtons("opt2","Bar Format",c("Default"="def","Stacked"="stack","Relative"="relative"))},
            box= {sliderInput("opt2", "Jitter:", min = 0, max = 1, value = 0.0, step= 0.1)},
            return()
          )
    })

# Additional options for the plot   
    
  output$option3 =  renderUI({
    if (!is.data.frame(plot.data()) | values$plotState==0|is.null(input$opt1))
      return()
    switch(input$opt1,
           box= {sliderInput("opt3", "Offset:",min = -2, max = 2, value = 0.0, step= 0.1)},
           hist= {radioButtons("opt3","Normalisation",
                               c("Default"="","Percent"="percent","Density"="density",
                                 "Prob Density"="probability density"))},
           hist2d= {radioButtons("opt3","Normalisation",
                                    c("Default"="","Percent"="percent","Density"="density",
                                      "Prob Density"="probability density"))},
           return()
          )
    })
  
  # Additional options for the plot   
  
  output$option4 =  renderUI({
    if (!is.data.frame(plot.data()) | values$plotState==0|is.null(input$opt1))
      return()
    if (input$opt1 %in% c("hist","hist2d")){
      sliderInput("opt4", "Bins:",min = 0, max = nrow(plot.data()), value = 0, step= 1)
    }
  })  

# The data for the plot and the plot itself are sepeated. plot.data collects the data.
# It also allows the user to filter the data directly from the table. I split these two processes,
# as I didn't want the plot/data to update everytime the filter changes. This allows a measure of
# control over the process.
# I may rethink this reasoning in future editions of the package.
  
  plot.data=eventReactive(input$plotButton, {
    values$plotState <- 1
    if(!is.data.frame(sqldata()))
      return()
# commented out (for now)... this would prevent generation of plots with duplicated column inputs

#     inputs=c(input$xaxis,input$yaxis,input$factor)
#     validate(
#       need(!any(duplicated(inputs)),
#            paste("Duplicated columns:",paste(inputs[duplicated(inputs)],collapse=", ")))
#     )
   
    return(sqldata()[input$view_rows_all,])
  })

# The data for the plot and the plot itself are sepeated. The code below plots the data.
      
  output$plot1 <- renderPlotly({
    if(is.null(plot.data())|values$plotState==0|is.null(input$opt1))
      return()
    if(input$factor=="------"){
      confact <- ""
    }else{confact <- input$factor}
    if(input$size=="------"){
      consize <- "NULL"
    }else{
      validate(
        need(class(sqldata()[,input$size]) %in% c("integer","numeric"), "Size column must be numerical!")
      )
      consize <- input$size}
    col.names <- colnames(plot.data())
    # I admit it gets a little ugly here. Nested swtiches are used to a produce a plot that's unique 
    # to the user's input. The structure between the different cases is not all that varied.
    # It simply passes the user inputs (e.g. x-axis column) as arguments to plotly function calls.
    switch(isolate(input$plot_type),
           ts={
             validate(
               need(input$xaxis!="------" & input$yaxis!="------", 
                    paste0("Plot requires valid x-axis and y-axis columns"))
             )
             pldata <- arrange_(plot.data(),input$xaxis)
             switch(input$opt1,
                    bar={p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                       group=eval(parse(text = confact)),type="bar") %>% 
                      layout(barmode=input$opt2)},
                    scatter={p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                           group=eval(parse(text = confact)),mode="markers",size=eval(parse(text = consize)))},
                    line={p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                        group=eval(parse(text = confact)))},
                    area={p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                        group=eval(parse(text = confact)),fill= input$opt2)},
                    return()
             )
             p <- p %>% layout(xaxis=format_axis(input$xaxislabel),yaxis=format_axis(input$yaxislabel),title=input$title)
           },
           dist={
             switch(input$opt1,
                    hist={
                      validate(
                        need(input$xaxis!="------", paste0("Plot requires valid x-axis column"))
                      )
                      p <- plot_ly(plot.data(),x=eval(parse(text = input$xaxis)),
                                       group=eval(parse(text = confact)),type="histogram",
                                   opacity=0.6, histnorm=input$opt3, nbinsx=input$opt4) %>%
                      layout(barmode=input$opt2,xaxis=format_axis(input$xaxislabel),
                             yaxis=format_axis(""),title=input$title)},
                    
                    hist2d={
                      validate(
                        need(input$xaxis!="------" & input$yaxis!="------", 
                             paste0("Plot requires valid x-axis and y-xais column"))
                      )
                      p <- plot_ly(plot.data(),x=eval(parse(text = input$xaxis))
                                         ,y=eval(parse(text = input$yaxis)),
                                   type="histogram2d", histnorm=input$opt3, nbinsx=input$opt4, nbinsy=input$opt4) %>%
                      layout(xaxis=format_axis(input$xaxislabel),yaxis=format_axis(input$yaxislabel),title=input$tile)},
                    
                    box={
                      validate(
                        need(input$xaxis!="------"|input$yaxis!="------", 
                             paste0("Plot requires valid x-axis or y-axis column"))
                      )
                      if(input$xaxis %in% col.names & input$yaxis %in% col.names){
                        p <- plot_ly(plot.data(),x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                      group=eval(parse(text = confact)),type="box", boxpoints = "all", 
                                      jitter = input$opt2,pointpos = input$opt3) %>%
                        layout(xaxis=format_axis(input$xaxislabel),
                               yaxis=format_axis(input$yaxislabel),title=input$title)
                      
                        }else if(input$xaxis %in% col.names){
                        p <- plot_ly(plot.data(),x=eval(parse(text = input$xaxis)),
                                     group=eval(parse(text = confact)),type="box", boxpoints = "all", 
                                     jitter = input$opt2,pointpos = input$opt3) %>%
                          layout(xaxis=format_axis(input$xaxislabel),
                                 yaxis=format_axis(input$yaxislabel),title=input$title)
                      }else{
                        p <- plot_ly(plot.data(),y=eval(parse(text = input$yaxis)),
                                     group=eval(parse(text = confact)),type="box", boxpoints = "all", 
                                     jitter = input$opt2,pointpos = input$opt3) %>%
                          layout(xaxis=format_axis(input$xaxislabel),
                                 yaxis=format_axis(input$yaxislabel),title=input$title)}
                      },
                    
                    dense={
                      validate(
                        need(input$xaxis!="------", paste0("Plot requires valid x-axis column"))
                      )
                      p <-densityplotly(plot.data(),input$xaxis,confact) %>%
                      layout(xaxis=format_axis(input$xaxislabel),
                             yaxis=format_axis("Density"),title=input$title)},
                    
                    cumult={
                      validate(
                        need(input$xaxis!="------", paste0("Plot requires valid x-axis column"))
                      )
                      p <-cumultplotly(plot.data(),input$xaxis,confact) %>%
                      layout(xaxis=format_axis(input$xaxislabel),
                             yaxis=format_axis("Cumulative Density (%)"),title=input$title)},
                    return()
             )
           },
           ddd={
             validate(
               need(input$xaxis!="------" & input$yaxis!="------" & input$zaxis!="------", 
                    paste0("Plot requires valid x-axis, y-axis and z-axis columns"))
             )
             pldata <- arrange_(plot.data(),input$xaxis,input$yaxis)
             switch(input$opt1,
                    scatter3d={ p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                             z=eval(parse(text = input$zaxis)),group=eval(parse(text = confact)),
                                             type = "scatter3d",size=eval(parse(text = consize)),mode='markers')},
                    line3d={ p <- plot_ly(pldata,x=eval(parse(text = input$xaxis)),y=eval(parse(text = input$yaxis)),
                                          z=eval(parse(text = input$zaxis)),group=eval(parse(text = confact)),
                                          type = "scatter3d",mode='lines')},
                    return()
             )
             p <- p %>%   layout(xaxis=format_axis(input$xaxislabel),yaxis=format_axis(input$yaxislabel),
                                 zaxis=format_axis(input$zaxislabel))
           },
           pie={
             validate(
               need(input$yaxis!="------" & input$factor!="------", 
                    paste0("Plot requires valid y-xais and colour column"))
             )
             p <- plot_ly(labels = plot.data()[,input$factor], values = plot.data()[,input$yaxis],
                          type = "pie",hole=input$opt1)
             p <- p %>% layout(title=input$title)
             },
           return()
    )
    p %>% layout(showlegend = input$legend)
  })
  
}

shinyApp(ui, server)