



# Define UI for application that draws a histogram
shinyUI(
    
    dashboardPage(
    dashboardHeader(
         title = "TLC DATA HUB" 
        #                 titleWidth = 330,
        #                 enable_rightsidebar = F,
        #                 rightSidebarIcon = "gears"
                        # ,
                        # left_menu = tagList(
                        #     introBox(dropdownBlock(useShinyjs(),
                        #                   id = "mydropdown",
                        #                   title = "Give Us Your Feedback",
                        #                   icon = icon("sliders"),
                        #                   br(),br(),
                        #                   textInput(inputId = 'textarea', label = 'Your Name'),
                        #                   selectInput('areyou','Occupation', choices = c('Transportation Professional', 'Journalist', 'Student', 'Other')),
                        #                   textAreaInput(inputId = 'textarea1', label = 'Comment 1', width = NULL, height = NULL,
                        #                                 cols = NULL, rows = NULL, placeholder = "80 characters limit!", resize = NULL),
                        #                   textAreaInput(inputId = 'textarea2', label = 'Comment 2', width = NULL, height = NULL,
                        #                                 cols = NULL, rows = NULL, placeholder = "80 characters limit!", resize = NULL),
                        #                   radioButtons(inputId = 'rate', 'Please, rate your experience.', choices = c('1 star','2 stars','3 stars','4 stars','5 stars'), selected = '5 stars',
                        #                                inline = F, width = NULL, choiceNames = NULL,
                        #                                choiceValues = NULL),
                        #                   actionButton('submit11', label = "Submit",icon("plane"), 
                        #                                style="color: #fff;background-color:#2e6da4; border-color: #2e6da4"), br(),br(),br()
                        #                   
                        #     ),data.step = 20, data.intro = steps[20])
                        #     
                        # )
                        
                        
    ),
    dashboardSidebar(width = 330, 
                     
                     introBox(sidebarMenu(id = 'side',br(),
                                 menuItem("Menu", tabName = "dashboards"
                                          #, icon = icon("dashboard")
                                          ,
                                          menuSubItem("Trip Viz",tabName = "m1"
                                                      #, icon = icon("dashboard")
                                                      ),
                                          menuSubItem("Industry Metrics",tabName = "fl"
                                                      #, icon = icon("dashboard")
                                                      ),
                                          menuSubItem("Data Bank",tabName = "databank1"
                                                      #, icon =icon("fas fa-database")
                                                      )),
                                 
                                 
                                 shiny::conditionalPanel(condition = "input.side == 'm1'",
                                                         
                                                         actionButton('start', label = "Info/Tutorial",icon("info-circle"),  
                                                                      style="color: #fff;  border-color: #2e6da4"),
                                                         
                                                         box(solidHeader = T, collapsible = F, collapsed = F, closable = F, title = 'CONTROLS', status = 'primary',width = NULL,
                                                                 introBox(
                                                                     
                                                                     conditionalPanel(
                                                                         'input.ind != "FHV"',
                                                                     selectInput('cbd', label = 'STEP 1: Select a Service Zone', choices = c('NYC (all boroughs and airports)'='Citywide',
                                                                                                                                                  'Manhattan – Congestion Zone'='Congestion_Zone',
                                                                                                                                                  'Manhattan – Core'='Core', 
                                                                                                                                                  'Manhattan – Central Business District (CBD)'='CBD', 
                                                                                                                                                  'Manhattan – Midtown'='Midtown', 
                                                                                                                                                  'Manhattan – Sub Midtown'='Sub_Midtown',
                                                                                                                                                  'NYC Excluding Manhattan Core'='Non_Core',
                                                                                                                                                  'NYC Airports (JFK, LGA and EWR)'='Airports'), selected = 'Core')),
                                                                     conditionalPanel(
                                                                         'input.ind == "FHV"',
                                                                         selectInput('cbd', label = 'STEP 1: Select a Service Zone', choices = c('NYC (all boroughs and airports)'='Citywide'
                                                                                                                                                 ), selected = 'Citywide'))
                                                                         ,
                                                                           userPost(collapsible = T,collapsed = T,
                                                                                                 id = 'help',
                                                                                                 image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                                                 author = NULL,
                                                                                                 description = "ZONES INFORMATION -->",
                                                                                                 "*NYC (all boroughs and airports) - All 5 boroughs, JFK, LaGuardia, Newark Airport.",br(),br(),
                                                                                                 "*Manhattan – Congestion Zone - Taxi zones below 96st in Manhattan (The zone in which NYS charges the Congestion Surcharge).",br(),br(),
                                                                                                 "*NYC Excluding Manhattan Core - All 5 boroughs (excluding Core zone), JFK, LaGuardia, Newark Airport.",br(),br(),
                                                                                                 "*Core - Taxi zones below 110st & 96st in Manhattan.", br(),br(),
                                                                                                 "*Manhattan – Central Business District (CBD) - Taxi zones below 59st in Manhattan.", br(),br(),
                                                                                                 "*Manhattan – Sub Midtown - 13 Taxi zones in central Manhattan (see map).", br(),br(),
                                                                                                 "*Manhattan – Midtown - Taxi zones below 66st and above 14st in Manhattan.", br(),br(),
                                                                                                 "*NYC Airports (JFK, LGA and EWR) - JFK, LaGuardia, Newark Airport."),
                                                                          data.step = 2, data.intro = steps[2]),br(),
                                                                 
                                                                 
                                                                 introBox(
                                                                     conditionalPanel(
                                                                         'input.ind != "FHV"',
                                                                     selectInput('metric', label = 'STEP 2: Select a Metric', choices = c('PickUps', 'DropOffs', 'Trips'), selected = 'PickUps'))
                                                                     ,conditionalPanel(
                                                                         'input.ind == "FHV"',
                                                                         selectInput('metric', label = 'STEP 2: Select a Metric', choices = c('Trips'), selected = 'Trips')),
                                                                          userPost(collapsible = T,collapsed = T,
                                                                                   id = 'help1',
                                                                                   image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                                   author = NULL,
                                                                                   description = "METRIC INFORMATION -->",
                                                                                   "*PickUps - Number of rides that started in the selected zone",br(),br(),
                                                                                   "*DropOffs - Number of rides that ended in the selected zone",br(),br(),
                                                                                   "*Trips - Number of trips that started in the selected service zone + Number 
                                                                                            of trips that started outside of the selected servise zone, but ended in it"),
                                                                          data.step = 3, data.intro = steps[3]),br(),
                                                                 
                                                                 
                                                                 introBox(selectInput(inputId = "ind", label = 'STEP 3: Select an Industry', choices = c('Yellow Taxi'='YELLOW',
                                                                                                                                                         'Green Taxi'='GREEN',
                                                                                                                                                         'For-Hire Vehicles (excluding HVFHS)'='FHV', 
                                                                                                                                                         'High Volume For Hire Services'='HVFHV', 
                                                                                                                                                         'Shared'='SHARED', 
                                                                                                                                                         'All Vehicles'='ALL'
                                                                 ), selected = 'YELLOW'),
                                                                 userPost(collapsible = T,collapsed = T,
                                                                          id = 'help2',
                                                                          image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                          author = NULL,
                                                                          description = "INDUSTRY TERMINOLOGY -->",
                                                                          "*Yellow Taxi - Yellow cabs (medallions).",br(),br(),
                                                                          "*Green Taxi - Green cabs (Street Hail Liveries).",br(),br(),
                                                                          "*For-Hire Vehicles (excluding HVFHS) - non-high volume FHVs (this includes trips from Livery, Black Car and Luxury Limousine FHV bases).",br(),br(),
                                                                          "*High Volume For Hire Services - UBER, LYFT, VIA. *JUNO is currently unavailable.",br(),br(),
                                                                          "*Shared - Trips that have been shared within the HVFHS industry.",br(),br(),
                                                                          "*All - All industries combined.")
                                                                 ,data.step = 4, data.intro = steps[4]),br(),
                                                                 
                                                                 
                                                                 introBox(
                                                                     conditionalPanel(
                                                                         'input.ind != "FHV"',
                                                                     uiOutput('slider'),
                                                                          userPost(collapsible = T,collapsed = T,
                                                                                   id = 'help3',
                                                                                   image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                                   author = NULL,
                                                                                   description = "TIMEFRAME INFORMATION -->",
                                                                                   paste0("*Data are available in monthly increments from 2011 to 2021.")
                                                                          )),data.step = 5, data.intro = steps[5])),
                                                         introBox(box(solidHeader = T, collapsible = F, collapsed = F, closable = F, title = '', status = 'primary', width = NULL,
                                                                          
                                                                          actionButton('report', label = "Download This Page",icon("download"), 
                                                                                       style="color: #fff; border-color: #2e6da4")
                                                                          # ,
                                                                          # 
                                                                          # downloadButton("btnn", "Summary"),br(),br(),userPost(collapsible = T,collapsed = T,
                                                                          #                                   id = 'report',
                                                                          #                                   src = "https://nulltx.com/wp-content/uploads/2014/07/question.jpg",
                                                                          #                                   author = NULL,
                                                                          #                                   description = "SUMMARY INFORMATION -->",
                                                                          #                                   "*This will generate a summary report with the selected above settings.")
                                                         ),data.step = 6, data.intro = steps[6])
                                                         
                                                         
                                 ),
                                 
                                 shiny::conditionalPanel(condition = "input.side == 'fl'",
                                                         
                                                         actionButton('start1', label = "Info/Tutorial",icon("info-circle"),  
                                                                      style="color: #fff;  border-color: #2e6da4"),
                                                         
                                                         box(solidHeader = T, collapsible = F, collapsed = F, closable = F, title = 'CONTROLS', status = 'primary',
                                                                 
                                                                 sliderInput("monthdate", "Choose a Date Range",
                                                                             min = ymd('2014-01-01'), max = last_month,
                                                                             value = c(ymd('2015-01-01'),ymd('2018-12-01')),timeFormat="%b %Y"),
                                                                 
                                                                 
                                                                 width = NULL, userPost(collapsible = T,collapsed = T,
                                                                                        id = 'helpf1',
                                                                                        image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                                        author = NULL,
                                                                                        description = "RANGE AVAILABLE -->",
                                                                                        paste0("*Data are available in monthly increments from Jan 2014 to Sep 2021"
                                                                                               #,ymd('2014-01-01')," to ",ymd('2021-09-01'),"."
                                                                                        )),br(),
                                                                 
                                                                 
                                                                 selectInput("indMetric1", "Select Your Variable for y-axis", choices = c(
                                                                     "Trips per Day" = "trips_per_day",
                                                                     "Trips per Day Shared" = "trips_per_day_shared",
                                                                     "Farebox per Day" = "farebox_per_day",
                                                                     "Unique Drivers" = "unique_drivers",
                                                                     "Unique Vehicles" = "unique_vehicles",
                                                                     "Averageg Minutes per Trip" = "avg_minutes_per_trip",
                                                                     "Average Days Vehicles on Road" = "avg_days_vehicles_on_road", 
                                                                     "Average Hours per Day per Vehicle" = "avg_hours_per_day_per_vehicle",
                                                                     "Average Days Drivers on Road" = "avg_days_drivers_on_road",
                                                                     "Average Hours per Day per Driver" = "avg_hours_per_day_per_driver", 
                                                                     "Percent of Trips Paid with Credit Card" = "percent_of_trips_paid_with_credit_card"), selected = "unique_vehicles"),
                                                                 userPost(collapsible = T,collapsed = T,
                                                                          id = 'helpf3',
                                                                          image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                          author = NULL,
                                                                          description = "METRICS TERMINOLOGY -->",
                                                                          "*Trips Per Day - Average number of trips recorded each day.",br(),br(),
                                                                          "*Farebox Per Day - Total amount, across all vehicles, collected from all fares,
                                                                                    surcharges, taxes, and tolls. Note: this amount does not include
                                                                                    amounts from credit card tips.",br(),br(),
                                                                          "*Unique Drivers - The total unique number of hack drivers who recorded a trip
                                                                                    each month.",br(),br(),
                                                                          "*Unique Vehicles - The total unique number of medallion taxis/SHLS/FHVs and
                                                                                    standby vehicles.",br(),br(),
                                                                          "*Vehicles Per Day - The average unique number of medallion taxis/SHLS/FHVs and
                                                                                    standby vehicles.",br(),br(),
                                                                          "*Avg Days Vehicles on Road - The average number of days each vehicle spent on the road per
                                                                           month.",br(),br(),
                                                                          "*Avg Hours Per Day Per Vehicle - The average number of hours in which a vehicle recorded a trip.",br(),br(),
                                                                          "*Avg Days Drivers on Road  - The average number of days each driver recorded a trip.",br(),br(),
                                                                          "*Avg Hours Per Day Per Driver - The average number of hours each.",br(),br(),
                                                                          "*Avg Minutes Per Trip - Average trip time from meter‐on to meter‐off.",br(),br(),
                                                                          "*Percent of Trips Paid with Credit Card - Number of trips where passenger paid by credit card out of the
                                                                   total number of trips.",br(),br(),
                                                                          "*Trips Per Day Shared - Average number of shared trips recorded each day."),br(),
                                                                 box(solidHeader = T, collapsible = F, collapsed = F, closable = F, title = '', status = 'primary', width = NULL,
                                                                         
                                                                         actionButton('im_report', label = "Download This Page",icon("download"), 
                                                                                      style="color: #fff; border-color: #2e6da4")
                                                                         # ,
                                                                         # 
                                                                         # downloadButton("btnn", "Summary"),br(),br(),userPost(collapsible = T,collapsed = T,
                                                                         #                                   id = 'report',
                                                                         #                                   src = "https://nulltx.com/wp-content/uploads/2014/07/question.jpg",
                                                                         #                                   author = NULL,
                                                                         #                                   description = "SUMMARY INFORMATION -->",
                                                                         #                                   "*This will generate a summary report with the selected above settings.")
                                                                 )                                                                        # ,
                                                                 
                                                         )),
                                 
                                 shiny::conditionalPanel(condition = "input.side == 'databank1'",
                                                         
                                                         actionButton('start2', label = "Info/Tutorial",icon("info-circle"),  
                                                                      style="color: #fff;  border-color: #2e6da4")                                                                      # ,
                                                         
                                 ),br(),br(),br(),br(),br(),br(),h5(htmlOutput("easterEgg"))
                                 
                                 
                                 
                     ),
                     data.step = 1, data.intro = steps[1]),heigth = '100%'
                     
                     
                     
    ),
    dashboardBody(useShinyalert(),introjsUI(),
                  tags$head(includeCSS("styles.css"))
                  
                  ,
                  tabItems(
                      tabItem(tabName = "m1",
                              
                              
                              conditionalPanel(
                                  'input.ind != "FHV"',
                              fluidRow(
                                  
                                  column(width = 12,
                                         introBox(box(solidHeader = T, collapsible = F, collapsed = F, closable = F, title = 'Industry Trips Map', status = 'success',
                                                 withSpinner(leafletOutput("leaf", height = '1100px')), width = NULL),data.step = 7, data.intro = steps[7])),
                                  
                                  
                                  
                                  
                                  
                                  absolutePanel(id="controls1",
                                                style="z-index:999;",
                                                class = "panel panel-default",
                                                draggable = F, top = 170, right = 40, width = '40%',
                                                column(width = 1),
                                                column(width = 11,
                                                       introBox(box(solidHeader = T, collapsible = T, collapsed = F, closable = F, 
                                                               title = "Service Zone Metrics", status = 'danger', icon = "fa fa-taxi", 
                                                               introBox(selectInput(inputId = "val", 'Monthly | Daily', choices = c('Monthly', 'Daily Average'), 
                                                                           selected = 'Monthly'),data.step = 9, data.intro = steps[9]),
                                                               
                                                               
                                                               width = NULL,
                                                               
                                                               introBox(valueBoxOutput('first_main', width = 12),data.step = 10, data.intro = steps[10]),br(),
                                                               introBox(valueBoxOutput('second_main', width = 12),data.step = 11, data.intro = steps[11]),br(),
                                                               introBox(selectInput(inputId = "lag", label = 'Select a number of months to go back to compare to the selected month.', 
                                                                           choices = c(seq(1,12, by = 1)), selected = 12),data.step = 12, data.intro = steps[12]),
                                                               introBox(valueBoxOutput('third_main', width = 12),data.step = 13, data.intro = steps[13]),br(),
                                                               introBox(valueBoxOutput('fourth_main', width = 12),data.step = 14, data.intro = steps[14])
                                                               
                                                       ),data.step = 8, data.intro = steps[8]))) 
                                  
                                  
                              )), 
                              fluidRow(
                                  introBox(box(title = "Trends", status = 'warning', width = 12, solidHeader = T, collapsible = F, collapsed = F, closable = F,
                                          fluidRow(
                                              column(width = 3, br(),
                                                     introBox(
                                                         
                                                          
                                                         boxPad(color = "black",
                                                                
                                                                userPost(collapsible = T,collapsed = T,
                                                                         id = 'for_fhv',
                                                                         image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                         author = NULL,
                                                                         description = "PLOT INFORMATION -->",
                                                                         "The plot to the right (-->) renders a citywide month to month trend for the traditional FHV industry.",br(),br(),
                                                                         "***Detailed geographical information is not available at the moment."),br(),
                                                                
                                                                conditionalPanel(
                                                                    'input.ind == "FHV"',
                                                                
                                                                selectInput(inputId = "indFhv", label = 'FHV Sub-Industries', choices = c('Traditional FHVs Combined'='fhv',
                                                                                                                                               'Black Cars'='fhv_black_car',
                                                                                                                                               'Liveries'='fhv_livery', 
                                                                                                                                               'Luxury'='fhv_lux_limo'
                                                                ), selected = 'Traditional FHVs Combined')),
                                                                
                                                                
                                                                conditionalPanel(
                                                                    'input.ind != "FHV"',
                                                            uiOutput('radio'),
                                                            
                                                            
                                                            
                                                            
                                                            conditionalPanel(
                                                                'input.zones == "Service Zone"', 
                                                                uiOutput('firstfork'),
                                                                
                                                                userPost(collapsible = T,collapsed = T,
                                                                         id = 'for_service_zone',
                                                                         image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                         author = NULL,
                                                                         description = "PLOT INFORMATION -->",
                                                                         "The plot to the right (-->) renders a month to month trend for the service zone picked in the step 1.",br(),br(),
                                                                         "Choose 'Trend' option to see trends for all industries side-by-side within the service zone that you picked."),br()
                                                            ),
                                                            
                                                            
                                                            conditionalPanel(
                                                                'input.zones == "Taxi Zone"', 
                                                                uiOutput('secondfork'),
                                                                uiOutput('B'), checkboxInput(inputId = 'check', label = 'Show on the map', value = FALSE, width = NULL),
                                                                
                                                                conditionalPanel(
                                                                    'input.ech1 == "Taxi Zone"',  
                                                                    userPost(collapsible = T,collapsed = T,
                                                                             id = 'help4',
                                                                             image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                             author = NULL,
                                                                             description = "PLOT INFORMATION -->",
                                                                             "The plot to the right (-->) renders a month to month trend for the taxi zone picked here.",br(),br(),
                                                                             "Choose 'Trend' option to see trends for all industries side-by-side within the taxi zone that you just picked."),br()
                                                                ), conditionalPanel('input.ech1 == "Trend"', br(), userPost(collapsible = T,collapsed = T,
                                                                                                                            id = 'help5',
                                                                                                                            image = "https://image.flaticon.com/icons/png/512/36/36601.png",
                                                                                                                            author = NULL,
                                                                                                                            description = "PLOT INFORMATION -->",
                                                                                                                            "Trends for all industries side-by-side within the taxi zone that you picked here.",br(), br(),
                                                                                                                            "In the plot to the right (-->), you can eliminate industries by clicking on them, and zoom in and out by moving the slider."),br(), br(), br()), width = NULL))),data.step = 16, data.intro = steps[16])),
                                              column(width = 9,
                                                     
                                                     introBox(withSpinner(echarts4rOutput("first1", height = '450px')),data.step = 17, data.intro = steps[17]))
                                          )),data.step = 15, data.intro = steps[15])),
                              
                              
                              conditionalPanel(
                                  'input.ind != "FHV"',
                              fluidRow(
                                  
                                  column(width = 6, setShadow('box'),
                                         introBox(box(solidHeader = T, collapsible = T, collapsed = T, closable = F, title = 'Industry Trips Table', status = 'warning', 
                                                 withSpinner(DT::dataTableOutput('dth')), width = NULL, br(),br(),br()),data.step = 18, data.intro = steps[18])
                                  ),
                                  
                                  column(width = 6,
                                         introBox(box(solidHeader = T, collapsible = T, collapsed = T, closable = F, title = "Taxi Zone Metrics", status = 'warning', icon = "fa fa-taxi", h5(htmlOutput("text1")), radioButtons(inputId = "vald", 'Monthly || Daily', choices = c('Monthly', 'Daily'), selected = 'Monthly'), width = NULL,
                                                 
                                                 valueBoxOutput('first_secondary', width = 12),br(),
                                                 valueBoxOutput('second_secondary', width = 12),br(),
                                                 valueBoxOutput('third_secondary', width = 12),br(),
                                                 valueBoxOutput('fourth_secondary', width = 12)
                                         ),data.step = 19, data.intro = steps[19])
                                  )))
                              
                              
                      ),
                      
                      tabItem(tabName = "fl",
                              fluidRow(
                                  withSpinner(echarts4rOutput("mainGraph", height = '370px'))
                              ),
                              fluidRow(
                                  #Dynamic infoBoxes
                                  valueBoxOutput("yellowtripbox", width = 4),
                                  valueBoxOutput("greentripbox", width = 4),
                                  # valueBoxOutput("hvsharing", width = 4)),
                                  valueBoxOutput("hvtripbox", width = 3)),
                              fluidRow(
                                  
                                  valueBoxOutput("bctripbox", width = 3),
                                  valueBoxOutput("lxtripbox", width = 3),
                                  valueBoxOutput("lvtripbox", width = 3)
                                  #,
                                  #box(textOutput("textbox2"), width = 6),
                                  #box(textOutput("textbox3")),
                                  
                              ),
                              
                              
                              fluidRow(column(5,selectInput(inputId = "dimension", label = strong("Choose a Dimension to See Different Graphs Below"),
                                                            choices = c('Trips, Drivers & Vehicles'='1', 'Time & Money' = '2'), 
                                                            multiple = FALSE, selectize = TRUE)),column(7)
                              ),
                              fluidRow(
                                  box(plotlyOutput(outputId = 'trips_per_day'), width = 6),
                                  box(plotlyOutput(outputId = 'trips_year'), width = 6)),
                              fluidRow(
                                  box(plotlyOutput(outputId = 'trips_per_month')),
                                  box(plotlyOutput(outputId = 'medallions_per_month'))
                              ),
                              fluidRow(
                                  box(textOutput("textbox"))#,
                                  #box(textOutput("textbox4"), width = 6)
                              )
                      ),
                      
                      
                      tabItem(tabName = "databank1",
                              # flipBox(id = 123, 
                              #                             front = 
                                                            
                                                            fluidRow(
                                                                column(width = 12,
                                                                       
                                                                       conditionalPanel(
                                                                           'input.dataset == "Trip Data"',
                                                                           downloadButton('downloadData1', 'Download Data Set')),
                                                                       # conditionalPanel(
                                                                       #     'input.dataset == "DropOffs"',
                                                                       #     downloadButton('downloadData2', 'Download Data Set')),
                                                                       # conditionalPanel(
                                                                       #     'input.dataset == "Trips"',
                                                                       #     downloadButton('downloadData3', 'Download Data Set')),
                                                                       conditionalPanel(
                                                                           'input.dataset == "Monthly_Metrics"',
                                                                           downloadButton('downloadData4', 'Download Data Set'))
                                                                ),
                                                                
                                                                
                                                                column(width = 12,
                                                                       box(solidHeader = T, collapsible = T, collapsed = F, closable = F, title = 'DATA', status = 'warning', width = NULL,    
                                                                               tabsetPanel(
                                                                                   id = 'dataset',
                                                                                   tabPanel('Trip Data', DT::dataTableOutput('pu')),
                                                                                   # tabPanel('DropOffs', DT::dataTableOutput('do')),
                                                                                   # tabPanel('Trips', DT::dataTableOutput('tr')),
                                                                                   tabPanel('Monthly_Metrics', DT::dataTableOutput('MM')),
                                                                                   tabPanel('test',DT::dataTableOutput('dict'))
                                                                               ), br(),br(),br(),br())
                                                                       
                                                                ), br(),br(),br(),br()
                                                            )
                                                          # , back = tagList( box(solidHeader = T, collapsible = T, collapsed = F, closable = F, title = 'DATA DICTIONARY', status = 'warning', width = NULL), br(),br(),br(),br()
                                                          #   )
                                                          # , trigger = 'click', width = 12)
                              
                              )
                      
                      
                      
                  )
                  
    )
    
    
))
