


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
 
  
  #    beginning <- Sys.time()
  # 
  #   onStop(function(){
  # 
  #       time  <- difftime(Sys.time(), beginning,
  #                         units = "mins")
  #       dbExecute(mydb,paste("INSERT INTO visit_time
  # (stamp,time)
  # VALUES('",Sys.time(),"','",time,"');", sep=""))
  # 
  #   })
    
    
    
    shinyjs::runjs("$('#textarea1').attr('maxlength', 80)")
    shinyjs::runjs("$('#textarea2').attr('maxlength', 80)")
    
    
    
    
  #   observeEvent(input$submit11,{
  #     dbExecute(mydb,paste("INSERT INTO monitor_feedback
  # (name,occupation,comments1,comments2,grade,time)
  # VALUES('",input$textarea,"','",input$areyou,"','",input$textarea1,"','",input$textarea2,"','",input$rate,"','",Sys.time(),"');", sep=""))
  #     feedback_accepted()
  # 
  #   })
    
    
    
    
    
    
    output$radio <- renderUI({
        radioButtons(inputId = "zones", label = '', 
                     choices = c("Taxi Zone", "Service Zone"), 
                     selected = 'Taxi Zone')
    })
    
    output$firstfork <- renderUI({
        selectInput(inputId = "ech", label = paste0('Trends in ',names(choices)[choices == input$cbd]), 
                    choices = c("Service Zone", "Industry Trends in Selected Service Zone"), 
                    selected = "Industry Trends in Selected Service Zone")
    })
    
    output$secondfork <- renderUI({
        selectInput(inputId = "ech1", label = paste0('Trends in ',input$ZONE), 
                    choices = c("Taxi Zone", "Industry Trends in Selected Taxi Zone"), 
                    selected = "Industry Trends in Selected Taxi Zone")
    })
    
    
    
    
    
    
    
    welcome() 
    

    
    observeEvent(input$btnYes,{
      
      removeModal()
      introjs(session)
      
      
    }) 
        
    
    observeEvent(input$start,{
tuto1()
      
    })   
    

    
    
    
    
    observeEvent(input$start1,{
        tuto2()
        
    })
    
    
    
    
    observeEvent(input$start2,{
        tuto3()
        
    }) 
    
    
    observeEvent(input$report, {
download1()
    })
    
    #add reporting for industry metrics indicators
    observeEvent(input$im_report, {
download2()
    })
    
    

    
    
    
    data_m <- reactive ({

        data1 %>% dplyr::filter(Z == input$cbd, company == input$ind,metric == input$metric) %>% dplyr::select(year_month,id,company,count,Zone,metric) 
 
    })
 
    output$slider <- renderUI({
        sliderInput("year1",label = 'STEP 4: Select Period', min = min(data_m()$year_month), max = max(data_m()$year_month),
                    value = as.Date('2019-05-01'), timeFormat = "%Y-%b")
    })
    

    
    month <- reactive({
      req(input$year1)
      print(paste0(substr(input$year1,1,7),"-01"))
    })
    
    all <- reactive({
        
     
        data <- data1 %>% dplyr::filter(metric == 'PickUps',company == input$ind, year_month == month(), Z %in% c('Core','Non_Core')) %>% dplyr::select(count) %>% summarise(count = sum(count)) 
        print(data)
    })
    
    
    box_1st_main <- reactive({
        
        if (input$val == 'Monthly'){
            req(input$year1)
            data <- data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month) %>% summarise(count = sum(count))
            data1 <- data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month) %>% summarise(count1 = sum(count))
            
        } 
        else if (input$val == 'Daily Average'){
            req(input$year1)
            a <- as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
            b <- as.numeric(days_in_month(as.Date(month()) %m-% months(as.numeric(input$lag))))
            data <- data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month) %>% summarise(count = sum(count)/a)
            data1 <- data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month) %>% summarise(count1 = sum(count)/b)  
        }
        
        
        if (nrow(data1) > 0){
            data <- bind_cols(data,data1)
        } else {data <- data %>% add_column(count1 = 0)
        }
        data <- data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif/(count1) * 100)
        # data <- data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif/count1 * 100)
        
        
        #print(data)
    })
    

    
    
    for_chart <- reactive({
        #id <- lookup

        if (input$ind != 'FHV'){
       test <- data1 %>% dplyr::filter(Z == input$cbd, metric == input$metric) 
        } else {
          test <- fhvs %>% dplyr::filter(industry == input$indFhv) %>% dplyr::arrange(desc(count))  
        }
        print(test)  
    })
    
    output$B <- renderUI({
        selectInput(inputId = "ZONE", label = 'Pick a Zone', choices = unique(for_chart()$Zone), selected = "Upper East Side South")
    })
    
    
    box_1st_secondary <- reactive({
      
      if (input$ind != 'FHV'){

        req(input$ZONE)
        if (input$vald == 'Monthly'){
            req(input$year1)
            data <- data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month, Zone) %>% summarise(count = sum(count)) %>% dplyr::filter(Zone == input$ZONE)
            data1 <- data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>% dplyr::group_by(year_month, Zone) %>% summarise(count1 = sum(count))%>% dplyr::filter(Zone == input$ZONE)} 
        else if (input$vald == 'Daily'){
            req(input$year1)
            a <- as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
            b <- as.numeric(days_in_month(as.Date(month()) %m-% months(as.numeric(input$lag))))
            data <- data_m() %>% dplyr::filter(year_month == month()) %>% dplyr::group_by(year_month, Zone) %>% summarise(count = sum(count)/a) %>% dplyr::filter(Zone == input$ZONE)
            data1 <- data_m() %>% dplyr::filter(year_month == paste0(as.Date(month()) %m-% months(as.numeric(input$lag)))) %>%  dplyr::group_by(year_month, Zone) %>% summarise(count1 = sum(count)/b) %>% dplyr::filter(Zone == input$ZONE)
        }
        
        if (nrow(data1) > 0){
            data <- bind_cols(data,data1)
        } else {data <- data %>% add_column(count1 = 0)
        }
        data <- data %>% dplyr::mutate(dif = count - count1) %>% dplyr::mutate(ratio = dif/(count1) * 100)
   } })
    
    
    
    for_map <- reactive({
      
      if (input$ind != 'FHV'){
        req(input$year1)
   
        data_m() %>% 
            dplyr::filter(year_month == month()) %>% dplyr::select(id,count)
      
   } })
    
    
    pal <- reactive({
      
      if (input$ind != 'FHV'){
        data <- data_m()
        data$year_month <- floor_date(data$year_month, 'months')
        if (input$metric %in% c('PickUps', 'DropOffs', 'Trips')){
            data<- data %>% dplyr::group_by(year_month,company,id) %>% 
                dplyr::summarise(count = sum(count))}
        else { data<- data %>% dplyr::group_by(year_month,company,id) %>% 
            dplyr::summarise(count = mean(count))}
 }  })
    
    
    output$leaf <- renderLeaflet({
      if (input$ind != 'FHV'){
        
        leaflet(options = leafletOptions(zoomControl = FALSE, preferCanvas = TRUE,updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
            setView(-73.809354, 40.737084, zoom = 12)%>% 
            addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
            addProviderTiles(providers$CartoDB.Positron, group = 'Standard Gray') %>%
            addTiles(group  = 'City View') %>%
            
            addResetMapButton() %>%
            addLayersControl(position = 'topleft',
                             baseGroups = c('Satellite', "Standard Gray", "City View"),
                             options = layersControlOptions(collapsed = FALSE) 
            )
  }  })
    
    
    observe({
      if (input$ind != 'FHV'){
        req(input$ZONE)
        
        shape@data <- left_join(shape@data, for_map(), by="id")
        #str(for_map())
        req(input$year1)
        
        
        pal <- colorNumeric(palette = "OrRd",if (input$ind == 'YELLOW') {domain=shape@data$count} else {domain=c(min(pal()$count),max(pal()$count))})
        
        #print(shape@data)
        marker <- shape@data %>% dplyr::filter(zone == input$ZONE)
        
        a=leafletProxy("leaf", deferUntilFlush = F) %>%
            clearShapes() %>%
            clearMarkers() %>%
            
            addPolygons(data = shape, weight = 1.3,
                        color = ~pal(count),
                        fillOpacity = 0.9,
                        popup = paste0("Zone: ", shape@data$zone, "<br>",
                                       "Count: ", format(shape@data$count, big.mark = ",")),
                        highlightOptions = highlightOptions(weight = 6,
                                                            color = "orange",
                                                            bringToFront = TRUE))
        if (input$check == T){
            a <- a %>% addMarkers(lng = marker$x, lat = marker$y)
        } else {a <- a}
        
        a <- a %>% clearControls()%>%
            
            addLegend("bottomleft", pal <- pal, values = shape@data$count,
                      title = "Trip Counts"
            )
        
        
  }  })
    
    
    output$dth <- DT::renderDataTable(
        for_chart(), extensions = 'Scroller', options = list(
            deferRender = TRUE,
            scrollY = 390,
            scroller = TRUE
        )
    )
    
    
      
    output$first1 <- renderEcharts4r({
      if (input$ind != 'FHV'){
      
        withoutFHV <- for_chart() %>% dplyr::filter(company != 'FHV')
      
        if (input$zones == 'Service Zone'){
            if(input$ech == 'Service Zone'){
                
                ec <- withoutFHV %>% dplyr::filter(company == input$ind) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = sum(count))
                
                ec <- ec %>% group_by(company) %>%
                    e_charts(year_month) %>%
                    e_bar(count) %>%
                    e_theme("westeros") %>%
                    e_tooltip(trigger = 'axis') %>%
                    echarts4r::e_legend(type = 'scroll')
            }
            
            else if(input$ech == 'Industry Trends in Selected Service Zone'){
                ec <- withoutFHV %>%  dplyr::group_by(year_month, company, metric) %>% summarise(count = sum(count))
                
                ec <- ec %>%
                    group_by(company) %>%
                    e_charts(year_month) %>%
                    e_line(count) %>%
                    e_theme("westeros") %>%
                    e_tooltip(trigger = 'axis') %>%
                    e_datazoom(type = 'slider')%>%
                    echarts4r::e_legend(type = 'scroll') %>%
                    e_color(
                        c('white', "green", "black",'shared','yellow')
                    )
            }} else if (input$zones == 'Taxi Zone'){
                if(input$ech1 == 'Taxi Zone'){
                    
                    ec <- withoutFHV %>% dplyr::filter(company == input$ind, Zone == input$ZONE) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = sum(count))
                    
                    ec <- ec %>% group_by(company) %>%
                        e_charts(year_month) %>%
                        e_bar(count) %>%
                        e_theme("westeros") %>%
                        e_tooltip(trigger = 'axis') %>%
                        echarts4r::e_legend(type = 'scroll')
                }
                
                else if(input$ech1 == 'Industry Trends in Selected Taxi Zone'){
                    ec <- withoutFHV %>% dplyr::filter(Zone == input$ZONE) %>% dplyr::group_by(year_month, company, metric) %>% summarise(count = sum(count))
                    
                    ec <- ec %>%
                        group_by(company) %>%
                        e_charts(year_month) %>%
                        e_line(count) %>%
                        e_theme("westeros") %>%
                        e_tooltip(trigger = 'axis') %>%
                        e_datazoom(type = 'slider')%>%
                        echarts4r::e_legend(type = 'scroll')%>%
                        e_color(
                          c('white', "green", "black",'shared','yellow')
                        )
                }}
        
      } else
      {
        ec <- for_chart()
        
        ec <- ec %>%
          group_by(industry) %>%
          e_charts(year_month) %>%
          e_bar(count) %>%
          e_theme("westeros") %>%
          e_tooltip(trigger = 'axis') %>%
          e_datazoom(type = 'slider')%>%
          echarts4r::e_legend(type = 'scroll')
        
        
      }
    })
    #################################################################################################################################
    output$first_main <- renderValueBox({
        
        
        a <- as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
        shinydashboard::valueBox(format(box_1st_main()$count, big.mark = ","),
                                 subtitle = paste0(if (input$ind == 'ALL') {'Combined '} else {paste0(names(choices1)[choices1 == input$ind],' industry ')},input$val," ","'",names(choices)[choices == input$cbd],"'"," ",input$metric, ' in ',as.yearmon(input$year1, "%Y-%m-%d")),
                                 icon = icon("fas fa-taxi"),
                                 color = 'blue', width = 12)
    })
    
    
    output$second_main <- renderValueBox({
        shinydashboard::valueBox(if(input$val == 'Monthly') {paste0(round(box_1st_main()$count/all()$count * 100),'%')} else if (input$val == 'Daily Average') {
            paste0(round(box_1st_main()$count/(all()$count/as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))) * 100),'%')
        },
        subtitle = paste0(if (input$ind == 'ALL') {'Combined '} else {paste0(names(choices1)[choices1 == input$ind],' industry ')},input$val," ","'",names(choices)[choices == input$cbd],"'"," ",input$metric,' to all ',names(choices1)[choices1 == input$ind],' ',input$metric,' in ',as.yearmon(input$year1, "%Y-%m-%d")),
        icon = icon("fas fa-taxi"),
        color = 'blue', width = 12)
    })
    
    output$third_main <- renderValueBox({
        
        shinydashboard::valueBox(
            
            if (box_1st_main()$dif == box_1st_main()$count) {paste0('NO DATA')} else {
                format(round(box_1st_main()$dif), big.mark = ",")},
            if (box_1st_main()$dif == box_1st_main()$count) {subtitle =paste0("*Data are avaliable in ranges from ",first(data_m()$year_month)," to ",last(data_m()$year_month),".")} 
            else {subtitle = paste0(if (input$ind == 'ALL') {'Combined '} else {paste0(names(choices1)[choices1 == input$ind],' industry ')},"'",names(choices)[choices == input$cbd],"'"," ",input$val," ",input$metric," difference between ",as.yearmon(input$year1, "%Y-%m-%d"), " and ",as.yearmon(input$year1 %m-% months(as.numeric(input$lag))))},
            icon = icon("fas fa-taxi"),
            if (box_1st_main()$dif > 0) {color = 'green'} else if (box_1st_main()$dif < 0) {color = 'red'} else {color = 'blue'}, width = 12)
    })
    
    
    output$fourth_main <- renderValueBox({
        shinydashboard::valueBox(if (box_1st_main()$ratio == Inf) {paste0('NO DATA')} else {
            paste(format(round(box_1st_main()$ratio), big.mark = ","), " %")},
            if (box_1st_main()$ratio == Inf) {subtitle =paste0("*Data are avaliable in ranges from ",first(data_m()$year_month)," to ",last(data_m()$year_month),".")} 
            else {subtitle = paste0(names(choices1)[choices1 == input$ind],' industry ',"'",names(choices)[choices == input$cbd],"'"," ",input$val," ",input$metric," percent difference between ",as.yearmon(input$year1, "%Y-%m-%d"), " and ",as.yearmon(input$year1 %m-% months(as.numeric(input$lag))))},
            icon = icon("fas fa-taxi"),
            if (box_1st_main()$ratio > 0) {color = 'green'} else if (box_1st_main()$ratio < 0) {color = 'red'} else {color = 'blue'}, width = 12)
    })
    
    ####################################################################################################################################    
    
    ######################################################################################################################################   
    output$first_secondary <- renderValueBox({
        a <- as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))
        shinydashboard::valueBox(format(round(box_1st_secondary()$count), big.mark = ","),
                                 subtitle = paste0(input$ZONE," ",input$vald," ",input$metric, ' in ',as.yearmon(input$year1, "%Y-%m-%d")),
                                 icon = icon("fas fa-taxi"),
                                 color = 'blue', width = 12)
    })
    
    
    output$second_secondary <- renderValueBox({
        shinydashboard::valueBox(if(input$vald == 'Monthly'){paste0(round(box_1st_secondary()$count/box_1st_main()$count * 100),'%')} else if (input$vald == 'Daily'){
            paste0(round(box_1st_secondary()$count/(box_1st_main()$count/as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d")))) * 100),'%')
        }
        
        ,
        subtitle = paste0('Ratio of ' ,input$ZONE," ",input$vald," ",input$metric,' to all ',input$metric,' in ',as.yearmon(input$year1, "%Y-%m-%d")),
        icon = icon("fas fa-taxi"),
        color = 'blue', width = 12)
    })
    
    output$third_secondary <- renderValueBox({
        
        shinydashboard::valueBox(
            
            if (box_1st_secondary()$dif == box_1st_secondary()$count) {paste0('NO DATA')} else {
                format(round(box_1st_secondary()$dif), big.mark = ",")},
            if (box_1st_secondary()$dif == box_1st_secondary()$count) {subtitle =paste0("*Data are avaliable in ranges from ",first(data_m()$year_month)," to ",last(data_m()$year_month),".")} 
            else {subtitle = paste0(input$ZONE," ",input$vald," ",input$metric," difference between ",as.yearmon(input$year1, "%Y-%m-%d"), " and ",as.yearmon(input$year1 %m-% months(as.numeric(input$lag))))},
            icon = icon("fas fa-taxi"),
            if (box_1st_secondary()$dif > 0) {color = 'green'} else if (box_1st_secondary()$dif < 0) {color = 'red'} else {color = 'blue'}, width = 12)
    })
    
    
    output$fourth_secondary <- renderValueBox({
        shinydashboard::valueBox(if (box_1st_secondary()$ratio == Inf) {paste0('NO DATA')} else {
            paste(format(round(box_1st_secondary()$ratio), big.mark = ","), " %")},
            if (box_1st_secondary()$ratio == Inf) {subtitle =paste0("*Data are avaliable in ranges from ",first(data_m()$year_month)," to ",last(data_m()$year_month),".")} 
            else {subtitle = paste0(input$ZONE," ",input$vald," ",input$metric," percent difference between ",as.yearmon(input$year1, "%Y-%m-%d"), " and ",as.yearmon(input$year1 %m-% months(as.numeric(input$lag))))},
            icon = icon("fas fa-taxi"),
            if (box_1st_secondary()$ratio > 0) {color = 'green'} else if (box_1st_secondary()$ratio < 0) {color = 'red'} else {color = 'blue'}, width = 12)
    })    
    
    
    #############################################################################################################################################     
    
    #FL----------------------------------------------
    #add value boxes
    
    mainGr <- reactive({
        a = input$indMetric1
        data <- setDT(industry_metrics)[,c('license_class',..a,'month_date',"shared_trips_per_day_percent")]
        data$month_date <- ymd(data$month_date)
        data$license_class <- as.character(data$license_class)
        
        start_date = ymd(input$monthdate[1])
        end_date = ymd(input$monthdate[2])
        
        data =  subset(data, 
                       (month_date >= start_date & 
                            month_date <= end_date))
        
        print(data)
        
        
    })
    
    output$yellowtripbox = renderValueBox({
        
        med_trips = head(round(mainGr()[mainGr()$license_class=="Yellow", 2],2),1)
        #print(med_trips)
        shinydashboard::valueBox(
            if (is.na(med_trips) == T || input$indMetric1 == 'trips_per_day_shared') {paste0('No Data')} else {
                paste0(med_trips)}, 
            if (is.na(med_trips) == T || input$indMetric1 == 'trips_per_day_shared') {paste0('.')} else {
                paste0('Yellow ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "yellow", width = 4)
    }) 
    output$greentripbox = renderValueBox({
        
        shl_trips = head(round(mainGr()[mainGr()$license_class=="Green", 2],2),1)
        # print(shl_trips)
        shinydashboard::valueBox(
            if (is.na(shl_trips) == T || input$indMetric1 == 'trips_per_day_shared') {paste0('No Data')} else {
                paste0(shl_trips)}, 
            if (is.na(shl_trips) == T || input$indMetric1 == 'trips_per_day_shared') {paste0('.')} else {
                paste0('Green ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "green", width = 4)
    }) 
    output$hvtripbox = renderValueBox({ 
        
        ubers_etc = head(round(mainGr()[mainGr()$license_class=='FHV - High Volume', 2],2),1)
        shinydashboard::valueBox(
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('No Data')} else {
                paste0(ubers_etc)}, 
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('.')} else {
                paste0('HVFHV ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "maroon", width = 4)
    })
    output$bctripbox = renderValueBox({ 
        
        ubers_etc = head(round(mainGr()[mainGr()$license_class=='FHV - Black Car', 2],2),1) 
        shinydashboard::valueBox(
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('No Data')} else {
                paste0(ubers_etc)}, 
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('.')} else {
                paste0('FHV - Black Car ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "navy", width = 4)
    })
    output$lxtripbox = renderValueBox({ 
        
        ubers_etc = head(round(mainGr()[mainGr()$license_class=='FHV - Lux Limo', 2],2),1) 
        print(ubers_etc)
        shinydashboard::valueBox(
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('No Data')} else {
                paste0(ubers_etc)}, 
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('.')} else {
                paste0('FHV - Lux Limo ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "blue", width = 4)
    })
    output$lvtripbox = renderValueBox({ 
        
        ubers_etc = head(round(mainGr()[mainGr()$license_class=='FHV - Livery', 2],2),1)  
        shinydashboard::valueBox(
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('No Data')} else {
                paste0(ubers_etc)}, 
            if (is.na(ubers_etc) == T || input$indMetric1 %in% c('trips_per_day_shared','percent_of_trips_paid_with_credit_card')) {paste0('.')} else {
                paste0('FHV - Livery ',names(choices2)[choices2 == input$indMetric1],' as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "red", width = 4)
    })
    output$hvsharing = renderValueBox({ 
        recent_date = as.character(industry_metrics[1, "month_date"])
        my_query = "High Volume trips per day are shared as of 'SAMPLE'"
        shared = paste0(first(mainGr()[mainGr()$license_class == 'FHV - High Volume',4]) * 100, ' %') 
        print('before')
        print(shared)
        print('after')
        shinydashboard::valueBox(
            if (shared[1] == "NA %") {paste0('No Data')} else {
                paste0(shared[1])}, 
            if (shared[1] == "NA %") {paste0('.')} else {
                paste0('High Volume trips per day that are shared as of ', as.yearmon(input$monthdate[2]))}, icon = icon("fas fa-taxi"),
            color = "aqua", width = 3)
    })
    #choose columns to display----
    output$mytable = renderDataTable({
        industry_metrics
    })
    
    output$textbox = renderText({
        print("*Note that the * next to graphs designates these aggregations are based on daily averages going back and not on summations over selected periods. Boxes above show the values that correspond to the latest month selected")
    })
    
    
    #charts
    #main chart
    output$mainGraph <- renderEcharts4r({
        
        td <- mainGr()
        
        colnames(td)[2] <- 'count'
        
        td <- td %>% group_by(license_class) %>%
            e_charts(month_date) %>%
            e_line(count) %>%
            e_theme("westeros") %>%
            e_tooltip(trigger = 'axis') %>%
            echarts4r::e_legend(type = 'scroll')%>%
            e_color(
                c("navy", "#ca76b6",'red','blue','green','yellow')
            )
        
    })
    
    #other graphs
    #trips per day----
    output$trips_per_day = renderPlotly({
   
        td = subset(industry_metrics, 
                    (month_date >= input$monthdate[1] & 
                         month_date <= input$monthdate[2]), c('trips_per_day', 'month_date', 'license_class'))
        print(head(td))
        trips = plot_ly(td, x = ~month_date
                        , y = ~trips_per_day
                        ,type = 'scatter'
                        , split = ~license_class
                        , mode = 'lines'
                        ,color = ~license_class
                        #,colors = pal
        )   
        
        trips = layout(trips,             
                       title = "Average Trips per Day each Month", 
                       xaxis = list(           
                           title = "Month & Year",   
                           showgrid = F        
                       ),
                       yaxis = list(           
                           title = "Trips Per Day"      
                       ))
        
        #farebox per day----
        if (input$dimension == '2') {
 
            td = subset(industry_metrics,
                        (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                        c('farebox_per_day', 'month_date', 'license_class'))
            
            trips = plot_ly(td, x = ~month_date, y = ~farebox_per_day
                            , type = 'scatter'
                            , split = ~license_class
                            , mode = 'lines'
                            ,color = ~license_class
                            #,colors = pal
            )
            
            trips = layout(trips,              
                           title = "Average Farebox Per Day each Month", 
                           xaxis = list(          
                               title = "Month & Year",    
                               showgrid = F       
                           ),
                           yaxis = list(           
                               title = "Farebox Per Day"     
                           ))
        }
        trips
    })
    
    #trips per year----
    output$trips_year = renderPlotly({
        
        td = subset(industry_metrics, 
                    (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                    c('trips_per_month','trips_per_day','month_date','license_class', 'year','trips_per_week'))
        
        
        nd = aggregate(trips_per_day ~ year + license_class, data = td, FUN = sum)
        
        uniks = plot_ly(nd
                        , x = ~year
                        , y = ~trips_per_day
                        , split = ~license_class
                        , type = 'bar'
                        ,color = ~license_class
                        #,colors = pal
        )
        uniks = layout(uniks,             
                       title = "*Average Trips Per Year", 
                       xaxis = list(           
                           title = "Month & Year",    
                           showgrid = F        
                       ),
                       yaxis = list(          
                           title = "Trips"     
                       ))
        
        #farebox per year------------------------------------------
        if (input$dimension == '2') {
   
            td =  subset(industry_metrics, 
                         (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                         c('farebox_per_day','month_date','license_class', 'year','trips_per_week'))
            
            nd = aggregate(farebox_per_day ~ year + license_class, data = td, FUN = sum)
            
            uniks = plot_ly(nd
                            , x = ~year
                            , y = ~farebox_per_day
                            , split = ~license_class
                            , type = 'bar'
                            ,color = ~license_class
                            #,colors = pal
            )
            uniks = layout(uniks,              
                           title = "*Average Farebox Per Year", 
                           xaxis = list(           
                               title = "Month & Year",     
                               showgrid = F        
                           ),
                           yaxis = list(           
                               title = "Farebox"      
                           ))
        }
        uniks
    })
    
    #trips per month------------------------------------------
    output$trips_per_month = renderPlotly({
        
        td =  subset(industry_metrics,
                     (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                     c('trips_per_month','month_date','license_class', 'year'))
        uniks = plot_ly(td, 
                        x = ~month_date, y = ~trips_per_month
                        , type = 'scatter'
                        , split = ~license_class
                        , mode = 'lines'
                        ,color = ~license_class
                        #,colors = pal
        )
        
        uniks = layout(uniks,             
                       title = "*Trips Per Month Over Time", 
                       xaxis = list(          
                           title = "Month & Year",     
                           showgrid = F      
                       ),
                       yaxis = list(           
                           title = "Trips Per Month"      
                       ))
                   
        
        
        #farebox per month----- 
        if (input$dimension == '2') {

            td =  subset(industry_metrics, 
                         (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                         c('farebox_per_month','month_date','license_class', 'year'))
            
            uniks = plot_ly(td, 
                            x = ~month_date, y = ~farebox_per_month
                            , type = 'scatter'
                            , split = ~license_class
                            , mode = 'lines'
                            ,color = ~license_class
                            #,colors = pal
            )
            
            uniks = layout(uniks,              
                           title = "*Farebox Per Month Over Time", 
                           xaxis = list(           
                               title = "Month & Year",     
                               showgrid = F        
                           ),
                           yaxis = list(           
                               title = "Farebox Per Month"      
                           ))
        }
        uniks     
    })
    
    #vehicles per month----
    output$medallions_per_month = renderPlotly({

        td =  subset(industry_metrics, 
                     (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                     c('unique_vehicles','month_date','license_class'))
        uniks = plot_ly(td, 
                        x = ~month_date, y = ~unique_vehicles
                        , type = 'scatter'
                        , split = ~license_class
                        , mode = 'lines'
                        ,color = ~license_class
                        #,colors = pal
        )
        uniks = layout(uniks,              
                       title = "Unique Vehicles Per Month Over Time", 
                       xaxis = list(          
                           title = "Month & Year",    
                           showgrid = F        
                       ),
                       yaxis = list(           
                           title = "Unique Vehicles"      
                       ))
                  
    
        #vehicles_per_day ----
        if (input$dimension == '2') {

            td =  subset(industry_metrics, 
                         (month_date >= input$monthdate[1] & month_date <= input$monthdate[2]), 
                         c('vehicles_per_day','month_date','license_class'))
            uniks = plot_ly(td, 
                            x = ~month_date, y = ~vehicles_per_day
                            , type = 'scatter'
                            , split = ~license_class
                            , mode = 'lines'
                            ,color = ~license_class
                            #,colors = pal
            )
            
            uniks = layout(uniks,              
                           title = "Vehicles Per Day Over Time", 
                           xaxis = list(           
                               title = "Month & Year",    
                               showgrid = F       
                           ),
                           yaxis = list(           
                               title = "Vehicless Per Day Per Month"      
                           ))
        }
        uniks     
    })
    
    #add data for im report
    for_im <- reactive({

        data =  subset(industry_metrics, 
                       (month_date >= input$monthdate[1] & 
                            month_date <= input$monthdate[2]))
        
    })
    
    
    ###############################################################################################
    
    
    #data banks----
    output$pu = DT::renderDataTable({
        DT::datatable(data1)
        
    })
    

    
    output$MM = DT::renderDataTable({
        DT::datatable(industry_metrics,options = list(scrollX = TRUE))
    })
    
    
    output$dict = DT::renderDataTable({
        DT::datatable(dict, class = 'cell-border stripe', options = list(
            columnDefs = list(list(className = 'dt-left', targets = 2))
        ))
    })
    
    output$downloadData1 = downloadHandler(
        filename = function() {
            
            paste('PickUps', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            # dbExecute(mydb,paste("INSERT INTO monitor_download
            #                                       (count, time)
            #                                       VALUES('pu','",Sys.time(),"');", sep=""))
            write.csv(data1, con, row.names = F)
        }
    )
    

    
    output$downloadData4 = downloadHandler(
        filename = function() {
            paste('Monthly_Metrics', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            # dbExecute(mydb,paste("INSERT INTO monitor_download
            #                                       (count, time)
            #                                       VALUES('mtr','",Sys.time(),"');", sep=""))
            write.csv(industry_metrics, con, row.names = F)
        }
    )
    
    #egg--------------------------------------------------
    output$easterEgg <- renderText({
        HTML(paste0("<div style='color:#fff; text-align: center; font-family: monospace;
    font-style: italic  '>",
                    paste0('by',br(),br(),'Nikita Voevodin',br(),br(),'&',br(),br(),'Fausto Lopez', sep = '<br/><br/>'),
                    "</div>"))
    })
    
    ##############################################################################################
    output$btnn <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = "main_report.html",
        content = function(file) {
            # dbExecute(mydb,paste("INSERT INTO monitor_report
            #                                       (count, time)
            #                                       VALUES('1','",Sys.time(),"');", sep=""))
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
                zone = input$cbd,
                metric = input$metric,
                industry = input$ind,
                monthly = round(box_1st_main()$count),
                dif = round(box_1st_main()$dif),
                rat = round(box_1st_main()$ratio),
                all = all()$count,
                mon = input$year1,
                lag = input$lag,
                a = as.numeric(round(box_1st_main()$count)),
                dif = round(box_1st_main()$dif),
                rat = round(box_1st_main()$ratio),
                b = as.numeric(days_in_month(as.Date(input$year1, format = "%Y-%m-%d"))),
                test = for_chart(),
                shape = shape,
                for_map = for_map(),
                pal = pal()
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    ################################################################################################  
    
    
    #industry metrics report download added
    output$im_btnn <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = "industry_metrics_report.html",
        content = function(file) {
            # dbExecute(mydb,paste("INSERT INTO monitor_report1
            #                                       (count, time)
            #                                       VALUES('1','",Sys.time(),"');", sep=""))
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "industry_metrics_report.Rmd")
            file.copy("industry_metrics_report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
                metric = input$indMetric1,
                for_im = for_im()
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    ##############################################################################################
    
})
