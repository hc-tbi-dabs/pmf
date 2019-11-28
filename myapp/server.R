library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
# source("helpers.R")
# source("data.R")

# recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

x<- "Account 	Product 	Region 	Revenue
AxisBank 	FBB 	North 	2000
HSBC 	FBB 	South 	30000
SBI 	FBB 	East 	1000
ICICI 	FBB 	West 	1000
BandhanBank 	FBB 	West 	200
AxisBank 	SIMO 	North 	200
HSBC 	SIMO 	South 	300
SBI 	SIMO 	East 	100
ICICI 	SIMO 	West 	100
BandhanBank 	SIMO 	West 	200"


recommendation <- read.table(text = x, header = TRUE, sep = " ")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    # create the server functions for the dashboard  

    #some data manipulation to derive the values of KPI boxes
    total.revenue <- sum(recommendation$Revenue)
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(sales.account$value, format="d", big.mark=',')
            ,paste('Top Account:',sales.account$Account)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")  
    })
    output$value2 <- renderValueBox({ 
        valueBox(
            formatC(total.revenue, format="d", big.mark=',')
            ,'Total Expected Revenue'
            ,icon = icon("gbp",lib='glyphicon')
            ,color = "green")  
    })
    output$value3 <- renderValueBox({
        valueBox(
            formatC(prof.prod$value, format="d", big.mark=',')
            ,paste('Top Product:',prof.prod$Product)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "yellow")   
    })
    #creating the plotOutput content
    output$revenuebyPrd <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=Product, y=Revenue, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Product") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
            ggtitle("Revenue by Product") + labs(fill = "Region")
    })
    output$revenuebyRegion <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=Account, y=Revenue, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Account") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
            ggtitle("Revenue by Region") + labs(fill = "Region")
    })
    
    ##AS
    
    observeEvent(input$submit_as, {
      saveData(input, as_df, asDir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_as, {
      deleteData(asDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_as <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_as
      input$delete_as
      
      datatable(loadData(as_df, asDir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_as <- downloadHandler(
      filename = "data_as.csv",
      content = function(file) {
        write.csv(loadData(as_df, asDir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    
    ##AIR
    
    observeEvent(input$submit_a, {
      saveData(input, ar_df, arDir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_a, {
      deleteData(arDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_a <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_a
      input$delete_a
      
      datatable(loadData(ar_df, arDir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_a <- downloadHandler(
      filename = "data_air.csv",
      content = function(file) {
        write.csv(loadData(ar_df, arDir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    
    ## RM
    
    observeEvent(input$submit_rm, {
        saveData(input, rm_df, rmDir)
    })

    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_rm, {
        deleteData(rmDir)
    })

    # Show the previous responses in a reactive table ----
    output$responses_rm <- renderDataTable({
        # update with current response when Submit or Delete are clicked
        input$submit_rm
        input$delete_rm

        datatable(loadData(rm_df, rmDir), options = list(scrollX = TRUE))
        # data.frame( loadData(rm_df) )

    })

    # Downloadable csv of selected dataset ----
    output$downloadData_rm <- downloadHandler(
        filename = "data_rm.csv",
        content = function(file) {
            write.csv(loadData(rm_df, rmDir), file, row.names = FALSE, quote= TRUE)
        }
    )
    
    
    ##RCPI
    
    observeEvent(input$submit_rc, {
      saveData(input, rc_df, rcDir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_rc, {
      deleteData(rcDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_rc <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_rc
      input$delete_rc
      
      datatable(loadData(rc_df, rcDir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_rc <- downloadHandler(
      filename = "data_rc.csv",
      content = function(file) {
        write.csv(loadData(rc_df, rcDir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    
    ##CS
    
    observeEvent(input$submit_cs, {
      saveData(input, cs_df, csDir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_cs, {
      deleteData(csDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_cs <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_cs
      input$delete_cs
      
      datatable(loadData(cs_df, csDir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_cs <- downloadHandler(
      filename = "data_cs.csv",
      content = function(file) {
        write.csv(loadData(cs_df, csDir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    ##EA
    
    observeEvent(input$submit_ea, {
      saveData(input, ea_df, eaDir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_ea, {
      deleteData(eaDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_ea <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_ea
      input$delete_ea
      
      datatable(loadData(ea_df, eaDir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_ea <- downloadHandler(
      filename = "data_ea.csv",
      content = function(file) {
        write.csv(loadData(ea_df, eaDir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    
    ##RN1
    
    observeEvent(input$submit_rn1, {
      saveData(input, rn1_df, rn1Dir)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete_rn1, {
      deleteData(rn1Dir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses_rn1 <- renderDataTable({
      # update with current response when Submit or Delete are clicked
      input$submit_rn1
      input$delete_rn1
      
      datatable(loadData(rn1_df, rn1Dir), options = list(scrollX = TRUE))
      # data.frame( loadData(rm_df) )
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_rn1 <- downloadHandler(
      filename = "data_rn1.csv",
      content = function(file) {
        write.csv(loadData(rn1_df, rn1Dir), file, row.names = FALSE, quote= TRUE)
      }
    )
    
    
    ##RN2
    
    observeEvent(input$submit, {
        saveData(input, rn2_df, outputDir)
        # resetForm(session)
    })
    
    # observeEvent(input$clear, {
    #     resetForm(session)
    # })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete, {
        deleteData(outputDir)
    })
    
    # Show the previous responses in a reactive table ----
    output$responses <- renderDataTable({
        # update with current response when Submit or Delete are clicked
        input$submit 
        input$delete
      
        datatable(loadData(rn2_df, outputDir), options = list(scrollX = TRUE))
        # loadData(rn2_df)
        
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(loadData(rn2_df, outputDir), file, row.names = FALSE, quote= TRUE)
        }
    )
    

})
