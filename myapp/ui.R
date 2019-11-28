# Excel Password:EHPPMF
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



dashboardPage(
    title = 'This is my Page title',
#Dashboard header carrying the title of the dashboard
    dashboardHeader(title = "Basic Dashboard"),  
    #Sidebar content of the dashboard
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("All Staff", tabName = "allstaff", icon = icon("group")),
            menuItem("Air", tabName = "air", icon = icon("plane",lib='glyphicon')),
            menuItem("RM", tabName = "rm", icon = icon("warning")),
            menuItem("RCPI", tabName = "rcpi", icon = icon("tint")),
            menuItem("CS", tabName = "cs", icon = icon("skull")),
            menuItem("EA", tabName = "ea", icon = icon("leaf")),
            menuItem("RN1", tabName = "rn1", icon = icon("fire")),
            menuItem("RN2", tabName = "rn2", icon = icon("exclamation")),
            menuItem("Disclaimer", tabName = "disc", icon = icon("send",lib='glyphicon'))
        )
    ),
    

    ## Dashboard
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        valueBoxOutput("value1"),
                        valueBoxOutput("value2"),
                        valueBoxOutput("value3"),
                        box(
                            title = "Revenue per Account",
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotOutput("revenuebyPrd", height = "300px")
                        ),
                        box(
                            title = "Revenue per Product",
                            status = "primary",
                            solidHeader = TRUE, 
                            collapsible = TRUE, 
                            plotOutput("revenuebyRegion", height = "300px")
                        ) 
                    )),
            
          # ALLSTAFF
          tabItem(tabName = "allstaff",
                  fluidPage(
                      titlePanel("Data Collection Template, 'All Staff' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("All EHP Staff"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("Region_as", "Region_as", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("Date (YY/MM/DD)_as", "Date (YY/MM/DD)_as", Sys.Date()),
                              textInput("Description", "Description", ""),
                              textInput("Lead EHP Program", "Lead EHP Program", ""),
                              textInput("Area Involved", "Area Involved", ""),
                              textInput("Additional EHP Program Area Involved", "Additional EHP Program Area Involved", ""),
                              textInput("Additional EHP Program Area Involved 1", "Additional EHP Program Area Involved 1", ""),
                              textInput("Additional EHP Program Area Involved 2", "Additional EHP Program Area Involved 2", ""),
                              textInput("Other Program/Branch (external to EHP) Involved", "Other Program/Branch (external to EHP) Involved", ""),
                              textInput("Effect of Outcome\tComments", "Effect of Outcome\tComments", ""),
                              actionButton("submit_as", "Submit"),
                              actionButton("clear_as", "Clear Form"), 
                              downloadButton("downloadData_as", "Download"),
                              actionButton("delete_as", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_as", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("<u><b>Lead EHP Program Area: </b></u>
                                           <br>EHP program where request originated
                                           <br>
                                           <br>
                                           <u><b>Collaborations: </b></u>
                                           <br>2 or more HC progam areas working together to 
                                           result in an outcome of substantial impact 
                                           <br>
                                           <br>
                                           <u><b>Substantial Impact: </b></u>
                                           <br>Outcome, beyond the routine, that advances the 
                                           program's objectives significantly
                                           <br>
                                           <br>
                                           <u><b>Note: </b></u>
                                           <br>Target of 3 for the whole office b/c this 
                                           should not reference routine work
                                           <br>
                                           <br>
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          #Air
          tabItem(tabName = "air",
                  fluidPage(
                      titlePanel("Data Collection Template, 'Air' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("All EHP Staff"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("Region_a", "Region_a", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("Date (YY/MM/DD)_a", "Date (YY/MM/DD)_a", Sys.Date()),
                              textInput("Description of Activity_a", "Description of Activity_a", ""),
                              textInput("Target Audience_a", "Target Audience_a", ""),
                              textInput("# participants_a", "# participants_a", ""),
                              textInput("# outreach surveys completed_a", "# outreach surveys completed_a", ""),
                              textInput("# participants  indicating improved understanding of subject matter_a", "# participants  indicating improved understanding of subject matter_a", ""),
                              textInput("# participants  indicating intent to apply material_a", "# participants  indicating intent to apply material_a", ""),
                              textInput("Did this activity result in a Notable Outcome with a Substantial Impact?_a", "Did this activity result in a Notable Outcome with a Substantial Impact?_a", ""),
                              textInput("Was another EHP Program Involved to  Facilitate an Outcome with Substantial Impact?_a", "Was another EHP Program Involved to  Facilitate an Outcome with Substantial Impact?_a", ""),
                              textInput("Outcome  (Linked to TB Submission)_a", "Outcome  (Linked to TB Submission)_a", ""),
                              textInput("Comments_a", "Comments_a", ""),
                              actionButton("submit_a", "Submit"),
                              actionButton("clear_a", "Clear Form"), 
                              downloadButton("downloadData_a", "Download"),
                              actionButton("delete_a", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_a", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("Indicator means to capture activities 
                                           completed as the liaison between the Region 
                                           and NCR, such as when providing regional 
                                           intelligence to NCR, or when responding to 
                                           stakeholder inquiries.
                                           <br>
                                           <u><b>Note: </b></u>
                                           <br>All the actions (i.e., each email or phone call) 
                                           taken to support the activity need not be itemized
                                           <br>
                                           <br>
                                           <u><b>Results: </b></u>
                                           <br>Data pertaining to 'improved understanding' and 
                                           'intent to apply material' are to be derived from 
                                           the outreach survey (only for events where it is 
                                           applicable to distribute said survey)
                                           <br>
                                           <br>
                                           <u><b>Substantial Impact: </b></u>
                                           <br>Outcome, beyond the routine, that advances the 
                                           program's objectives significantly
                                           <br>
                                           <br>
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          
          #RM
          tabItem(tabName = "rm",
                  fluidPage(
                      titlePanel("Data Collection Template, 'CMP-RM' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("CHEMICALS MANAGEMENT PLAN - RISK MANAGERS"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("Region_rm", "Region_rm", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("Date (YY/MM/DD)_rm", "Date (YY/MM/DD)_rm", Sys.Date()),
                              textInput("Compliance Promotion Activity", "Compliance Promotion Activity", ""),
                              textInput("Industry/Participants", "Industry/Participants", ""),
                              textInput("Timeline met for industry inquiry?", "Timeline met for industry inquiry?", ""),
                              textInput("Comments_rm", "Comments_rm", ""),
                              actionButton("submit_rm", "Submit"),
                              actionButton("clear_rm", "Clear Form"), 
                              downloadButton("downloadData_rm", "Download"),
                              actionButton("delete_rm", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_rm", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("
                                           <br>On an ongoing basis conduct activities to support 
                                           compliance promotion, for the industry and stakeholders on 
                                           their legal obligations related to Chemicals Management 
                                           Plan (includes inquiries, IFRs, FUO, mailouts, database 
                                           updates, comp-pro plan input, support/attend regional 
                                           workshops etc.).
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          #RCPI
          tabItem(tabName = "rcpi",
                  fluidPage(
                      titlePanel("Data Collection Template, 'CMP-RCPI' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("CHEMICALS MANAGEMENT PLAN - RCPI"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("Region_rc", "Region_rc", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              textInput("Type of Risk Communications Activity_rc", "Type of Risk Communications Activity_rc", ""),
                              dateInput("Date (YY/MM/DD)_rc", "Date (YY/MM/DD)_rc", Sys.Date()),
                              textInput("Name of Event (incl. Location)_rc", "Name of Event (incl. Location)_rc", ""),
                              textInput("Target Audience_rc", "Target Audience_rc", ""),
                              textInput("Enter a number for approximate number of interactions_rc", "Enter a number for approximate number of interactions_rc", ""),
                              textInput("Printed Material_rc", "Printed Material_rc", ""),
                              textInput("# outreach surveys completed_rc", "# outreach surveys completed_rc", ""),
                              textInput("# participants  indicating improved understanding of subject matter_rc", "# participants  indicating improved understanding of subject matter_rc", ""),
                              textInput("# participants  indicating intent to apply material_rc", "# participants  indicating intent to apply material_rc", ""),
                              textInput("Target Audience_rc", "Target Audience_rc", ""),
                              textInput("Was another EHP Program Involved to Facilitate an Outcome with Substantial Impact?_rc", "Was another EHP Program Involved to Facilitate an Outcome with Substantial Impact?_rc", ""),
                              textInput("Was this a CALM session?_rc", "Was this a CALM session?_rc", ""),
                              actionButton("submit_rc", "Submit"),
                              actionButton("clear_rc", "Clear Form"), 
                              downloadButton("downloadData_rc", "Download"),
                              actionButton("delete_rc", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_rc", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("
                                           <br>Report only on: <br> 
                                           i, CALM workshops and <br>
                                           ii, significant 'events', when there is an 
                                           in-person attendance
                                           <br>
                                           <br>
                                           <u><b>Results (of activity): </b></u>
                                           <br>Data pertaining to 'improved understanding' and 
                                           'intent to apply material' are to be derived from 
                                           the outreach survey (only for events where it is 
                                           applicable to distribute said survey)
                                           <br>
                                           <br>
                                           <u><b>Substantial Impact: </b></u>
                                           <br>Outcome, beyond the routine, that advances the 
                                           program's objectives significantly
                                           <br>
                                           <br>
                                           <u><b>Printed Material </b></u>
                                           <br>Plain Language/Factsheet - please provide 
                                           a URL or USBN to ID which document was distributed
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          #CS
          tabItem(tabName = "cs",
                  fluidPage(
                      titlePanel("Data Collection Template, 'Contaminated Sites' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("Contaminated Sites"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("REGION_cs", "REGION_cs", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("WHERE AND WHO_cs", "WHERE AND WHO_cs", Sys.Date()),
                              selectInput("FY 17-18 REPORTING MONTH_cs", "FY 17-18 REPORTING MONTH_cs", ""), 
                              textInput("CATEGORY_cs", "CATEGORY_cs", ""),
                              textInput("CS PROJECT Site Name &  FCSI #_cs", "CS PROJECT Site Name &  FCSI #_cs", ""),
                              selectInput("CS PROJECT TIME SHARING (YES/NO)_cs", "CS PROJECT TIME SHARING (YES/NO)_cs", ""),
                              textInput("CS PROJECT TIME SHARING (YES/NO)_cs", "CS PROJECT TIME SHARING (YES/NO)_cs", ""),
                              numericInput("CS PROJECT TASK_cs", "CS PROJECT TASK_cs", 0),
                              numericInput("CS PROGRAM TASK 1_cs", "CS PROGRAM TASK 1_cs", 0),
                              numericInput("DURATION (IN 0.5 HRS)_cs", "DURATION (IN 0.5 HRS)_cs", 0),
                              numericInput("COMMENTS (For Added Clarity)_cs", "COMMENTS (For Added Clarity)_cs", 0),
                              textInput("FOR RORB (IF APPLICABLE) WAS EHP PMF CS SERVICE STD MET? (YES/NO)_cs", "FOR RORB (IF APPLICABLE) WAS EHP PMF CS SERVICE STD MET? (YES/NO)_cs", ""),
                              textInput("IF SERVICE STD WAS NOT MET, INCLUDE DETAILS BELOW._cs", "IF SERVICE STD WAS NOT MET, INCLUDE DETAILS BELOW._cs", ""),
                              actionButton("submit_cs", "Submit"),
                              actionButton("clear_cs", "Clear Form"), 
                              downloadButton("downloadData_cs", "Download"),
                              actionButton("delete_cs", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_cs", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("<u><b>Service Standard </b></u>
                                           <br>EHP standard defined by FCSAP service standards, and custodian 
                                           needs.  Applicable FCSAP service standards will be 15 business days 
                                           (NCSCS/ASCS reviews), 30 business days (for technical reviews), 
                                           or renegotiated timelines agreed to with Custodian.  
                                           Timelines required for non-FCSAP federal sites to be negotiated 
                                           and mutually agreed upon.
                                           <br>
                                           <br>
                                           <u><b>Cautions: </b></u>:
                                           Do not include Environmental Assessment reviews here.  
                                           If relevant, coordinate with EA Coordinator to record under 'All Staff'.
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          #EA
          tabItem(tabName = "ea",
                  fluidPage(
                      titlePanel("Data Collection Template, 'Environmental Assessment' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("ENVIRONMENTAL ASSESSMENT"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("REGION_ea", "REGION_ea", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("WHERE AND WHO_ea", "WHERE AND WHO_ea", Sys.Date()),
                              selectInput("FY 17-18 REPORTING MONTH_ea", "FY 17-18 REPORTING MONTH_ea", ""), 
                              textInput("CATEGORY_ea", "CATEGORY_ea", ""),
                              textInput("EA PROJECT NAME_ea", "EA PROJECT NAME_ea", ""),
                              selectInput("EA PROJECT TIME SHARING (YES/NO)_ea", "EA PROJECT TIME SHARING (YES/NO)_ea", ""),
                              textInput("EA PROJECT STAGE_ea", "EA PROJECT STAGE_ea", ""),
                              numericInput("EA PROJECT CATEGORY SUBTYPE_ea", "EA PROJECT CATEGORY SUBTYPE_ea", 0),
                              numericInput("EA PROGRAM CATEGORY SUBTYPE 1_ea", "EA PROGRAM CATEGORY SUBTYPE 1_ea", 0),
                              numericInput("ABORIGINAL CONSULTATION (YES/NO)_ea", "ABORIGINAL CONSULTATION (YES/NO)_ea", 0),
                              numericInput("DURATION (IN 0.5 HRS)_ea", "DURATION (IN 0.5 HRS)_ea", 0),
                              textInput("COMMENTS (For Added Clarity)_ea", "COMMENTS (For Added Clarity)_ea", ""),
                              textInput("FOR RORB (IF APPLICABLE) WAS EHP PMF EA SERVICE STD MET? (YES/NO)_ea", "FOR RORB (IF APPLICABLE) WAS EHP PMF EA SERVICE STD MET? (YES/NO)_ea", ""),
                              textInput("IF SERVICE STD WAS NOT MET, INCLUDE DETAILS BELOW_ea", "IF SERVICE STD WAS NOT MET, INCLUDE DETAILS BELOW_ea", ""),
                              actionButton("submit_ea", "Submit"),
                              actionButton("clear_ea", "Clear Form"), 
                              downloadButton("downloadData_ea", "Download"),
                              actionButton("delete_ea", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_ea", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("<u><b>Substantial impact: </b></u>
                                           <br>Outcome, beyond the routine, that 
                                           advances the program's objectives significantly
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          #RN1
          tabItem(tabName = "rn1",
                  fluidPage(
                      titlePanel("Data Collection Template, 'Radon(1)' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("ENVIRONMENTAL RADIATION MONITORING AND PROTECTION"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("REGION_rn1", "REGION_rn1", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("Date (YY/MM/DD)_rn1", "Date (YY/MM/DD)_rn1", Sys.Date()),
                              selectInput("Partner Group\tIf Other_rn1", "Partner Group\tIf Other_rn1", ""), 
                              textInput("Partner Group (Add comment)_rn1", "Partner Group (Add comment)_rn1", ""),
                              textInput("Name of Group_rn1", "Name of Group_rn1", ""),
                              selectInput("Type of Project_rn1", "Type of Project_rn1", ""),
                              textInput("Description of Project_rn1", "Description of Project_rn1", ""),
                              numericInput("Other EHP Program Involved_rn1", "Other EHP Program Involved_rn1", 0),
                              numericInput("Comment_rn1", "Comment_rn1", 0),
                              actionButton("submit_rn1", "Submit"),
                              actionButton("clear_rn1", "Clear Form"), 
                              downloadButton("downloadData_rn1", "Download"),
                              actionButton("delete_rn1", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                                  tabPanel("Data",
                                           p(h4("About")),
                                           dataTableOutput("responses_rn1", width = 800)),
                                  tabPanel("Help",
                                           p(h4("Key term definitions:")),
                                           HTML("<u><b>Substantial impact: </b></u>
                                           <br>Outcome, beyond the routine, that 
                                           advances the program's objectives significantly
                                           ")
                                  )
                              )
                          )
                      )
                  )),
          
          
          ##RN2
          
          tabItem(tabName = "rn2",
                  fluidPage(
                      titlePanel("Data Collection Template, 'Radon(2)' Key Term Definitions and FY 17-18 Targets"),
                      sidebarLayout(
                          sidebarPanel(
                              helpText("ENVIRONMENTAL RADIATION MONITORING AND PROTECTION"),
                              br(),
                              #input names need to be equal to col names
                              selectInput("Region", "Region", 
                                          c("ON", "AB", "QC", "BC", "MB")), 
                              dateInput("Date (YY/MM/DD)", "Date (YY/MM/DD)", Sys.Date()),
                              selectInput("Stakeholder Group", "Stakeholder Group", 
                                          c("Industry", "Municipal", "NGO", "Provincial", "Public")), 
                              textInput("If Other Stakeholder Group (Add comment)", "If Other Stakeholder Group (Add comment)", ""),
                              textInput("Name of Group/Event", "Name of Group/Event", ""),
                              selectInput("Method of Engagement", "Method of Engagement", 
                                          c("Presentation", "E&A Contract", "Research")),
                              textInput("If Other Method of Engagement (Add comment)", "If Other Method of Engagement (Add comment)", ""),
                              numericInput("Number of Participants/Interactions", "Number of Participants/Interactions", 0),
                              numericInput("# outreach surveys completed", "# outreach surveys completed", 0),
                              numericInput("# participants indicating improved understanding of subject matter", "# participants indicating  improved understanding  of subject matter", 0),
                              numericInput("# participants indicating intent to apply material", "# participants indicating intent to apply material", 0),
                              textInput("Did this activity result in a Notable Outcome with a Substantial Impact?", "Did this activity result in a Notable Outcome with a Substantial Impact?", ""),
                              textInput("Was another EHP Program Involved to Facilitate an Outcome with Substantial Impact?", "Was another EHP Program Involved to Facilitate an Outcome with Substantial Impact?", ""),
                              textInput("Comments", "Comments", ""),
                              actionButton("submit", "Submit"),
                              actionButton("clear", "Clear Form"), 
                              downloadButton("downloadData", "Download"),
                              actionButton("delete", "Delete All Data")
                          ),
                          mainPanel(
                              tabsetPanel(
                              tabPanel("Data",
                                       p(h4("About")),
                                       dataTableOutput("responses", width = 800)),
                              tabPanel("Help",
                                       p(h4("Key term definitions:")),
                                       HTML("<u><b>Number of Participants/ Interactions: </b></u>
                                           <br>Number of people that participated (examples: meetings, workshop, 
                                           technical committee, working group, E&A contract, research) or 
                                           interacted (examples: trade-shows, presentations) in a radon event.
                                           <br>
                                           <br>
                                           <u><b>Results: </b></u>:
                                           Data pertaining to 'improved understanding' and 'intent to apply material' 
                                           are to be derived from the outreach survey (only for events where it is 
                                           applicable to distribute said survey)
                                           <br>
                                           <br>
                                           <u><b>Substantial Impact: </b></u>:
                                           Outcome, beyond the routine, that advances the program's objectives 
                                           significantly
                                           ")
                                       )
                              )
                          )
                      )
                  )),
          
          tabItem(tabName = "disc", 
                  p(h4("Proof of Concept:")),
                  HTML(#"<u><b>Equation for calculation: </b></u>
                      "<br> Trial application developed by DABS.
                               <br>
                               <br>
                               Release: Nov 21st, 2019 <br>
                               "))
        )
        
    ),
    skin = "red"

)

# dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')
    