
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rhandsontable)
library(shinythemes)


shinyUI(
  navbarPage(title = div(img(src="img/nxp-logo.png",height="30px", width="100px"), tags$b("CAB Script Analysis")), inverse=TRUE, theme = shinytheme("spacelab"), 
             footer = em(style="color:dodgerblue;", "NXP Confidential and Proprietary"),
             
             header=tags$head(
               
               tags$style(HTML("
                               .shiny-server-account {
                               z-index:2147483647;
                               }",
                               ".shiny-output-error-validation {
                              font-family: 'Lobster', cursive;
                              font-weight: 800;
                              line-height: 1.1;
                               color: green;}
                               
                               .checkbox-inline { 
                              margin-left: 0px;
                               margin-right: 10px;
                               }
                               .checkbox-inline+.checkbox-inline {
                               margin-left: 0px;
                               margin-right: 10px;
                               }
                               "
                               ))
             ),
             
            
  
  
  tabPanel("CAB LIST",
          
           tabsetPanel(
           
             
             tabPanel("Split Editor",
                      fluidRow(
                        
                        column(6,  
                               
                               h3("Search for Split Data in eSWR"),
                               
                               br(),  
                               
                               fluidRow(
                                 
                                 column(5,
                                        
                                        #CAB ID input
                                        textInput(inputId="cab_id", label = "CAB Search", value = NULL)),
                                 column(6,
                                        #Lot ID input
                                        textAreaInput(inputId="split_lotid", label = tags$b("Lot ID Search; separate by ','"), value = NULL,
                                                      width = '200px', height = '100px', resize = 'both')),
                                 
                                 column(1,
                                        br(),
                                        actionButton("getsplitdata", "Go"))

                                        ),
                               
                               verbatimTextOutput("cabStr"),
                                
                               conditionalPanel(
                                 condition = "input.getsplitdata",
                                 
                                 tags$style(type='text/css', '#CreateNewSplit {background-color: rgba(255,255,0,0.40); color: red; 
                                            font-family: "Georgia", Times, "Times New Roman", serif; font-weight: Normal; font-size: 16px}'),
                                 verbatimTextOutput("CreateNewSplit")
                               ),
                                
                                uiOutput("splitIDSelection"),
                               
                           
                               conditionalPanel(
                                 condition = "input.getsplitdata",
                                 div(shinycssloaders::withSpinner(dataTableOutput('splitSummTable')), style = "font-size:80%")
                               ),
                               br(),
                               
                               conditionalPanel(
                                 condition = "input.getsplitdata",
                                 
                                 wellPanel(
                                   tags$b("Select the splits you want to do analysis on"),
                                   conditionalPanel(
                                     condition = "input.splitid_4analysis",
                                     checkboxInput("select_allsplit", 'All/None',  value = FALSE)
                                   ),
                                   
                                   uiOutput("splitList4Analysis"),
                                   
                                   conditionalPanel(
                                     condition = "input.splitid_4analysis",
                                     checkboxInput("combine_split", "Do you want to combine them for analysis?"),
                                     actionButton("do_analysis","Generate Split String")
                                   )
                                 ),
                                 
                                 verbatimTextOutput("splitStr")

                               ),
                               
                               checkboxInput("show_splitdtltbl", tags$b('Show Detail Data'),  value = FALSE),
                               
                               conditionalPanel(
                                 condition = "input.show_splitdtltbl",
                                 div(dataTableOutput('splitDtlTable'), style = "font-size:80%")
                               )

                               ),   #column 6 for search for split in eSWR
                        
                        column(1),
                        
                        column(5,   #define new split
                               h3(tags$b("Define New Split in eSWR System (Recommended)")),
                               br(),
                               
                               tags$b("Links to the eSWR System:"),
                               helpText(a(tags$b("eSWR for ATMC"),href="http://web-apps.am.freescale.net/cgi-bin/swr/bin/swr_home.cgi?fab=m13")),
                               helpText(a(tags$b("eSWR for CHD"),href="http://web-apps.am.freescale.net/cgi-bin/swr/bin/swr_home.cgi?fab=m12")),
                               helpText(a(tags$b("eSWR for OHT"),href="http://web-apps.am.freescale.net/cgi-bin/swr/bin/swr_home.cgi?fab=m11")),
                               
                               br(),  
                               h3(tags$b("Create New Split in Shiny (won't be saved to eSWR)")),
                               
                               fluidRow(
                                 column(8,
                                        textAreaInput(inputId="split_newlotid", label = tags$b("Step1: Input lot ID, separated by ','"), value = NULL,
                                                      width = '400px', height = '100px', resize = 'both')),
                                 column(1
                                        # br(),
                                        # actionButton("createsplit", "Go")
                                      ),
                                 column(3)
                               ),
                               
                               
                               tags$b("Step2: Define # of split levels"),
                               
                               selectizeInput(inputId = "split_lvl", label = NULL, choices = c(1:24), multiple = TRUE, selected = NULL, width = '20%', options = list(maxItems = 1)),
                               
                               tags$b("Step3: Choose Wafers for each level of split"),
                               br(),br(),
                               
                               conditionalPanel(
                                 condition = "input.split_lvl",
                                 uiOutput("waferSelection_baseline"),
                                 shinycssloaders::withSpinner(uiOutput("waferSelection"))
                               ),
                               
                               actionButton("addwfrtobl", "Save Split to Table"),
                               br(),br(),
                               
                               conditionalPanel(
                                 condition = "input.addwfrtobl",
                                 shinycssloaders::withSpinner(dataTableOutput("newSplitTbl"))
                               ),
                               conditionalPanel(
                                 condition = "input.split_lvl",
                                 checkboxInput("combine_split_shiny", "Do you want to combine lots for analysis?"),
                                 actionButton("do_analysis_shiny","Generate Split String")
                               ),
                               
                               conditionalPanel(
                                 condition = "input.do_analysis_shiny",
                                 verbatimTextOutput("cabStr_shiny"),
                                 verbatimTextOutput("splitStr_shiny")
                               )
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               )   # #column 6 end for define new split
                        
                        
                        
                       
                        
                        )
                      
          
                      
                        
                        
                      ),    #split panel end
             
             
             tabPanel("Pilot/Ramp Editor",
                      
                      br(), br(),
                      
                      fluidRow(
                        column(5,
                               textAreaInput(inputId="baseline_lotid", label = tags$b("lot ID for baseline, separated by ','"), value = NULL,
                                             width = '400px', height = '100px', resize = 'both'),
                               
                               textAreaInput(inputId="pilot_lotid", label = tags$b("lot ID for pilot, separated by ','"), value = NULL,
                                             width = '400px', height = '100px', resize = 'both')
                               ),
                        column(1
                               # br(),
                               # actionButton("createsplit", "Go")
                               
                        ),
                        column(6)
                      )
                      
                      )
             
             
           )    #tabsetPanel end
      
           ) #"CAB LIST" tab end

  
)  #navbarPage end
)  #shinyUI end
