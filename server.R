
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(tidyverse)
library(DT)
library(assertthat)
library(rhandsontable)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
#library(testthat)

shinyServer(function(input, output,session) { 
  
  source("dbconn.R")

  # split_lot <- read.csv("SPLIT_LOT.csv", stringsAsFactors = F)
  # split_waf <- read.csv("SPLIT_WAF.csv", stringsAsFactors = F)
  
  #join tables on SPLIT_ID
  split_joineddata <- inner_join(split_waf, split_lot, by = c("SPLIT_ID" = "SPLIT_ID"))
  
  #data re-format
  split_data <- split_joineddata %>% 
    #filter data to only contain active lot
    filter(STATUS == "A") %>%   
    # #split the WAFTERS column to multiple rows each stores a single wafer
    # mutate(WAFERS = strsplit(as.character(WAFERS), ",")) %>%    
    # unnest(WAFERS) %>%
    #select useful columns
    dplyr::select(SPLIT_ID, SPLIT_NUM, LOTID.y, SPLIT_NAME, RECIPE, WAFER_CNT, WAFERS, BASELINE, SPLIT_CMT, FAB, PART, 
           MOO, TECH, LOTTYPE, PRIORITY, CURMAINQTY, LOTCMNT, STAGE, EXPERIMENT, DESCRIPTION) %>%
    rename(LOT_ID = LOTID.y, WAFER_ID = WAFERS)
  
  #all LOT ID from Edwin's split data
  splitdata_lotlist <- unique(split_data$LOT_ID)
  
  
  #re-format lot ID input
  
  split_lotid <- reactive({
    
    req(input$split_lotid)
    
    
    #lotid <- 'DV10959.1K'
    #lotid <- "DD67064.1J, DD67324.1J, DD66916.1X"
    #lotid <- "DD67064.1J, DD67324.1J, DD66916.1"
    lotid <- input$split_lotid
    
    # print(typeof(lotid))
    # 
    # print(lotid)
    
    #split input lotid string, remove leading and trailing whitespace
    split_lotid <- trimws(unlist(strsplit(lotid, ",")), which = "both")
    
    return(split_lotid)
    
  })
  
  
  #filter data on user-input lot ID
  split_lotdata <- eventReactive(input$getsplitdata, {
    
    req(split_lotid())
    
    split_lotid <- split_lotid()
    
    #need at least one lot id existing in eswr
    shiny::validate(
      need(any(split_lotid %in% splitdata_lotlist), message = FALSE)
    )
    
    #filter split data to specified lots
    split_lotdata <- filter(split_data, LOT_ID %in% as.character(split_lotid))
    
    return(split_lotdata)

  })
  
  #generate the list of split id of the user input lots
  splitid_list <- reactive({
    req(split_lotdata())
    
    # split_lotid <- split_lotid()
    # #need at least one lot id existing in eswr
    # shiny::validate(
    #   need(any(split_lotid %in% splitdata_lotlist), message = FALSE)
    # )

    split_lotdata <- split_lotdata()
    
    splitid_list <- sort(unique(split_lotdata$SPLIT_ID))
    
    return(splitid_list)
    
  })
  
  
  #split_ID selection 
  output$splitIDSelection <- renderUI({
    
    req(splitid_list())
    
    result <- radioButtons(inputId = "splitid_input", label = "Explore data by SPLIT_ID",
                           choices = c("All" = "all",splitid_list()),
                           selected = "all", inline = T)
    
  })
  
  
  #filter lot data on split ID input
  split_filteredlotdata <- reactive({
    
    req(split_lotdata(), input$splitid_input)
    
    if(input$splitid_input == "all"){
      
      split_filteredlotdata <- split_lotdata()
      
    } else{
      split_lotdata <- split_lotdata()
      
      splitid <- input$splitid_input
      
      #print(splitid)
      
      split_filteredlotdata <- filter(split_lotdata, SPLIT_ID == splitid)
      
    }
    
    return(split_filteredlotdata)

  })
  
  
  
  output$splitSummTable <- DT::renderDataTable({
    
    req(split_filteredlotdata(), input$splitid_input)
    
    split_filteredlotdata <- split_filteredlotdata()
    
    DT::datatable(split_filteredlotdata %>% 
                    select(SPLIT_ID, LOT_ID, MOO, WAFER_ID, SPLIT_NAME, BASELINE, STAGE, EXPERIMENT) %>%
                    arrange(SPLIT_ID, BASELINE), 
                  #filter = "top", 
                  style = "bootstrap", options = list(
                    
                    pageLength = 10,
                    #paging = FALSE,
                    searching = TRUE,
                    lengthChange = FALSE,
                    autoWidth = TRUE,
                    orderClasses = TRUE       #sorted columns are colored
                    #info = FALSE,
                    #ordering = FALSE#,
                    #columnDefs = list(list(targets = c(0:11), searchable = FALSE))
                  ))
    
    # #display all splits if user selects "all"
    # if(input$splitid_input == "all") {
    #   
    #   DT::datatable(split_lotdata %>% select(SPLIT_ID, LOT_ID, MOO, WAFER_ID, SPLIT_NAME, BASELINE, STAGE, EXPERIMENT), 
    #                 #filter = "top", 
    #                 style = "bootstrap", options = list(
    #     
    #     pageLength = 10,
    #     #paging = FALSE,
    #     searching = TRUE,
    #     lengthChange = FALSE,
    #     autoWidth = TRUE,
    #     orderClasses = TRUE       #sorted columns are colored
    #     #info = FALSE,
    #     #ordering = FALSE#,
    #     #columnDefs = list(list(targets = c(0:11), searchable = FALSE))
    #   ))
    #   
    # } 
    
  })
  
  
  
  #UI of split analysis list selection
  output$splitList4Analysis <- renderUI({
    
    req(splitid_list())
    
    splitid_list <- splitid_list()
    
    checkboxGroupInput('splitid_4analysis', label = NULL, choices = splitid_list, selected = NULL, inline = TRUE)
    
  })
  
  
  observe({
    updateCheckboxGroupInput(
      session, 'splitid_4analysis', choices = splitid_list(),
      selected = if(input$select_allsplit) splitid_list(), inline = TRUE
    )
  })
  
  
  
  #output split detail data for selected lots
  output$splitDtlTable <- DT::renderDataTable({
    
    req(split_filteredlotdata(), input$splitid_input)
    
    split_filteredlotdata <- split_filteredlotdata()
    
    DT::datatable(split_filteredlotdata, filter = "top", style = "bootstrap", options = list(
      
      pageLength = 10,
      #paging = FALSE,
      searching = TRUE,
      lengthChange = FALSE,
      autoWidth = TRUE,
      orderClasses = TRUE       #sorted columns are colored
      #info = FALSE,
      #ordering = FALSE#,
      #columnDefs = list(list(targets = c(0:11), searchable = FALSE))
    )) #%>% 

  })
  
  
  #check for existence of user-input lot id's in eswr system
  #print message asking user to define new split if lot does not exist
  output$CreateNewSplit <- renderPrint({
    
    req(split_lotid())
    
    split_lotid <- split_lotid()
    
    if(!all(split_lotid %in% splitdata_lotlist)) {
      newlot <- split_lotid[!split_lotid %in% splitdata_lotlist]
      
      #print(newlot)
      
      cat("No Results found for Lot ", paste(newlot,  collapse = ", "), " in eSWR. \nPlease create new split on the right panel.", sep ="")
      
    }
  })
  
##############Create New Split UI#####################  
  
  #create multiple wafer selection UI based on user-input split level
  
  #base line wafer selection
  output$waferSelection_baseline <- renderUI({
    
    req(input$split_lvl)
    
    list(
      tags$p(h4("BaseLine")),
      fluidRow(
        column(4,
               #h4("BaseLine")
               textInput(inputId= "level_baseline", label = " Split Name", value = NULL, width = '100%')
        ),
        column(8,
               checkboxGroupInput(inputId = "wafers_baseline", label = "Select Wafers", choices = c(1:25), selected = NULL, inline = TRUE)
        )
      )
    )
    
  })
  
  output$waferSelection <- renderUI({
    
    req(input$split_lvl)
    
    #print(input$input$split_lvl)
    
    #numLvl <- 3
    numLvl <- as.integer(input$split_lvl)
    
    lapply(1:(numLvl-1), function(i) {
      
      list(
       tags$p(h4(paste0("Level ", i))),
       fluidRow(
         column(4,
                textInput(inputId= paste0("level_",i), label = " Split Name", value = NULL, width = '100%')
         ),
         column(8,
                checkboxGroupInput(inputId = paste0("wafers_", i), label = "Select Wafers", choices = c(1:25), selected = NULL, inline = TRUE)
         )
       )
      )
    })
  })
  
  
  # observe({
  #   if(input$addwfrtobl > 0){
  #     
  #     isolate({
  #       updateSelectizeInput(
  #         session, 'split_lvl',
  #         #selected = newselected()
  #         selected = NULL)
  #     })
  #   }
  # })
  
  
  
  # create reactiveValues to store new split tbl 
  v <- reactiveValues()
  
  v$new_split_tbl <- data.frame(
    SPLIT_NUM <- integer(0),
    LOT_ID = character(0),
    #MOO = rep(NA, length(split_newlot) *25),
    SPLIT_NAME = character(0),
    WAFER_ID = character(0),
    BASELINE = integer(0)
    #STAGE = rep(NA,  length(split_newlot) *25),
    #EXPERIMENT = rep(NA, length(split_newlot) *25)
  )
 
  #write split data into table
  observeEvent(input$addwfrtobl, {
    
    req(input$split_newlotid, input$split_lvl)
    
    if(input$addwfrtobl > 0){
      
      isolate({
        
        #numLvl <- 3
        numLvl <- as.integer(input$split_lvl)
        
        #check no wafer duplicates among different split levels
        waferlist_base <- as.character(c(1:25))
        #waferlist_all <- c()
        #initiate waferlist_all to be the baseline wafers
        waferlist_all <- input$wafers_baseline
        
        for (i in 1:(numLvl-1)) {
          # wafter_list <- c("13", "14", "15")
          wafer_list <- input[[paste0("wafers_", i)]]
          
          waferlist_all <- c(waferlist_all, wafer_list)
        }
        
         # print("waferlist_all: ")
         # print(waferlist_all)
        
        #wafer selection check
        shiny::validate(
          #assert no duplicated wafers
          need(assertthat::are_equal(anyDuplicated(waferlist_all), 0), message = "Warning: You have duplicated wafer selections! Please fix it.")#,
          #assert all wafers are assigned to a split level
          #need(assertthat::are_equal(waferlist_all, waferlist_base), message = "Warning: You haven't assigned all the wafers! Please fix it.")
        )
        
        
        #get the new lot list based on user-input new lot id
        #split_newlotinput<- "TM54160.1W"
        split_newlotinput <- input$split_newlotid
        split_newlot <- trimws(unlist(strsplit(split_newlotinput, ",")), which = "both")
        
        #create split table with baseline split info
        splitname_baseline <- input$level_baseline
        
        split_tbl <- data.frame(
          SPLIT_NUM = rep(0, length(split_newlot)),
          LOT_ID = split_newlot,
          #SPLIT_NAME = rep("baseline", length(split_newlot)),
          SPLIT_NAME = rep(splitname_baseline, length(split_newlot)),
          WAFER_ID = rep(NA,length(split_newlot)),
          BASELINE = rep(1, length(split_newlot))
          )
        
        #concatenate wafers list to a string
        waferlist_baseline <- paste(input$wafers_baseline, collapse = ",")
        #paste(c("1","2","3","4"), collapse = ",")
        
        split_tbl$WAFER_ID <- waferlist_baseline
        #split_tbl$WAFER_ID <- paste(c("1","2","3","4"), collapse = ",")
        
        for (i in 1:(numLvl-1)){
         
          wafers <- input[[paste0("wafers_", i)]]
          split_name <- input[[paste0("level_",i)]]
          # wafers <- c("13", "14", "15")
          # split_name <- "Level1"
          
          wafer_list <- paste(wafers, collapse = ",")
          #print(str(wafer_list))
          print("wafer list & split_name:")
          print(wafer_list)
          print(split_name)
          
          split_tbl_new <- data.frame(
            SPLIT_NUM = rep(i, length(split_newlot)),
            LOT_ID = split_newlot,
            SPLIT_NAME = rep(split_name, length(split_newlot)),
            WAFER_ID = rep(NA,length(split_newlot)),
            BASELINE = rep(0, length(split_newlot))
          )
          
          split_tbl_new$WAFER_ID <- wafer_list
          
          split_tbl <- rbind(split_tbl, split_tbl_new)
        }
        
        v$new_split_tbl <- rbind(v$new_split_tbl, split_tbl)
        
      })

    }
  })
  
  
  
  #create new split table
  output$newSplitTbl <- DT::renderDataTable({
    
    new_split_tbl <- v$new_split_tbl
    
    #make WAFER_ID as two-digit character
    #new_split_tbl$WAFER_ID <- str_pad(new_split_tbl$WAFER_ID, width=2, side = "left", pad="0")
    new_split_tbl <- arrange(new_split_tbl, LOT_ID, SPLIT_NUM)
    
    DT::datatable(new_split_tbl, style = "bootstrap", options = list(
      
      pageLength = 10,
      #paging = FALSE,
      searching = TRUE,
      lengthChange = FALSE,
      autoWidth = TRUE,
      orderClasses = TRUE       #sorted columns are colored
      #info = FALSE,
      #ordering = FALSE#,
      #columnDefs = list(list(targets = c(0:11), searchable = FALSE))
    ))
    
    # rhandsontable(new_split_tbl, width = 1000)  %>% 
    #   hot_cols(columnSorting = TRUE)
    
    
    
  })
  
#################CREATE SPLIT STRINGS#############################  
  
  #create condition string to retrieve lot data from hive -- eSWR 
  cab_condition <- reactive({
    
    req(split_lotid())
    
    split_lotid <- split_lotid()
    
    #split_lotid <- c("DD67064.1J", "DD67324.1J", "DD66916.1X")
    
    lotlist <- split_lotid[split_lotid %in% splitdata_lotlist]
    
    str <- paste0('"', lotlist, '"', collapse = ",")
    
    cab_condition <- paste("lot_id IN (", str, ")", sep="")
    
    return(cab_condition)
    
    
  })
  
  output$cabStr <- renderPrint({
    
    req(cab_condition())
    
    print("cab_condition:")
    cab_condition()
  })
  
  
  #create split condition string -- eSWR
  split_conditions_list <- eventReactive(input$do_analysis, {
    
    req(split_lotdata(), input$splitid_4analysis)
    
    split_lotdata <- split_lotdata()
    
    splitid_selected <- input$splitid_4analysis
    
   
    
    #splitid_selected <- c("09-101041", "09-101043", "09-101091", "09-101195")
    #splitid_selected <- c("09-101041")
    #splitid_selected <- c("09-101065", "09-101066")
    
    split_targetdata <- split_lotdata %>% filter(SPLIT_ID %in% splitid_selected) %>%
      arrange(SPLIT_ID, SPLIT_NUM)
    
    #create a list of lists to store all the split_conditions 
    split_conditions_list <- list()

    
    #combine splits if user chooses to 
    if(input$combine_split){
      
      #split_conditions <- list()
      
      data_bsl <- filter(split_targetdata, BASELINE == 1)
      data_exp <- filter(split_targetdata, BASELINE ==0)
      
      data_bsl$WAFER_ID <- sapply(data_bsl$WAFER_ID, FUN = function(x) paste0(as.integer(unlist(strsplit(x, ","))), collapse=','))
      
      #create baseline string
      str_bsl <- paste0('lot_id == "',data_bsl$LOT_ID, '"', " & wafer_id %in% c(", data_bsl$WAFER_ID, ")", collapse = " | ")
      split_conditions["baseline"] <- str_bsl
      
      #create experiment string
      for (i in sort(unique(data_exp$SPLIT_NUM))){
        
        data_exp_lvl <- filter(data_exp, SPLIT_NUM == i)
        
        data_exp_lvl$WAFER_ID <- sapply(data_exp_lvl$WAFER_ID, FUN = function(x) paste0(as.integer(unlist(strsplit(x, ","))), collapse=','))
        
        str_exp <- paste0('lot_id == "',data_exp_lvl$LOT_ID, '"', " & wafer_id %in% c(", data_exp_lvl$WAFER_ID, ")", collapse = " | ")
        split_conditions[paste0("experiment", i)] <- str_exp
        
      }
      
      split_conditions_list[["split_conditions_1"]] <- split_conditions
      
      #print(split_conditions)
      # return(split_conditions_list)
      
      
    } else {  #do not combine splits
      
      # split_conditions_list <- list()
      
      # print("splitid_selected:")
      # print(splitid_selected)
      # print(seq(splitid_selected))
      
      #loop over selected split ids
      for(id_seq in seq(splitid_selected)){
        
        id <- splitid_selected[id_seq]
        
        data <- filter(split_targetdata, SPLIT_ID == id)
        data_bsl <- filter(data, BASELINE == 1)
        data_exp <- filter(data, BASELINE ==0)
        
        lot_id <- unique(data$LOT_ID)
        #lot_id <- "DE91953.1F"
        
        #baseline wafers
        wafer_list_bsl <- as.integer(unlist(strsplit(data_bsl$WAFER_ID, ",")))
        wafer_list_bsl <- paste0(wafer_list_bsl, collapse=',')
        
        #starts split_conditions with baseline
        split_conditions <- list("baseline" = paste0('lot_id == "',lot_id, '"', " & wafer_id %in% c(", wafer_list_bsl, ")"))
        
        #experimental wafers
        #SPLIT_NUM defines how many levels of experiments
        exp_lvls <- data_exp$SPLIT_NUM
        
        #add experiments to split_conditions
        for (i in exp_lvls){
          data_exp_lvl <- filter(data_exp, SPLIT_NUM == i)
          
          wafer_list_exp <- as.integer(unlist(strsplit(data_exp_lvl$WAFER_ID, ",")))
          wafer_list_exp <- paste0(wafer_list_exp, collapse=',')
          
          split_conditions[paste0("experiment", i)] <- paste0('lot_id == "',lot_id, '"', " & wafer_id %in% c(", wafer_list_exp, ")")
          
          # split_conditions = list("baseline" = paste0('LOTID == "',lot_id, '"', " & WAFER_NUM %in% c(", wafer_list_bsl, ")"),
          #                         paste0("experiment", i) = paste0('LOTID == "',lot_id, '"', " & WAFER_NUM %in% c(", wafer_list_exp, ")")
          # )
        }
        
        split_conditions_list[[paste0("split_conditions_", id_seq)]]<- split_conditions
        
        # #functionality test(comment out later)
        # atmc_split <- read.csv("ATMC13lot_Part_Lot_Insert.csv", stringsAsFactors = F)
        # split_condition_bsl = split_conditions[["baseline"]]
        # split_condition_exp = split_conditions[["experimental"]]
        # dt_bsl <- filter_(atmc_split, split_condition_bsl)
        # dt_exp <- filter_(atmc_split, split_condition_exp)
      }
      
      #print(split_conditions_list)
      
      # return(split_conditions_list)
      
    }
    
    return(split_conditions_list)
    
  })
  
  
  
  output$splitStr <- renderPrint({
    
    print("split_conditions_list:")
    split_conditions_list()
    
  })
  
  
  #create cab condition string to retrieve lot data from hive -- shiny 
  cab_condition_shiny <- reactive({
    
    req(input$split_newlotid, input$split_lvl)
    
    new_split_tbl <- v$new_split_tbl
    
    #re-formatting
    #split_newlotinput <- input$split_newlotid
    #split_newlot <- trimws(unlist(strsplit(split_newlotinput, ",")), which = "both")
    #split_newlotinput<- "DD67064.1J, DD67324.1J, DD66916.1X"
    #split_newlot <- c("DD67064.1J", "DD67324.1J", "DD66916.1X")
    
    split_newlot <- unique(new_split_tbl$LOT_ID)
    
    str <- paste0('"', split_newlot, '"', collapse = ",")
    
    cab_condition <- paste("lot_id IN (", str, ")", sep="")
    
    cab_condition_print <<- cab_condition
    
    return(cab_condition)
    
    
  })
  
  output$cabStr_shiny <- renderPrint({
    
    req(cab_condition_shiny())
    
    print("cab_condition:")
    cab_condition_shiny()
  })
  
  
  
  #create split condition string -- shiny
  split_conditions_list_shiny <- eventReactive(input$do_analysis_shiny, {
    
    req(input$split_newlotid, input$split_lvl)
    
    new_split_tbl <- v$new_split_tbl
    
    split_newlot <- unique(new_split_tbl$LOT_ID)
    
    #split level
    numLvl <- as.integer(input$split_lvl)
    #numLvl <- 3
    
    split_conditions_list <- list()
    
    #combine splits if user chooses to 
    if(input$combine_split_shiny){
      
      split_conditions <- list()
      
      data_bsl <- filter(new_split_tbl, BASELINE == 1)
      data_exp <- filter(new_split_tbl, BASELINE ==0)
      
      data_bsl$WAFER_ID <- sapply(data_bsl$WAFER_ID, FUN = function(x) paste0(as.integer(unlist(strsplit(x, ","))), collapse=','))
      
      #create baseline string
      str_bsl <- paste0('lot_id == "',data_bsl$LOT_ID, '"', " & wafer_id %in% c(", data_bsl$WAFER_ID, ")", collapse = " | ")
      split_conditions["baseline"] <- str_bsl
      
      #create experiment string
      for (i in sort(unique(data_exp$SPLIT_NUM))){
        
        data_exp_lvl <- filter(data_exp, SPLIT_NUM == i)
        data_exp_lvl$WAFER_ID <- sapply(data_exp_lvl$WAFER_ID, FUN = function(x) paste0(as.integer(unlist(strsplit(x, ","))), collapse=','))
        
        str_exp <- paste0('lot_id == "',data_exp_lvl$LOT_ID, '"', " & wafer_id %in% c(", data_exp_lvl$WAFER_ID, ")", collapse = " | ")
        split_conditions[paste0("experiment", i)] <- str_exp
        
      }
      
      split_conditions_list[["split_conditions_1"]] <- split_conditions
      
      #print(split_conditions)
      # return(split_conditions_list)
      
      #if user does not combine splits  
    } else {  
      #create a list of lists to store all the split_conditions 
      
      # print("splitid_selected:")
      # print(splitid_selected)
      # print(seq(splitid_selected))
      
      #loop over each lot
      
      for(lot_seq in seq(split_newlot)){
        
        lot_id <- split_newlot[lot_seq]
        
        data <- filter(new_split_tbl, LOT_ID == lot_id)
        data_bsl <- filter(data, BASELINE == 1)
        data_exp <- filter(data, BASELINE ==0)
        
        #lot_id <- unique(data$LOT_ID)
        #lot_id <- "DE91953.1F"
        
        #baseline wafers
        wafer_list_bsl <- as.integer(unlist(strsplit(data_bsl$WAFER_ID, ",")))
        wafer_list_bsl <- paste0(wafer_list_bsl, collapse=',')
        
        #starts split_conditions with baseline
        split_conditions <- list("baseline" = paste0('lot_id == "',lot_id, '"', " & wafer_id %in% c(", wafer_list_bsl, ")"))
        
        #experimental wafers
        #SPLIT_NUM defines how many levels of experiments there are
        exp_lvls <- data_exp$SPLIT_NUM
        
        #add experiments to split_conditions
        for (i in exp_lvls){
          data_exp_lvl <- filter(data_exp, SPLIT_NUM == i)
          
          wafer_list_exp <- as.integer(unlist(strsplit(data_exp_lvl$WAFER_ID, ",")))
          wafer_list_exp <- paste0(wafer_list_exp, collapse=',')
          
          split_conditions[paste0("experiment", i)] <- paste0('lot_id == "',lot_id, '"', " & wafer_id %in% c(", wafer_list_exp, ")")
          
          # split_conditions = list("baseline" = paste0('LOTID == "',lot_id, '"', " & WAFER_NUM %in% c(", wafer_list_bsl, ")"),
          #                         paste0("experiment", i) = paste0('LOTID == "',lot_id, '"', " & WAFER_NUM %in% c(", wafer_list_exp, ")")
          # )
        }
        
        split_conditions_list[[paste0("split_conditions_", lot_seq)]]<- split_conditions
        
        # #functionality test(comment out later)
        # atmc_split <- read.csv("ATMC13lot_Part_Lot_Insert.csv", stringsAsFactors = F)
        # split_condition_bsl = split_conditions[["baseline"]]
        # split_condition_exp = split_conditions[["experimental"]]
        # dt_bsl <- filter_(atmc_split, split_condition_bsl)
        # dt_exp <- filter_(atmc_split, split_condition_exp)
      }
      
      #print(split_conditions_list)
      
      # return(split_conditions_list)
      
    }
    
    split_conditions_list_print <<- split_conditions_list
    
    return(split_conditions_list)
    
    
  })
  
  
  output$splitStr_shiny <- renderPrint({
    
    print("split_conditions_list:")
    split_conditions_list_shiny()
    
  })
  
  
  
  
  

})
