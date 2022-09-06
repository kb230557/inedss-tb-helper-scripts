


#========== Script set-up ==========

#load libraries

library(tidyverse)
library(RSelenium)

#load helper functions
devtools::source_url("https://github.com/Cook-County-Department-of-Public-Health/ccdph-functions/blob/master/general-use-rselenium-functions.R?raw=TRUE")
devtools::source_url("https://github.com/Cook-County-Department-of-Public-Health/ccdph-functions/blob/master/inedss-rselenium-functions.R?raw=TRUE")

#key_set("idph_username") #set your IDPH web portal username -- only needs to be done once per computer
#key_set("idph_portal")  #set your IDPH web portal password -- only needs to be done once per computer

end_date <- as.Date("2018-01-01")

#========== Prepare error file ==========

#generate dataframe
errors <- data.frame(StateCaseNumber = character(), 
                     Name = character(),
                     DOB = as.Date(character()),
                     EventDate = as.Date(character()), 
                     CaseStatus = as.Date(character()), 
                     Reason = character()
)  

#store dataframe
error_path <- paste0('tb-exceptions/', Sys.Date(), "_tb-exceptions.csv") 
write_csv(errors, error_path)



#========== Log into I-NEDSS ==========

#open selenium session
start_server()

#log in to INEDSS
login_inedss()

#id tb row (css sometimes changes)
tb_row <- find_child_element("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2)", "tbody > tr", "TB Section")

#open tb section
click("divTB", selectorType = "id")

#select tb disease to get to all open TB disease cases
click(paste0("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", tb_row + 1, ") > td:nth-child(1) > a:nth-child(1)"))



#========== Initialize counters ==========

#Initializing n_child to start with first case 
tr_n_child_Val <- 8

#Initializing counters
totalClosed <- 0
totalLeftOpen <- 0



#========== Loop to close cases ==========

repeat {
  
  #Increment n_child until case encountered 
  nextCase <- try(rD$findElement(using = "css", value = paste0("table.indessTable:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", tr_n_child_Val, ") > td:nth-child(2) > a:nth-child(1)")))
  
  while(class(nextCase) == "try-error" & tr_n_child_Val < 301) {
    
    tr_n_child_Val <- tr_n_child_Val + 1
    nextCase <- try(rD$findElement(using = "css", value = paste0("table.indessTable:nth-child(2) > tbody:nth-child(1) > tr:nth-child(", tr_n_child_Val, ") > td:nth-child(2) > a:nth-child(1)")))
    
  } #while closure
  
  #If not encountered, send error and stop script
  if (tr_n_child_Val >= 301) {
    
    stop("No case to process")  
    
  } #stop script closure
  
  #Otherwise, click into nextCase
  nextCase$clickElement()
  
  #Give case page time to load
  wait_page("Case Summary")
  
  #Store case info in case needed for error tracking
  name <- get_text(".fieldsetNameBlock > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2)") %>% trimws()
  dob <- get_text(".fieldsetNameBlock > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(5)") %>% lubridate::mdy()
  eventDate <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2)") %>%
    lubridate::mdy()
  stateCaseNumber <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(1)")
  case_status <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(3)")
  
  #Store info to assess if case meets closure criteria
  disease <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1)")
  disposition <- get_text("#container > div:nth-child(4) > form:nth-child(4) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(2)")
  
  #if case is old and disposition is completed, attempt to close; else, exit out
  if(disease == "TB Disease" & disposition == "Completed" & !is.na(eventDate) & eventDate < end_date) {
    
    #click complete investigation
    ifVisiblethenClick("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > a:nth-child(1)")
    wait_page("Complete Investigation")
    
    #Check for final case validation issues
    caseValidation <- get_text("#completion")
    
    #If no errors, close case; else exit out and store in errors list
    if (grepl("no errors were found", caseValidation)) {
      
      #Add script closure comment to log
      enter_text("#comment", "Administratively closed.")
      
      #Click send to IDPH
      click(name.is("save"))
      
      #Accept alert
      acceptAlertwithWait()
      
      #Wait and save page title for if/else below
      Sys.sleep(4)
      page_check <- current_page()
      
      #Some errors will pop up only after attempting to close the case
      #Check if close was successfully closed, if no, save error and exit, if yes increment counter
      if (page_check == "Complete Investigation") {
        
        #save errors, back out, and iterate counters (note: considering putting this in a fx but modifying a lot of global env vars)
        
        #re-check for final case validation issues
        caseValidation <- get_text("#completion")
        
        #write invalid conditions to file
        caseResults <- data.frame(case = stateCaseNumber,
                                  name = name,
                                  dob = dob,
                                  date = eventDate, 
                                  case_status = case_status, 
                                  reason = caseValidation, 
                                  stringsAsFactors = FALSE) 
        write_csv(caseResults, error_path, append = T)
        
        #exit from complete case
        click(name.is("cancel"))
        
        #Give page time to load
        wait_page("Case Summary")
        
        #Increment cases worked counter
        totalLeftOpen <- totalLeftOpen + 1
        
        #close out to main page
        ifVisiblethenClick(name.is("cancel"))
        
        #Determine next row to work
        tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
        
      } else {  #if/else surprise error
      
        #Increment cases worked counter
        totalClosed <- totalClosed + 1
        
      } #if/else surprise error closure
      
    } else { #if/else close or validation errors
      
      #save errors, back out, and iterate counters
      #write invalid conditions to file
      caseResults <- data.frame(case = stateCaseNumber,
                                name = name,
                                dob = dob,
                                date = eventDate, 
                                case_status = case_status, 
                                reason = caseValidation, 
                                stringsAsFactors = FALSE) 
      write_csv(caseResults, error_path, append = T)
      
      #exit from complete case
      click(name.is("cancel"))
      
      #Give page time to load
      wait_page("Case Summary")
      
      #Increment cases worked counter
      totalLeftOpen <- totalLeftOpen + 1
      
      #close out to main page
      ifVisiblethenClick(name.is("cancel"))
      
      #Determine next row to work
      tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
      
    } #if/else close or validation errors closure
     
  } else {  #if/else to close or not
    
    click(name.is("cancel"))
    
    #Increment cases worked counter
    totalLeftOpen <- totalLeftOpen + 1
    
    #Determine next row to work
    tr_n_child_Val <- ifelse(totalLeftOpen %% 25 == 0, 8, tr_n_child_Val + 1)
    
    
  } #if/else to close or not closure
  
  #Give main page time to load
  wait_page("My Cases")
  
  #Determining page to work
  pageCount <- floor(totalLeftOpen / 25) + 1
  
  if (pageCount > 0) {
    
    #Click to go page being worked
    ifVisiblethenClick(paste0("td.position > select:nth-child(2) > option:nth-child(", pageCount,")"))
    
    #Give page time to load
    isPageLoaded(".pageDesc")
    
  }
  
}  #repeat closure



#========== Stop Selenium ==========

stop_server(need_java_kill = T)

