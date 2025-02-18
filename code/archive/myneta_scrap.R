
source("code/functions.R")

# THIS FILE CONTAINS FUNCTION TO SCRAP
# AFFEDEVITE DATA FROM MYNETA WEBSITE

# WILL USE THE FOLLOWING: 
# ADR CANDIDATE ID LIST 
# THIS LIST WILL BE PASSED ON TO A FUNCTION THAT SCRAPS

# DEFINING CSS EXTRACTOR

getcss=function(css_path, webpage){
  
  webpage %>% 
    html_element(css = css_path) %>% 
    html_text() %>% 
    str_trim()
  
}


# new function using httr2

extract_table <- function( css_selector, rsp) {
  
  
  # # Perform a GET request to the URL
  # response <- request(url) |> 
  #   req_perform()
  
  # Check if the request was successful
  if (rsp$status_code == 200) {
    # Parse the content as HTML
    content <- rsp |> 
      resp_body_html(encoding = "UTF-8")
    
    # Use rvest to extract the table with the specified CSS selector
    table <- content |> 
      html_element(css = css_selector) |> 
      html_table()
    
    return(table)
  } else {
    stop("Failed to retrieve data: HTTP status code ", response$status_code)
  }
}




safe_extract_table <- function(css_selector, webpage) {
  
  node <- rvest::html_element(webpage, css = css_selector)
  
  if ((length(node)>0)) {
    return(html_table(node, fill = TRUE))
  } else {
    warning(paste("The node", css_selector, "does not exist in this webpage."))
    return(data.frame())  # Return an empty data frame if the node does not exist
  }
}





table_scrap=function(url){
  
      # url1 <- "https://myneta.info/uttarpradesh2022/candidate.php?candidate_id=2908"
    # url2="https://myneta.info/punjab2022/candidate.php?candidate_id=742"
    # url= "https://myneta.info/punjab2022/candidate.php?candidate_id=293" 
    # Parse HTML
    webpage <- read_html(url)
    
    ima_table=webpage %>%
      html_node('#immovable_assets') %>%
      html_table()
    
    liab_table=webpage %>%
      html_node('#liabilities') %>%
      html_table()
    #profession
    prof_table=webpage %>%
      html_node('#profession') %>%
      html_table()
    
    incsource_table=webpage %>%
      html_node('#incomesource') %>%
      html_table()
    
    contract_table=webpage %>%
      html_node('#contractdetails') %>%
      html_table()
    
    pending_cases=webpage %>%
      html_node('#cases') %>%
      html_table()
    
    return(list(ima_table, 
                liab_table, 
                prof_table,
                incsource_table,
                contract_table,
                pending_cases))
    
    
}

myneta_scrap=function(url){

    # url1 <- "https://myneta.info/uttarpradesh2022/candidate.php?candidate_id=2908"
    # url2="https://myneta.info/punjab2022/candidate.php?candidate_id=742"
  # url= "https://myneta.info/punjab2022/candidate.php?candidate_id=293" 
    # Parse HTML
    webpage <- read_html(url)
    
   
    tables <- html_nodes(webpage, "table")
    data <- lapply(tables, html_table)
    
    # Extracted elements
    
    ## BASIC INFO
    state_year=webpage %>% 
      getcss(css_path =".w3-khaki , .w3-khaki h3" )
      
    
    candidate_name=webpage %>% 
     getcss(css_path  = "div[class='w3-panel'] h2") 
    
    
    const_name=webpage %>% 
      getcss(css_path = "div[class='w3-panel'] h5") 
    
    
    party=webpage %>% 
      getcss(css_path="body > div:nth-child(2) > div:nth-child(4) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3)")
    
    
    guardian=webpage %>% 
      getcss(css_path = "body > div:nth-child(2) > div:nth-child(4) > div:nth-child(1) > div:nth-child(1) > div:nth-child(4)")
    
    
    age=webpage %>% 
      getcss(css_path = ".w3-panel div:nth-child(5)")
    
    voter_list=webpage %>% 
      getcss(css_path = ".w3-panel div:nth-child(6)")
    
    # PROFESSION
    
    profession=webpage %>% 
      getcss(css_path="body > div:nth-child(2) > div:nth-child(4) > div:nth-child(1) > div:nth-child(1) > p:nth-child(7)")
    
    # INCOME AND PAN
    self_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(2) td:nth-child(4)")
    
    self_income_source=webpage %>% 
      getcss(css_path = "#incomesource .w3-bordered tr:nth-child(1) b")
    
    
    self_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(2) td:nth-child(2)")
    
    
    spouse_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(3) td:nth-child(2)")
    spouse_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(3) td:nth-child(4)")
    spouse_income_source=webpage %>% 
      getcss(css_path = "#incomesource tr:nth-child(2) b")
    
    dep_income_source=webpage %>% 
      getcss(css_path = "#incomesource tr:nth-child(3) b")
    huf_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(4) td:nth-child(2)")
    huf_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(4) td:nth-child(4)")
    dep1_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(5) td:nth-child(2)")
    dep1_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(5) td:nth-child(4)")
    dep2_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(6) td:nth-child(2)")
    dep2_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(6) td:nth-child(4)")
    dep3_pan_yn=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(7) td:nth-child(2)")
    dep3_income=webpage %>% 
      getcss(css_path = "#income_tax tr:nth-child(7) td:nth-child(4)")
    
    # CRIME AND CHARGES
    serious_crime=webpage %>% 
      getcss(css_path="ul:nth-child(1)")
    
    crime=webpage %>% 
      getcss(css_path = "ul:nth-child(2)")
    
    
    # MOVABLE ASSETS
    # includes assets other than land
    ## INCOMPLETE
    
    ma_cash_self=webpage %>% 
      getcss(css_path = "#movable_assets tr:nth-child(2) td:nth-child(3)")
    ma_cash_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(2) td:nth-child(4)")
    ma_cash_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(2) td:nth-child(5)")
    ma_cash_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(2) td:nth-child(6)")
    ma_cash_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(2) td:nth-child(7)")
    ma_cash_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(2) td:nth-child(8)")
    
    
    ma_bankdps_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(3)")
    ma_bankdps_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(4)")
    ma_bankdps_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(5)")
    ma_bankdps_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(6)")
    ma_bankdps_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(7)")
    ma_bankdps_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(3) td:nth-child(8)")
    
    
    
    ma_bond_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(3)")
    ma_bond_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(4)")
    ma_bond_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(5)")
    ma_bond_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(6)")
    ma_bond_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(7)")
    ma_bond_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(4) td:nth-child(8)")
    
      
    
    
    ma_postal_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(3)")
    ma_postal_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(4)")
    ma_postal_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(5)")
    ma_postal_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(6)")
    ma_postal_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(7)")
    ma_postal_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(5) td:nth-child(8)")
    
    
    ma_lic_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(2)")
    ma_lic_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(3)")
    ma_lic_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(4)")
    ma_lic_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(5)")
    ma_lic_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(6)")
    ma_lic_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(6) td:nth-child(7)")
    
    
    ma_adv_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(3)")
    ma_adv_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(4)")
    ma_adv_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(5)")
    ma_adv_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(6)")
    ma_adv_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(7)")
    ma_adv_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(7) td:nth-child(8)")
    
    
    
    
    ma_mv_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(3)")
    ma_mv_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(4)")
    ma_mv_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(5)")
    ma_mv_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(6)")
    ma_mv_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(7)")
    ma_mv_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(8) td:nth-child(8)")
    
    
    ma_jwl_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(3)")
    ma_jwl_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(4)")
    ma_jwl_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(5)")
    ma_jwl_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(6)")
    ma_jwl_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(7)")
    ma_jwl_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(9) td:nth-child(8)")
    
    
    ma_other_self=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(3)")
    ma_other_spouse=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(4)")
    ma_other_huf=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(5)")
    ma_other_dep1=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(6)")
    ma_other_dep2=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(7)")
    ma_other_dep3=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(10) td:nth-child(8)")
    
    
    ma_total=webpage %>% 
      getcss(css_path="#movable_assets tr:nth-child(12) td:nth-child(8) b")
    
    
    #movable_assets tr:nth-child(12) td:nth-child(8) b
    #tr:nth-child(10) td:nth-child(7) b
    #tr:nth-child(10) td:nth-child(7) b

    
        
    # IMMOVABLE ASSETS
    # includes land
    # INCOMPLETE ( only total count for now)
    
    ima_total=webpage %>% 
      getcss(css_path = "#immovable_assets tr:nth-child(8) td:nth-child(8) b")
    
    
    # LIABILITIES 
    # loans, etc 
    # INCOMPLETE ( only total for now)
    
    lbl_total=webpage %>% 
      getcss(css_path = "tr:nth-child(20) td:nth-child(8) b")
    
    
    # GOVT CONTRACT DETAILS
    
    self_contract=webpage %>% 
      getcss(css_path = "#contractdetails .w3-bordered tr:nth-child(1) b")
    
    
    spouse_contract=webpage %>% 
      getcss(css_path = "#contractdetails tr:nth-child(2) b")
    
    dep_contract=webpage %>% 
      getcss(css_path = "#contractdetails tr:nth-child(3) b")
    
    family_contract=webpage %>% 
      getcss(css_path = "tr:nth-child(4) td:nth-child(2) b")
    
    partner_contract=webpage %>% 
      getcss(css_path = "#contractdetails tr:nth-child(5) td:nth-child(2) b")
    
    firm_contract=webpage %>% 
      getcss(css_path = "tr:nth-child(6) td:nth-child(2) b")
    
   

  df=as.data.frame(cbind(state_year, candidate_name, const_name, 
                         party,guardian, age, voter_list, profession, 
                         self_income, self_income_source,
                         self_pan_yn, spouse_pan_yn, spouse_income, 
                         spouse_income_source,
                         dep_income_source, huf_pan_yn, huf_income, 
                         dep1_pan_yn,
                         dep1_income, dep2_pan_yn, dep2_income, dep3_pan_yn, 
                         dep3_income,
                         serious_crime, crime, 
                         ma_cash_self, ma_cash_spouse, ma_cash_huf, ma_cash_dep1, ma_cash_dep2, ma_cash_dep3,
                         ma_bankdps_self,ma_bankdps_spouse, ma_bankdps_huf, ma_bankdps_dep1, ma_bankdps_dep2, ma_bankdps_dep3,
                         ma_bond_self,ma_bond_spouse, ma_bond_huf, ma_bond_dep1, ma_bond_dep2, ma_bond_dep3,
                         ma_postal_self,ma_postal_spouse, ma_postal_huf, ma_postal_dep1, ma_postal_dep2, ma_postal_dep3,
                         ma_lic_self,ma_lic_spouse, ma_lic_huf, ma_lic_dep1, ma_lic_dep2, ma_lic_dep3,
                         ma_adv_self,ma_adv_spouse, ma_adv_huf, ma_adv_dep1, ma_adv_dep2, ma_adv_dep3,
                         ma_mv_self,ma_mv_spouse, ma_mv_huf, ma_mv_dep1, ma_mv_dep2, ma_mv_dep3,
                         ma_jwl_self,ma_jwl_spouse, ma_jwl_huf, ma_jwl_dep1, ma_jwl_dep2, ma_jwl_dep3,
                         ma_other_self,ma_other_spouse, ma_other_huf, ma_other_dep1, ma_other_dep2, ma_other_dep3,
                         ma_total, ima_total, lbl_total,
                         self_contract,
                         spouse_contract, 
                         dep_contract, 
                         family_contract, 
                         partner_contract, firm_contract))
  
  
  return(df)
     
}







  

