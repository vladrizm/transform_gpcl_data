# #Run with Proxy ----
# 
# if (!require("devtools")) install.packages("devtools")
# if (!require("getPass")) install.packages("getPass")
# library(getPass)
# library(httr)
# httr::set_config(httr::use_proxy(url=Sys.getenv("https_proxy"), port=80,
#                                  username=Sys.getenv("USERNAME"),
#                                  password=getPass(),
#                                  auth="gssnegotiate"))
# 
# Sys.getenv("https_proxy")


library(rvest)
### "https://content3.gpcl.com.au/viewcontent/CargoComparisonsSelection/CargoOriginDestination.aspx?View=C&Durat=C&Key=2022"

### Create website links per month data----
html <- read_html("https://content3.gpcl.com.au/viewcontent/CargoComparisonsSelection/CargoOriginDestination.aspx?View=C&Durat=M&Key=202207")

#Update table according year and month----
table_22_7 <- html %>%
  html_node("table") %>%
  html_table()

### Create website links per month data
# links_01 <- paste0("https://content3.gpcl.com.au/viewcontent/CargoComparisonsSelection/CargoOriginDestination.aspx?View=C&Durat=M&Key=20150", 1:9)
# links_01 <- append(links_01, paste0("https://content3.gpcl.com.au/viewcontent/CargoComparisonsSelection/CargoOriginDestination.aspx?View=C&Durat=M&Key=2015", 10:12))
# links_01
# 
# ### Loop through website links per month data and load table data into data frames
# year_index = "15_"
# for(i in 1:12) {
#   table_01 <- paste0("table_", year_index,i)
#   html_01 <- read_html(links_01[i])
#   table_02 <-html_01 %>% 
#     html_node("table") %>% 
#     html_table()
#   assign(table_01, table_02)
# }

## Function to clean and merge data

# table_15_1a <- table_15_1

# ### Add additional column with copied rows "Total" to stop commodity fill
# table_15_1 <- table_15_1 %>% tibble::add_column(X6 = NA)
# table_15_1[which(table_15_1[,3] == "Total"),6] <- "Total"
# 
# ## Find all commodities add fill down direction
# commo <- which(table_15_1[,2] == "" & table_15_1[,3] == "" & table_15_1[,4] == "" & table_15_1[,5] == "")
# table_15_1[commo,6] <- table_15_1[commo,1]
# 
# library(zoo)
# 
# ### Extend commodities instead NA
# table_15_1[,6] <- zoo::na.locf(table_15_1[,6])
# 
# ### Add date column
# 
# table_15_1$X7 <- as.Date("01/01/2015", "%d/%m/%Y")
# 
# ## Split table into Export and Import
# cut_table_01 <- which(table_15_1[,1] == "Import Origins")
# cut_table_02 <- which(table_15_1[,1] == "Totals by Country")
# assign(paste0("e_","table_15_1"), table_15_1[3:(cut_table_02[1]-1),])
# assign(paste0("i_","table_15_1"), table_15_1[(cut_table_01+3):cut_table_02[2]-1,])
# 
# z_01 <- get(paste0("e_","table_15_1"))
# z_01$X8 <- "Export"
# 
# z_02 <- get(paste0("i_","table_15_1"))
# z_02$X8 <- "Import"
# 
# ## Combine Export and Import together
# z_03 <- rbind(z_01,z_02)
# 
# w_01 <- z_03[,c(1,2,5,6,7,8)]
# w_02 <- z_03[,c(3,4,5,6,7,8)]
# 
# ## Generic columns names to rbind 
# names(w_01) <- paste0("X",1:7)
# names(w_02) <- paste0("X",1:7)
# 
# w_03 <- rbind(w_01,w_02)
# 
# ### Shift Vessel Count one row up
# 
# shift_column <- function(x, n){
#   c(x[-(seq(n))], rep("", n))
# }
# 
# w_03$X7 <- shift_column(w_03$X3, 1)
# 
# ### Shift Vessel tidy up table
# m_01 <- w_03[,c(7,4,5,6)]
# m_01 <- m_01[m_01$X7 != "",]
# m_001 <- replace(m_01$X7, m_01$X7 == "*","0")
# m_01$X7<- as.numeric(m_001)
# m_01 <- m_01[,c(1,2,4,3)]
# names(m_01) <- c("Vessel Count", "Commodity", "Export_Import", "Date" )
# 
# ### Commodity table
# n_01 <- w_03[,c(1,2,4,6,5)]
# n_01 <- n_01[n_01$X2 != "",]
# n_01 <- n_01[n_01$X1 != "Total",]
# n_01[,2] <- lapply(n_01[,2],function(x){as.numeric(gsub(",", "", x))})
# names(n_01) <- c("Country", "Tonnes", "Commodity", "Export_Import", "Date" )
library(tibble)

clean_month_data <- function(df, date_n){
  ### Add additional column with copied rows "Total" to stop commodity fill
  df <- df %>% tibble::add_column(X6 = NA)
  df[which(df[,3] == "Total"),6] <- "Total"
  
  ## Find all commodities add fill down direction
  commo <- which(df[,2] == "" & df[,3] == "" & df[,4] == "" & df[,5] == "")
  df[commo,6] <- df[commo,1]
  
  library(zoo)
  
  ### Extend commodities instead NA
  df[,6] <- zoo::na.locf(df[,6])
  
  ### Add date column
  
  df$X7 <- as.Date(date_n, "%d/%m/%Y")
  
  ## Split table into Export and Import
  cut_table_01 <- which(df[,1] == "Import Origins")
  cut_table_02 <- which(df[,1] == "Totals by Country")
  assign(paste0("e_","df"), df[3:(cut_table_02[1]-1),])
  assign(paste0("i_","df"), df[(cut_table_01+3):cut_table_02[2]-1,])
  
  z_01 <- get(paste0("e_","df"))
  z_01$X8 <- "Export"
  
  z_02 <- get(paste0("i_","df"))
  z_02$X8 <- "Import"
  
  ## Combine Export and Import together
  z_03 <- rbind(z_01,z_02)
  
  w_01 <- z_03[,c(1,2,5,6,7,8)]
  w_02 <- z_03[,c(3,4,5,6,7,8)]
  
  ## Generic columns names to rbind 
  names(w_01) <- paste0("X",1:7)
  names(w_02) <- paste0("X",1:7)
  
  w_03 <- rbind(w_01,w_02)
  
  ### Shift Vessel Count one row up
  
  shift_column <- function(x, n){
    c(x[-(seq(n))], rep("", n))
  }
  
  w_03$X7 <- shift_column(w_03$X3, 1)
  
  ### Shift Vessel tidy up table
  m_01 <- w_03[,c(7,4,5,6)]
  m_01 <- m_01[m_01$X7 != "",]
  m_001 <- replace(m_01$X7, m_01$X7 == "*","0")
  m_01$X7<- as.numeric(m_001)
  m_01 <- m_01[,c(1,2,4,3)]
  names(m_01) <- c("Vessel Count", "Commodity", "Export_Import", "Date" )
  
  ### Commodity table
  n_01 <- w_03[,c(1,2,4,6,5)]
  n_01 <- n_01[n_01$X2 != "",]
  n_01 <- n_01[n_01$X1 != "Total",]
  n_01[,2] <- lapply(n_01[,2],function(x){as.numeric(gsub(",", "", x))})
  names(n_01) <- c("Country", "Tonnes", "Commodity", "Export_Import", "Date" )
  
  list_results <- list(n_01, m_01)
  return(list_results)
}

c_22_7 <- clean_month_data(table_22_7, "01/07/2012")

openxlsx::write.xlsx(c_22_7, file = "Port_data_July_2022.xlsx")


# 
# ### c_15_1, c_15_2, c_15_3, c_15_4, c_15_5, c_15_6, c_15_7, c_15_8, c_15_9, c_15_10, c_15_11, c_15_12
# big_list <- purrr::map2(c_15_1, c_15_2, rbind)
# big_list <- purrr::map2(big_list, c_15_12, rbind)



### Update year!!! ----
year_date <- c("01/01/2022", "01/02/2022", "01/03/2022", "01/04/2022", "01/05/2022", "01/06/2022", "01/07/2022", "01/08/2022", "01/09/2022", "01/10/2022", "01/11/2022", "01/12/2022")

# big_list <- list()

### Update year!!! ----
for(i in 1:6) {
  clean_01 <- paste0("c_15_",i)
  data_01 <- get(paste0("table_22_",i)) ### Change Year!!!
  # date_02 <- year_date[i]
  assign(clean_01, clean_month_data(data_01, year_date[i]))
}

big_list <- purrr::map2(c_15_1, c_15_2, rbind)
for(i in 3:6) {
  clean_01 <- paste0("c_15_",i)
  big_list <- purrr::map2(big_list, get(clean_01), rbind)
}

names(big_list) <- c("Commodity", "Vessel_Count")
library(openxlsx)
openxlsx::write.xlsx(big_list, file = "Port_data_2022.xlsx")

big_list_22 <- big_list

big_list_15_22 <- purrr::map2(big_list_15_22, big_list_22, rbind)
openxlsx::write.xlsx(big_list_15_22, file = "Port_data_2015_2022.xlsx")
