#' Importing data from CBRT EVDS
#'
#' @param anahtar Your API key from EVDS, sign up to EVDS for API key.    EVDS API anahatarınız.
#' @param verisetleri Series you want to import.    Cekmek istediginiz seriler
#' @param baslangic_tarihi Start date you want.    Veri baslangic tarihi
#' @param bitis_tarihi End date for the data you want.    Veri son tarihi
#' @param frekans Frequency parameter for the data. Default is set automatically by EVDS. 1 to 8, frequent to infrequent.    Veri frekansı, bos birakildiginda varsayilan frekans ile gelir. 1-8 arasi deger
#'
#' @return
#' @export
#'
#' @examples evds_data2(anahtar = "yourkey", verisetleri = c("TP.PR.ARZ01","TP.KTF10"), baslangic_tarihi = "01-01-2020", bitis_tarihi = "01-03-2023")
#' @import RCurl
#' @import dplyr
#' @import stringr
#' @import XML

evds_data2 <- function(anahtar,
                   verisetleri,
                   baslangic_tarihi, bitis_tarihi, frekans = "") {
  library(readr)
  library(RCurl)
  library(dplyr)
  library(stringr)
  library(XML)
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")

  seri_isimler <- paste(verisetleri, collapse = "-")
  seri <- paste("series=",seri_isimler, sep="")
  frek <- paste("&frequency=", frekans, sep = "")
  gozlem <- "&aggregationTypes="
  formula <- "&formulas="

  veriadresi <-paste(adres, seri, tarihler, tamamlayici, gozlem, formula, frek, sep="")
  csvveri <- getURL(veriadresi, .opts = list(ssl.verifypeer = FALSE))
  veridf = read_csv(csvveri)
  veridf <- veridf %>% select(!UNIXTIME)
  veridf <- if("YEARWEEK" %in% colnames(veridf)) {
    veridf %>% select(!YEARWEEK)
  } else {
    veridf
  }
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q1","03")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q2","06")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q3","09")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q4","12")

  veridf$Tarih <- if(nchar(veridf$Tarih[1])<10){
    as.Date(paste0(veridf$Tarih, "-01"), format = "%Y-%m-%d")

  }
  else{
    as.Date(veridf$Tarih,format = "%d-%m-%Y")
  }
  return(veridf)
}
?evds_data2
