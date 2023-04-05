library(tidyverse);library(XML);library(RCurl);
evds_data2 <- function(anahtar,
                   verisetleri,
                   baslangic_tarihi, bitis_tarihi, frekans = "") {

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

