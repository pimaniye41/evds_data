evds_data <- function(anahtar,
                      verisetleri,
                      baslangic_tarihi, bitis_tarihi,
                      islem = "", formul = "", frekans = "") {
  adres <- "https://evds2.tcmb.gov.tr/service/evds/"
  tarihler <- paste("&startDate=",baslangic_tarihi,"&endDate=",bitis_tarihi, sep="")
  tamamlayici <- paste("&type=csv&key=",anahtar,sep="")

  seri_isimler <- paste(verisetleri, collapse = "-")
  seri <- paste("series=",seri_isimler, sep="")
  gozlem <- paste("&aggregationTypes=", islem, sep = "")
  formula <- paste("&formulas=", formul, sep = "")
  frek <- paste("&frequency=", frekans, sep = "")

  veriadresi <-paste(adres, seri, tarihler, tamamlayici, gozlem, formula, frek, sep="")
  csvveri <- getURL(veriadresi, .opts = list(ssl.verifypeer = FALSE))
  veridf = read_csv(csvveri)

  veridf <- veridf %>% select(!UNIXTIME)
  veridf <- if("YEARWEEK" %in% colnames(veridf)){
    veridf <- pivot_longer(data = veridf,
                           cols = c(3:ncol(veridf)),
                           names_to = "seri",
                           values_to = "deger")}
  else {pivot_longer(data = veridf,
                     cols = c(2:ncol(veridf)),
                     names_to = "seri",
                     values_to = "deger")
  }
  veridf$deger <- as.numeric(veridf$deger)

  veridf$Tarih <- if(nchar(veridf$Tarih[1])<10){
    as.Date(paste0(veridf$Tarih, "-01"), format = "%Y-%m-%d")
  }
  else{
    as.Date(veridf$Tarih,format = "%d-%m-%Y")
  }
  veridf <- arrange(veridf,Tarih)
  veridf <- veridf %>% pivot_wider(id_cols = Tarih,
                                   values_from = deger,
                                   names_from = seri)
  return(veridf)
}
