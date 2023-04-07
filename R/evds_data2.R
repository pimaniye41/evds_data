#' Importing data from CBRT EVDS
#' Newer version
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

  # changing characterized dates with numbers
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q1","03-31")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q2","06-30")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q3","09-30")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "Q4","12-31")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "S1","06-30")
  veridf$Tarih <- str_replace_all(string = veridf$Tarih, "S2","12-31")

  # monthly data (2022-12 or 2022-1) to (2022-1-01)
  veridf$Tarih <- ifelse(nchar(veridf$Tarih) == 6 |nchar(veridf$Tarih) == 7 ,
                         paste(veridf$Tarih,"-01",sep = ""),
                               veridf$Tarih)

  # yearly data (2023) to (2023-12-31)
  veridf$Tarih <- ifelse(nchar(veridf$Tarih) == 4,
                         paste(veridf$Tarih,"-12-31",sep = ""),
                         veridf$Tarih)

  # changing character date column to date str
  veridf$Tarih <- as.Date.character(veridf$Tarih,
                                    tryFormats = c("%d-%m-%y",
                                                   "%Y-%m-%d"))

  # choosing columns other than date to convert to numeric
  cols_to_convert <- names(veridf)[-1]

  # use mutate_at() to convert selected columns to numeric
  veridf <- veridf %>%
    mutate_at((cols_to_convert), as.numeric)

  return(veridf)
}
