# require(RCurl)
# index <- readLines("http://www.vcgr.vic.gov.au/CA2570C30016EEF3/VenueMonthlyExpenditure?OpenView&Start=1&Count=1000")
require(stringr)
venue.index <- readLines("http://www.vcgr.vic.gov.au/CA2570C30016EEF3/VenueMonthlyExpenditure?OpenView&Start=1&Count=1000")

venue.urls <- paste("http://www.vcgr.vic.gov.au", unlist(str_extract_all(venue.index, pattern="(\\/Website\\/maps.nsf\\/[A-z0-9]*/[A-z0-9]*\\?OpenDocument)")), sep="")

results <- data.frame()
errors.results <- data.frame()
for (i in seq_along(venue.urls)){
  venue.details <- NA
  try(venue.details <- readLines(venue.urls[i]))
  if (any(is.na(venue.details))){
    errors.results <- rbind(errors.results, data.frame(index=i,
                                                       type="download error",
                                                       name="",
                                                       url=venue.urls[i]))
    next
  }
  venue.details <- paste(venue.details, collapse=" ")
  venue.details <- gsub('"', '', venue.details)
  venue.name <- str_match_all(venue.details, "<title>([^<]+)<\\/title>")[[1]][2]
  venue.expenditure <- NA
  try(venue.expenditure <- str_match_all(venue.details, "(Full year|Full Year) *[<\\/=>A-z0-9% ]+([\\$0-9,.]+) *<br> *([\\$0-9,.]+) *<br> *([\\$0-9,.]+)[<\\/=>A-z0-9% ]+([\\$0-9,.]+) *<br> *([\\$0-9,.]+) *<br> *([\\$0-9,.]+)")[[1]][1,3:8])
  if (any(is.na(venue.details))){
    errors.results <- rbind(errors.results, data.frame(index=i,
                                                       type="expenditure error",
                                                       name=venue.name,
                                                       url=venue.urls[i]))
    next
  }
  
  venue.expenditure <- gsub("^\\$", "", venue.expenditure)
  venue.expenditure <- as.numeric(gsub(",", "", venue.expenditure))
  venue.address <- NA
  try(venue.address <- paste(str_match_all(venue.details, "Address: *(<[\\/ A-z0-9%#=]+> *)+([^<]+)(<[\\/ A-z0-9%#=]+> *)+([^<]+)(<[\\/ A-z0-9%#=]+> *)+([^<]+)")[[1]][1,c(3,5,7)], collapse=" "))
  if (any(is.na(venue.address))){
    errors.results <- rbind(errors.results, data.frame(index=i,
                                                       type="address error",
                                                       name=venue.name,
                                                       url=venue.urls[i]))
    next
  }
  venue.type <- str_match_all(venue.details, "Type: *(<[\\/ A-z0-9#%=]+> *)+([^<]+)")[[1]][1,3]
  venue.vo <- str_match_all(venue.details, "VO Licence: *(<[\\/ A-z0-9%=#\\']+> *)*([A-z0-9]+)")[[1]][1,3]
  venue.region <- str_match_all(venue.details, "Region: *(<[\\/ A-z0-9%=#\\']+> *)*([A-z0-9]+)")[[1]][1,3]
  venue.entitlements <- str_match_all(venue.details, "Attached Entitlements: *(<[\\/ A-z0-9%=#\\']+> *)*([A-z0-9]+)")[[1]][1,3]
  venue.egms <- str_match_all(venue.details, "Licensed EGMs: *(<[\\/ A-z0-9%=#\\']+> *)*([A-z0-9]+)")[[1]][1,3]
  
  venue.df <- data.frame(index=i,
                         name=venue.name, 
                         LossJulDec12=venue.expenditure[1],
                         LossJanJun13=venue.expenditure[2],
                         LossJul12Jun13=venue.expenditure[3],
                         LossJulDec11=venue.expenditure[4],
                         LossJanJun12=venue.expenditure[5],
                         LossJul11Jun12=venue.expenditure[6],
                         address=venue.address,
                         type=venue.type,
                         vo=venue.vo,
                         region=venue.region,
                         attached_entitlements=venue.entitlements,
                         egms=venue.egms)
  results <- rbind(results, venue.df)
  Sys.sleep(0.5)
}
write.csv(results, paste(strftime(Sys.time(), "%Y-%m-%d"), "VIC EGMS.csv"), row.names=FALSE)
write.csv(errors.results, paste(strftime(Sys.time(), "%Y-%m-%d"), "VIC EGMS errors.csv"), row.names=FALSE)

results$attached_entitlements <- as.integer(results$attached_entitlements)
results$egms <- as.integer(results$egms)