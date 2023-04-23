rm(list = ls())


library(rvest,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(httr,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(rvest,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")

library(RSelenium,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(RCurl,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(stringr,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(rJava,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(data.table,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(tidyr,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")
library(dplyr,lib.loc="C:/Users/praveen/Documents/R/win-library/4.0")


m<-as.integer(Sys.Date())


# rselineam #########
# rD$server$stop()

#rD <- rsDriver(browser="firefox",verbose = FALSE,port=m)

rD <- RSelenium::rsDriver(
  browser = "firefox",
  port = m,
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list('--headless')
    )
  )
)


remDr <- rD$client

remDr$open(silent = T)

x<-1

slist<-list()


for (i in 1:25){
  remDr$navigate(paste0("https://ikman.lk/en/ads/sri-lanka/cars?sort=date&order=desc&buy_now=0&urgent=0&page=",i))
  
  back<-paste0("https://ikman.lk/en/ads/sri-lanka/cars?sort=date&order=desc&buy_now=0&urgent=0&page=",i)
  
  print(i)
  
  for(k in 3:25){
    remDr$findElement("css", paste0(".gtm-normal-ad:nth-child(",k,")") )$clickElement()
    
    url2<-remDr$getCurrentUrl()[[1]]
    
    webpage <- read_html(url2)
    rank_data_html <- html_nodes(webpage,'.label--3oVZK')
    text <- html_text(rank_data_html)
    
    text2<-html_text(html_nodes(webpage,'.value--1lKHt'))
    
    d<-matrix(text2)
    t<-matrix(text)
    
    tt<-t%>%cbind(d)%>%t()%>%data.frame
    names(tt)<-as.character(unlist(tt[1,]))
    tt<-tt[-1,]
    
    # treat amount problem
    a=as.character(html_text(html_nodes(webpage,'.amount--3NTpl')))
    am=ifelse(identical(a, character(0)),0,a)
    
    s=html_text(html_nodes(webpage,'.sub-title--37mkY'))
    su=ifelse(identical(s, character(0)),0,s)
    
    
    tt<-tt%>%mutate(amount=as.character(am),
                    subtitle=as.character(su),
                    url=remDr$getCurrentUrl()[[1]],
                    date=date<-Sys.Date())
    
    
    slist[[x]]<-tt
    x<-x+1
    
    #go back
    remDr$navigate(back)
    
  }
  
}

hh<-bind_rows(slist)

names(hh)<-c("brand","model","edition","moderl_year","condition","transmission","body_type","fuel_type",
             "engine_capacity","milage","amount","subtitle","url","date")


kt1<-hh%>%mutate(amount=as.numeric(sub(",","",sub(",","",sub("Rs ","",amount)))),
                 milage=as.numeric(sub("km","",sub(",","",milage))))%>%
  distinct()




path<-paste0("C:\\Users\\praveen\\Desktop\\4-12-2019\\praveen\\scrape_sugar2\\scrape2\\data\\data_1",Sys.Date(),".csv")

write.csv(kt1,path,row.names = F)






###  2nd website


x<-1

slist<-list()




for (i in 3:25){
  remDr$navigate(paste0("https://riyasewana.com/search/cars?page=",i))
  
  back<-paste0("https://riyasewana.com/search/cars?page=",i)
  
  print(i)
  
  for(k in 1:60){
    remDr$findElement("css", paste0(".round:nth-child(",k,") .more") )$clickElement()
    
    url2<-remDr$getCurrentUrl()[[1]]
    
    webpage <- read_html(url2)
    rank_data_html <- html_nodes(webpage,'.moreh')
    text <- html_text(rank_data_html)
    
    text2<-html_text(html_nodes(webpage,'.aleft'))
    
    d<-matrix(c(text2[1],text2[3],text2[5],text2[7],text2[9],text2[11],text2[13],text2[15],text2[17],text2[19],text2[21],text2[23],text2[27]))
    t<-matrix(c(text2[2],text2[4],text2[6],text2[8],text2[10],text2[12],text2[14],text2[16],text2[18],text2[20],text2[22],text2[24],text2[28]))
    
    tt<-d%>%cbind(t)%>%t()%>%data.frame()
    names(tt)<-as.character(unlist(tt[1,]))
    tt<-tt[-1,]
    
    # treat amount problem
    
    tt<-tt%>%mutate(url=remDr$getCurrentUrl()[[1]],
                    date=date<-Sys.Date())
    
    
    slist[[x]]<-tt
    x<-x+1
    
    #go back
    remDr$navigate(back)
    
  }
  
}


riya<-bind_rows(slist)

riya<-riya%>%rename("amount"="Price",
                    "Brand: "="Make",
                    "Model: "="Model",
                    "Model year: "="Model Year",
                    "Engine capacity: "="Engine (cc)",
                    "Transmission: "="Gear",
                    "Fuel type: "="Fuel Type",
                    "Mileage: "="Mileage (km)")

riya1<-riya%>%mutate(amount=as.numeric(sub(",","",sub(",","",sub("Rs. ","",amount)))),
                     `Mileage: `=as.numeric(sub("km","",sub(",","",`Mileage: `))))%>%
  distinct()



write.csv(riya,paste0("C:\\Users\\praveen\\Desktop\\4-12-2019\\praveen\\scrape_sugar2\\scrape2\\data\\\\data_riya",Sys.Date(),".csv"),row.names = F)


rD$server$stop()
remDr$close()







