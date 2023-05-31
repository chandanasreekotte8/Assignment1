## Question 1
library(rvest)
library(tidyverse)

html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
names <- html_elements(html,".company-ellipses a") %>% html_text()
cmd <- html_elements(html,"td") %>% html_text()
value = 0
for(i in 0:49)
{
  value[i+1] = cmd[i*15+3]
}
price_change = 0
for(i in 0:49)
{
  price_change[i+1] = cmd[i*15+4]
}
market_cap = 0
for(i in 0:49)
{
  market_cap[i+1] = cmd[i*15+5]
}
week_high= 0
for(i in 0:49)
{
  week_high[i+1] = cmd[i*15+6]
}
week_low=0
for(i in 0:49)
{
  week_low[i+1] = cmd[i*15+7]
}
ROE=0
for(i in 0:49)
{
  ROE[i+1] = cmd[i*15+8]
}
PE=0
for(i in 0:49)
{
  PE[i+1] = cmd[i*15+9]
}
PBV=0
for(i in 0:49)
{
  PBV[i+1] = cmd[i*15+10]
}
EV=0
for(i in 0:49)
{
  EV[i+1] = cmd[i*15+11]
}
SALES=0
for(i in 0:49)
{
  SALES[i+1] = cmd[i*15+12]
}
PROFIT=0
for(i in 0:49)
{
  PROFIT[i+1] = cmd[i*15+13]
}


############################################################

## Question2

#part b
#hdfc life insurance
#scraping data
DATE <- c(0)
for(i in 1:10)
{
  DATE[i] <- paste("Mar",12+i)
}
DATE <- append(DATE,'TTM')
html <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/bfsi/insurance/hdfc-life-insurance/company-info")
Sales <- html %>% html_nodes("tbody~ tbody tr:nth-child(1) td+ td") %>% html_text()
YoY_Gr_Rt1 <- html %>% html_nodes("tbody~ tbody tr:nth-child(2) td+ td")%>%html_text()
Adj_EPS <- html%>%html_nodes("tbody~ tbody tr:nth-child(3) td+ td")%>%html_text()
YoY_Gr_Rt2 <- html%>%html_nodes("tbody~ tbody tr:nth-child(4) span")%>%html_text()
BVPS <- html%>%html_nodes("tbody~ tbody tr:nth-child(5) td+ td")%>%html_text()
Adj_Net_Profit <- html%>%html_nodes("tbody~ tbody tr:nth-child(6) td+ td")%>%html_text()
Cash_flow_from_Ops <- html%>%html_nodes("tr:nth-child(7) td+ td , tr:nth-child(7) .stick+ td")%>%html_text()
Debt_CF_from_Ops <- html%>%html_nodes("tr:nth-child(8) td+ td span")%>%html_text()
Return_on_Equity <- html%>%html_nodes("#tenyearxraysa .col-12 tr:nth-child(1) td+ td span")%>%html_text()
OP_Profit_Mgn <- html%>%html_nodes(".col-12.mt-4 tr:nth-child(2) td+ td , .col-12.mt-4 tr:nth-child(2) .stick+ td")%>%html_text()
Net_Profit_Mgn <- html%>%html_nodes(".col-12.mt-4 tr:nth-child(3) td+ td")%>%html_text()
Debt_to_Equity <- html%>%html_nodes(".col-12.mt-4 tr+ tr td+ td span")%>%html_text()
Working_cap_days <- html%>%html_nodes(".mt-4 tr:nth-child(5) td+ td , .mt-4 tr:nth-child(5) .stick+ td")%>%html_text()
Cash_conv_cycle <- html%>%html_nodes(".mt-4 tr:nth-child(6) td+ td , .mt-4 tr:nth-child(6) .stick+ td")%>%html_text()
Hdfc_Life_Insurance <- data.frame(DATE,Sales,YoY_Gr_Rt1,Adj_EPS,YoY_Gr_Rt2,BVPS,Adj_Net_Profit,Cash_flow_from_Ops,Debt_CF_from_Ops,Return_on_Equity,OP_Profit_Mgn,Net_Profit_Mgn,Debt_to_Equity,Working_cap_days,Cash_conv_cycle,row.names = TRUE)
Hdfc_Life_Insurance <- t(Hdfc_Life_Insurance)

###########################################################

### C
## Part 1
tennis <- function(p){
  a = 0 
  b = 0 
  c = 0
  while(a!=3 && b!= 3){
    a = a + sample(x=0:1,size=1,prob=c(1-p,p))
    c = c + 1
    b = c - a
  }
  return(a+b)
}

## part 2

matches <- c(0,0)

for(i in 1:1000) {
  matches[i] <- tennis(0.7)
}

ans <- mean(matches)


######################################################################
## Question 4
MontyHall <- function() 
{
  win <- sample(0:1,size = 1,prob = c(1/3,2/3))
  return(win)
}


t = 0 
for(i in 1: 1000){
  t = t + MontyHall()
}
t

###########################################################################
## Question 5
rank <- 1:100
link <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
a <- link%>%html_elements(".article_movie_title a")%>%html_text()
score <- link%>%html_elements(".tMeterScore")%>%html_text()
year <- link%>%html_elements(".start-year")%>%html_text()
moviedata <- data.frame(
  Ranking <- rank,
  Name <- a,
  Tomato_Score <- score,
  Year<-year
)

