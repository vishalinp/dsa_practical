##Easily initialize the entire tidyverse suite of packages
install.packages('tidyverse')

library(dplyr)
library(lubridate)

##Read in data
accounts <- read.csv('csv/SampleAccounts.csv')
#transactions <- read.csv('csv/SampleCuratedTransactionsList.csv')
ledgerEntries <- read.csv('csv/SampleLedgerEntries.csv')
rates <- read.csv('csv/SampleRates.csv')
transactionTypes <- read.csv('csv/SampleTransactionTypes.csv')

##Create single fact table
fact <- ledgerEntries %>%
  inner_join(accounts, by='account_id') %>%
  inner_join(transactionTypes, by='type')


#Find customer of interest to understand transactions
focusCustomer <- fact %>%
  group_by(customer_id) %>%
  summarise(transTypes = n_distinct(type)) %>%
  arrange(desc(transTypes)) %>%
  slice(1)

focusCustomerStatement <- fact %>%
  filter(customer_id == focusCustomer[[1]]) %>%
  arrange(created_at)

focusCustomerStatement2 <- fact %>%
  filter(customer_id == '1cf4eac9d442eda8329403546901e682'[[1]]) %>%
  group_by(account_id) %>%
  arrange(created_at) %>%
  mutate(balance = balance_1e8/1e8, available = available_1e8/1e8) %>%
  mutate(bal_diff = balance - ifelse(is.na(lag(balance)), 0, lag(balance))) %>%
  ungroup() %>%
  filter((currency=='XBT' & type %in% c(1,4,6,7,14,15,22,24)) | (currency!='XBT' & type %in% c(1,4))) %>%
  mutate(transaction_name = case_when(.$type==1 ~ ifelse(bal_diff > 0, ifelse(currency=='XBT', 'ReceiveBTC', 'FiatDeposit'), ifelse(currency=='XBT', 'SendBTC', 'FiatWithdrawal')),
                                      .$type==4 ~ ifelse(currency=='XBT', 'Ask', 'Bid'),
                                      .$type==6 ~ 'TradeSell',
                                      .$type==7 ~ 'TradeBuy',
                                      .$type==14 ~ 'SellBTC',
                                      .$type==15 ~ 'BuyBTC',
                                      .$type==22 ~ 'TradeBidFeeDebit',
                                      .$type==24 ~ 'TradeAskFeeDebit')) %>%
  mutate(ledgerTimestamp = as.integer(as_datetime(created_at))) %>%
  rowwise()%>%
  dplyr::mutate(amount_usd = bal_diff * get_usd_rate(currency, ledgerTimestamp),
         rate = get_usd_rate(currency, ledgerTimestamp),
         tdiff_min = get_rate_tdiff(currency, ledgerTimestamp)) %>%
  select(customer_id, created_at, type, currency, transaction_name, bal_diff, amount_usd) %>%
  rename(transaction_amount = bal_diff, transaction_amount_usd = amount_usd)


get_usd_rate <- function(currency, l_timestamp)
{
  rate_row_xbtusd <- rates %>%
    filter(pair == 'XBTUSD') %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)
  
  rate_row_toxbt <- rates %>%
    filter(pair == paste0('XBT', currency)) %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)
  
  ifelse(currency == 'XBT', as.numeric(rate_row_xbtusd$exchange_rate_1e6[1]/1e6),
         as.numeric(rate_row_toxbt$exchange_rate_1e6[1]/1e6)/as.numeric(rate_row_xbtusd$exchange_rate_1e6[1]/1e6))
}


get_usd_rate <- function(currency, l_timestamp)
{
  rate_row_xbtusd <- rates %>%
    filter(pair == 'XBTUSD') %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)
  
  as.numeric(rate_row_xbtusd$exchange_rate_1e6[1]/1e6)
}

get_xbt_rate <- function(currency, l_timestamp)
{
  rate_row_xbtusd <- rates %>%
    filter(pair == 'XBTUSD') %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)
  
  rate_row_toxbt <- rates %>%
    filter(pair == paste0('XBT', currency)) %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)
  
    as.numeric(rate_row_toxbt$exchange_rate_1e6[1]/1e6)/as.numeric(rate_row_xbtusd$exchange_rate_1e6[1]/1e6)
}


get_rate_tdiff <- function(currency, l_timestamp)
{
  tdiff <- NA
  
  rate_row_xbtusd <- rates %>%
    filter(pair == 'XBTUSD') %>%
    mutate(diff_sec = abs(l_timestamp - timestamp/1000.0)) %>%
    arrange(diff_sec) %>%
    slice(1)

    tdiff <- as.numeric(rate_row_xbtusd$diff_sec[1]/60)
    
    tdiff
}


##rough
rate <- rates %>%
  filter(pair == 'XBTUSD') %>%
  mutate(diff_sec = abs(1433119759298 - timestamp/1000.0)) %>%
  arrange(diff_sec) %>%
  slice(1) %>%
  as.numeric(.$exchange_rate_1e6[1]/1e6)

rate_row_xbtusd <- rates %>%
  filter(pair == 'XBTUSD') %>%
  mutate(diff_sec = abs(1439117772 - timestamp/1000.0)) %>%
  arrange(diff_sec) %>%
  slice(1)




##Timestamps
rates2 <- mutate(rates, timestamp=timestamp/1000)
class(rates2$timestamp)

as.integer(as_datetime('2015-06-01 1:49:21.969'))

difftime('1970-01-01 00:00:00.000', '2015-06-01 1:49:21.969', units='secs')
