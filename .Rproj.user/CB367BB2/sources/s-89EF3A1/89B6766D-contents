##Easily initialize the entire tidyverse suite of packages
install.packages('tidyverse')

library(dplyr)
library(lubridate)

##Read in data
accounts <- read.csv('csv/SampleAccounts.csv')
ledgerEntries <- read.csv('csv/SampleLedgerEntries.csv')
rates <- read.csv('csv/SampleRates.csv')
transactionTypes <- read.csv('csv/SampleTransactionTypes.csv')

##Create single fact table
fact <- ledgerEntries %>%
  inner_join(accounts, by='account_id') %>%
  inner_join(transactionTypes, by='type')

##Generate customer list
customers <- fact %>%
  distinct(customer_id)

##Generate individual customer statements

#Function to find conversion rates by closest timestamp
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

curatedTrans_full <- data.frame()

for(i in 1:length(customers[[1]]))
{
  customerStatement <- fact %>%
    filter(customer_id == customers[[1]][i]) %>%
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
    mutate(amount_usd = bal_diff * get_usd_rate(currency, ledgerTimestamp)) %>%
    select(customer_id, created_at, type, currency, transaction_name, bal_diff, amount_usd) %>%
    rename(transaction_amount = bal_diff, transaction_amount_usd = amount_usd)
  
  curatedTrans_full <- rbind(curatedTrans_full, customerStatement)
  
  write.csv(customerStatement, paste0('custStatements/', customers[[1]][i], '_statement.csv'), row.names=F)
}

##Customer status classification

curated_month <- curatedTrans_full %>%
  mutate(month = as.numeric(year(created_at))*100 + as.numeric(month(created_at)))

curated_month_only <- select(curated_month, customer_id, month)

months <- curated_month %>%
  distinct(month) %>%
  as.data.frame() %>%
  mutate(dummy=1)

customers <- customers %>%
  mutate(dummy=1)

customers_month <- full_join(customers, months, by='dummy') %>%
  select(-dummy)


#Function to determine transactional status
get_status <- function(customer, mon)
{
  trans <- filter(curated_month_only, customer == customer_id) %>%
    .$month
  
  active_current <- any(trans == mon)
  active_previous <- any(trans == (mon-1))
  active_prior <- any(trans < mon)
  
  status <- ifelse(active_current, ifelse(active_previous, 'Returning', ifelse(active_prior, 'Reactivated', 'New')), ifelse(active_prior, 'Churned', 'NULL'))
  
  status
}

customers_month <- customers_month %>%
  rowwise() %>%
  mutate(status = get_status(customer_id, month))


#Join status back to transaction table

final_curated <- curated_month %>%
  inner_join(customers_month, by = c("customer_id","month")) %>%
  rename(transactional_status = status) %>%
  select(-month)

write.csv(final_curated, 'finalAnswer/all_tasks.csv')
