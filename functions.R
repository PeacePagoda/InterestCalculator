library(dplyr)
library(tidyr)
library(highcharter)

calculate_compound_interest <- function(principal, interest_rate, no_of_compounds, no_of_time_periods) {
  p <- principal
  r <- interest_rate
  n <- no_of_compounds
  t <- no_of_time_periods
  final_amount =  p * (1 + (r/n))^(n*t)
  return(final_amount)
}


future_value_of_series <- function(deposits, interest_rate, no_of_compounds, no_of_time_periods){
  pmt <- deposits
  r <- interest_rate
  n <- no_of_compounds
  t <- no_of_time_periods
  
  future_value = pmt * ((((1 + (r/n))^(n*t)) - 1) / (r/n))
  return(future_value)
}


get_total_returns <- function(principal_return, compound_return){
  total_return = principal_return + compound_return
  total_return = round(total_return, digits=2)
  return(total_return)
}

get_time_intervals <-  function(year_month, years, months){
  if (year_month == 1){
    time = (months/12) + years
  }
  else {
    time = (years*12) + months
  }
  time = round(time, digits=2)
  return(time)
}


get_unit_of_time <-  function(year_month){
  if (year_month == 1){
    unit = "years"
  }
  else {
    unit = "months"
  }
  return(unit)
}

get_number_of_compounds <-  function(compound){
  if (compound == 1){
    compounds = 1
  }
  else {
    compounds = 12
  }
  return(compounds)
}


monthly_balance <- function(principal, interest_rate, compound, no_of_time_periods, deposits){
  p <- principal
  r <- interest_rate
  n <- get_number_of_compounds(compound)
  t <- no_of_time_periods
  d <- deposits
  
  balance <- c(p)
  for (x in 1:t){
    com <- p*((1+r/n)^(n*x)) + d*((((1+r/n)^(n*x)) - 1) / (r/n))
    balance = append(balance, com)
  }
  return(balance)
}

deposits <- function(principal, deposit, compound, no_of_time_periods){
  p <- principal
  d <- deposit
  n <- get_number_of_compounds(compound)
  t <- no_of_time_periods
  
  deposits <- c(p, rep(d*n, t))
  return(deposits)
}

time_periods <- function(no_of_time_periods){
  return(0:no_of_time_periods)
}

get_interest_summary <- function(principal, interest_rate, compound, no_of_time_periods, deposit){
  balance <- monthly_balance(principal, interest_rate, compound, no_of_time_periods, deposit)
  deposits <- deposits(principal, deposit, compound, no_of_time_periods)
  time_periods <- time_periods(no_of_time_periods)
    
  df <- as.data.frame(cbind(principal, balance, deposits, time_periods))
  
  df$total_deposits <- cumsum(df$deposits)
  df <- df %>% 
    mutate(total_interest=balance-total_deposits)
  df$total_interest <- round(df$total_interest, 2)
  
  df$interest <- df$total_interest - lag(df$total_interest)
  df$interest <- df$interest %>% replace_na(0)
  df$interest <- round(df$interest, 2)
  
  df <- df %>% select(time_periods, principal, deposits, total_deposits, interest, total_interest, balance)
  
  names(df) <- c("Year", "Initial Deposit", "Deposits", "Cum. Deposits", "Interest", "Cum. Interest", "Balance")
  
  number_rows <- no_of_time_periods+1
  
  result = list(df=df, number_rows=number_rows)
  
  return(result)
}


plot_results <- function(result){
  highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_xAxis(title = list(text = "Years")) %>% 
    hc_plotOptions(column = list(
      dataLabels = list(enabled = FALSE),
      stacking = "normal",
      enableMouseTracking = TRUE)
    ) %>% 
    hc_series( list(name="Total Interest",data=result[['df']]$`Cum. Interest`, color="red"),
               list(name="Total Deposits",data=result[['df']]$`Cum. Deposits`, color="green")
    )
  return(highchart())
}

get_interest_summary(1000, 0.05, 1, 5, 100)
