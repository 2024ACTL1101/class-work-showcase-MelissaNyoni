---
title: "ACTL1101 Assignment Part A"
author: "Melissa Nyoni"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
#Define dates
start_date <- as.Date('2021-12-31')
end_date <- as.Date('2024-01-04')

#Filter the data to include only the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]
#within the range after(incl.) start_date and before(incl.) end_date
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  
  if (i == nrow(amd_df)) {
    
    #Sell all shares at the end
    
    amd_df$trade_type[i] = 'sell'
    amd_df$accumulated_shares[i] = accumulated_shares
    amd_df$costs_proceeds[i] = accumulated_shares*amd_df$close[i]
      
  } else if (previous_price == 0) {
    
    #Initial purchase of shares
    
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
    accumulated_shares <- share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
    
  } else {
    
    if (amd_df$close[i] < previous_price) { 
      
      #Purchase more shares
      
      amd_df$trade_type[i] <- 'buy'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
      accumulated_shares = accumulated_shares + share_size
      amd_df$accumulated_shares[i] <- accumulated_shares
      
    } else {
      
      #No trade
      
      amd_df$trade_type[i] <- ''
      amd_df$costs_proceeds[i] <- 0
      amd_df$accumulated_shares[i] = accumulated_shares
      
    }
  }
  
  #Update previous price
  previous_price <- amd_df$close[i]
  
}
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
#Initialize variables

total_profit_or_loss_1 <- 0

#Calculate total profit/loss

for (i in 1:nrow(amd_df)) {
  total_profit_or_loss_1 <- total_profit_or_loss_1 + amd_df$costs_proceeds[i]
}

#Calculate total invested capital
total_capital_invested_1 <- -(total_profit_or_loss_1 - amd_df$costs_proceeds[nrow(amd_df)])

#Calculate ROI
ROI_1 <- (total_profit_or_loss_1 / total_capital_invested_1) * 100
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
#Option 1

amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_accumulated_shares <- 0
total_purchase_price <- 0

for (i in 1:nrow(amd_df)) {
  
  if (i == nrow(amd_df)) {
    
    #Sell all shares at the end
    
    amd_df$trade_type[i] = 'sell'
    amd_df$accumulated_shares[i] = accumulated_shares
    amd_df$costs_proceeds[i] = accumulated_shares*amd_df$close[i]
      
  } else if (previous_price == 0) {
    
    #Initial purchase of shares
    
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
    total_purchase_price <- total_purchase_price - amd_df$costs_proceeds[i]
    accumulated_shares <- share_size
    total_accumulated_shares <- share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
    
  } else {
    
    #Calculate average share price
  
    average_purchase_price <- total_purchase_price/total_accumulated_shares
  
    if (amd_df$close[i] >= average_purchase_price*1.5) {
      
      #Sell half the shares
      amd_df$trade_type[i] <- 'sell'
      sell_shares = accumulated_shares/2
      amd_df$costs_proceeds[i] <- sell_shares*amd_df$close[i]
      accumulated_shares <- sell_shares
      amd_df$accumulated_shares[i] = accumulated_shares
    
    }else if (amd_df$close[i] < previous_price) { 
      
      #Purchase more shares
      
      amd_df$trade_type[i] <- 'buy'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
      total_purchase_price <- total_purchase_price - amd_df$costs_proceeds[i]
      accumulated_shares = accumulated_shares + share_size
      total_accumulated_shares = total_accumulated_shares + share_size
      amd_df$accumulated_shares[i] <- accumulated_shares
      
    } else {
      
      #No trade
      
      amd_df$trade_type[i] <- ''
      amd_df$costs_proceeds[i] <- 0
      amd_df$accumulated_shares[i] = accumulated_shares
      
    }
  }
  
  #Update previous price
  previous_price <- amd_df$close[i]
  
}

```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
#Initialize variables

total_profit_or_loss_2 <- 0

#Calculate total profit/loss
sales <- 0

for (i in 1:nrow(amd_df)) {
  total_profit_or_loss_2 <- total_profit_or_loss_2 + amd_df$costs_proceeds[i]
  if (amd_df$costs_proceeds[i] > 0) {
    sales = sales + amd_df$costs_proceeds[i]
  }
}

#Calculate total invested capital
total_capital_invested_2 <- -(total_profit_or_loss_2 - sales)

#Calculate ROI
ROI_2 <- (total_profit_or_loss_2 / total_capital_invested_2) * 100
```

## My Discussion
Late in the second half of 2023, AMD saw large increases in their GPU shipments which resulted in them gaining overall market share over their competitors in this particular market. This attributed to the sudden spike in share prices from that point onward as investors began to witness increased potential and opportunities for AMD over their competitors. During this period, my profit-taking strategy earned about $250,000 more in total profit due to the algorithm taking advantage of the significantly high share prices and taking the profits from that at the right time. This ultimately resulted in a much higher ROI as well.
 

-----------------------------------------------------------------------------------------------------
Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




