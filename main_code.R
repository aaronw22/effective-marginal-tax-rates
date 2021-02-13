## Effective Marginal Tax Rates model ##
## Updated for 2020-21 Financial Year ##

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

# Taxes payable are negative numbers, tax offsets are positive numbers

# Generate pre-tax income vector
pre_tax_income <- data.table(pre_income = seq(from = 1, to = 200000, by = 1))

# Create a list to store tax payable amounts
payable <- list()

## Useful functions
tabs_join <- function(tax_table) {
  calc_table <- copy(pre_tax_income)
  
  calc_table <- tax_table[calc_table, on = c("income" = "pre_income"), roll = Inf]
  
  return(calc_table)
}

marg_tax <- function(data) {
  data[, tax_payable := -1 * ((income - threshold) * marginal + previous)]
  data[, c("threshold", "marginal", "previous") := NULL]
}

avg_tax <- function(data) {
  data[, tax_payable := -1 * (income * average)]
  data[, c("average") := NULL]
}

## Personal income tax and tax offsets module ####

# Load the tax rates
income_tax_tab <- fread("./Inputs/personal income tax rates.csv")
low_tax_offset_tab <- fread("./Inputs/low income tax offset rates.csv")
lm_tax_offset_tab <- fread("./Inputs/low middle income tax offset rates.csv")

## Calculate income tax payable

# Rolling join income tax schedule
payable[["income_tax"]] <- tabs_join(income_tax_tab)

# Calculate tax
payable[["income_tax"]] <- marg_tax(payable[["income_tax"]])

## Calculate Low Income Tax Offset

# Rolling join offset schedule
payable[["low_offset"]] <- tabs_join(low_tax_offset_tab)

# Calculate tax
payable[["low_offset"]] <- marg_tax(payable[["low_offset"]])

# Change negative values to zero
payable[["low_offset"]][tax_payable < 0, tax_payable := 0]

## Calculate Low and Middle Income Tax Offset

# Rolling join offset schedule
payable[["low_mid_offset"]] <- tabs_join(lm_tax_offset_tab)

# Calculate tax
payable[["low_mid_offset"]] <- marg_tax(payable[["low_mid_offset"]])

# Change negative values to zero
payable[["low_mid_offset"]][tax_payable < 0, tax_payable := 0]

## Medicare levy and levy surcharge module ####

# Load the tax rates
medicare_tab <- fread("./Inputs/medicare levy rates.csv")
medicare_sur_tab <- fread("./Inputs/medicare levy surcharge rates.csv")

# Rolling join Medicare levy
payable[["med_levy"]] <- tabs_join(medicare_tab)

# Calculate tax
payable[["med_levy"]] <- avg_tax(payable[["med_levy"]])

# Rolling join Medicare levy surcharge
payable[["med_levy_sur"]] <- tabs_join(medicare_sur_tab)

# Calculate tax
payable[["med_levy_sur"]] <- avg_tax(payable[["med_levy_sur"]])

## HELP debts repayment module ####

# Load the tax rates
hecs_help_tab <- fread("./Inputs/help debt rates.csv")

# Rolling join HELP repayments
payable[["help_debt"]] <- tabs_join(hecs_help_tab)

# Calculate tax
payable[["help_debt"]] <- avg_tax(payable[["help_debt"]])

## Superannuation taxes module ####

# Load the tax rates
super_tax_tab <- fread("./Inputs/super tax rates.csv")

## Combine all the various taxes together ####

# rbind into single table
payable <- rbindlist(payable, idcol = "tax_name")
 
# Sum the taxes and offsets at each income level
payable <- payable[, .(tax_payable = sum(tax_payable)), by = income]

# Calculate after-tax income
payable[, income_post := income + tax_payable]

# Calculate marginal and average tax rates
payable[, marginal := -1 * (tax_payable - shift(tax_payable)) / (income - shift(income))]
payable[, average := -1 * tax_payable / income]

graph <-
  ggplot(data = payable) +
  geom_line(aes(x = income, y = marginal))

plotly_graph <- ggplotly(graph)

htmlwidgets::saveWidget(plotly_graph, file = "plotly_graph.html")
