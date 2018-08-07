
# Expense - Retirement ----------------------------------------------------

pacman::p_load(tidyquant, ggthemes, viridis, highcharter, plotly, rio, caTools,
               lubridate, forcats, tidyverse, formattable, RColorBrewer, anytime,
               stringr)

source("R/Expense - 01 - Data Prep.R")



# Parameters --------------------------------------------------------------

DoB <- "1988-09-20"
Age <- interval(DoB %>% anytime(), now()) %>% as.period()

AnnualIncomeAfterTax <- 50000
AnnualSavings <- 30000
AnnualExpense <- 20000

CurrentSavings <- 25000

GrowRate <- 0.07
PullRate <- 0.04

AnnualRetirementSpending <- 30000

Savings_Rate <- AnnualSavings / AnnualIncomeAfterTax

FinancialIndependenceNumber <- AnnualRetirementSpending / PullRate
YearsToFInoInvestment <- (FinancialIndependenceNumber - CurrentSavings) / AnnualSavings



# Calculations ------------------------------------------------------------

Table <-
  tibble(
    Year = 0:45,
    Income = AnnualIncomeAfterTax,
    Expenses = AnnualExpense,
    ROI = AnnualSavings * GrowRate * Year,
    Delta = CurrentSavings + ROI,
    Networth = cumsum(Delta),
    FI = if_else(Networth > FinancialIndependenceNumber, "*", "")
  )


Table %>%
  ggplot(aes(x = Year, y = Networth)) +
  geom_line() +
  geom_hline(aes(yintercept = FinancialIndependenceNumber)) +
  scale_y_continuous()



# My Data -----------------------------------------------------------------

GrowRate <- 0.07
PullRate <- 0.04

Narrow_D %>% glimpse()


# --- Yearly
Annual_Income <-
  Narrow_D %>%
  filter(Category == "Income") %>%
  group_by(Year) %>%
  summarise(Annual_Income = sum(Amount))

Annual_Expense <-
  Narrow_D %>%
  filter(Category %in% c("Expense", "Debt")) %>%
  group_by(Year) %>%
  summarise(Annual_Expense = sum(Amount))

Annual_Savings <-
  Narrow_D %>%
  filter(Category == "Savings") %>%
  mutate(Investment = if_else(Type == "HSA - Income", "Annual_Savings", "Annual_Investments")) %>%
  group_by(Year, Investment) %>%
  summarise(Annual_Savings = sum(Amount)) %>%
  spread(Investment, Annual_Savings)


Annual_Current_Standings <-
  Annual_Income %>%
  left_join(Annual_Expense) %>%
  left_join(Annual_Savings) %>%
  mutate(Annual_Cash = Annual_Income - (Annual_Savings + Annual_Investments + Annual_Expense),
         Investment_Interest = Annual_Investments * GrowRate,
         Change = Annual_Investments + Investment_Interest + Annual_Savings + Annual_Cash,
         Savings_Rate = (Annual_Investments + Annual_Savings) / Annual_Income,
         Net_Worth = cumsum(Change) + 15229.15
  )


# --- Monthly
Monthly_Income <-
  Narrow_D %>%
  filter(Category == "Income") %>%
  group_by(Year, Month) %>%
  summarise(Monthly_Income = sum(Amount))

Monthly_Expense <-
  Narrow_D %>%
  filter(Category %in% c("Expense", "Debt")) %>%
  group_by(Year, Month) %>%
  summarise(Monthly_Expense = sum(Amount))

Monthly_Savings <-
  Narrow_D %>%
  filter(Category == "Savings") %>%
  mutate(Investment = if_else(Type == "HSA - Income", "Monthly_Savings", "Monthly_Investments")) %>%
  group_by(Year, Month, Investment) %>%
  summarise(Monthly_Savings = sum(Amount)) %>%
  spread(Investment, Monthly_Savings)

Monthly_Current_Standings <-
  Monthly_Income %>%
  left_join(Monthly_Expense) %>%
  left_join(Monthly_Savings) %>%
  ungroup() %>%
  mutate(Monthly_Cash = Monthly_Income - (Monthly_Savings + Monthly_Investments + Monthly_Expense),
         Investment_Interest = Monthly_Investments * GrowRate,
         Change = Monthly_Investments + Investment_Interest + Monthly_Savings + Monthly_Cash,
         Savings_Rate = (Monthly_Investments + Monthly_Savings) / Monthly_Income,
         Net_Worth = cumsum(Change) + 15229.15
  )



# Looking Ahead -----------------------------------------------------------
# UNDER DEVELOPMENT -- USE TIDYQUANT
# IDK w HSA

pacman::p_load(anytime, stringr, plyr, tidyquant, foreach)

Starting_Point <-
  Monthly_Current_Standings %>%
  slice(n()) %>%
  mutate(Date = anytime::anydate(paste(Year, Month)) %>% ceiling_date(unit = "month")) %>%
  select(Date, Net_Worth)

# Monthly average of the previous 12 months
# Multiply by 12 to get a yearly average going forward
Assumptions <-
  Monthly_Current_Standings %>%
  ungroup() %>%
  slice((n() - 11):n()) %>%
  summarise_at(3:10, mean) %>%
  mutate_at(1:7, function(x) x*12) %>%
  rename_all(funs(str_replace_all(., "Monthly_", "")))


FI_Table_Sim <-

  foreach(GrowRate = c(0.04, 0.05, 0.06, 0.07),
          .combine = "rbind") %do% {

    Annual_Current_Standings %>%
      bind_rows(
        tibble(
          Year = seq.Date(from = ceiling_date(Starting_Point$Date, "year"),
                          to = anydate("2050-01-01"),
                          by = "year") %>% year(),
          N = seq_along(Year),
          Annual_Income = Assumptions$Income %>% round_any(1000, floor),
          Annual_Expense = Assumptions$Expense %>% round_any(5000, ceiling),
          Annual_Investments = Assumptions$Investments %>% round_any(100),
          Annual_Savings = Assumptions$Savings %>% round_any(100),
          Annual_Cash = Assumptions$Cash %>% round_any(100),
          Growth = GrowRate,
          Investment_Interest = Annual_Investments * Growth * N,
          Change = Annual_Investments + Annual_Savings + Annual_Cash + Investment_Interest,
          Running_Change = cumsum(Change)
        )
      ) %>%
      mutate(
        Cash_Base = cumsum(Annual_Investments + Annual_Savings + Annual_Cash),

        Net_Worth =
          case_when(is.na(N) ~ Net_Worth,
                    !is.na(N) ~ Running_Change + Starting_Point$Net_Worth),
        Independent =
          if_else(Net_Worth >= (Annual_Expense / PullRate),
                  "***", ".")
      )

  }

FI_Table <-
  Annual_Current_Standings %>%
  bind_rows(
    tibble(
      Year = seq.Date(from = ceiling_date(Starting_Point$Date, "year"),
                      to = anydate("2050-01-01"),
                      by = "year") %>% year(),
      N = seq_along(Year),
      Annual_Income = Assumptions$Income %>% round_any(1000),
      Annual_Expense = Assumptions$Expense %>% round_any(10000),
      Annual_Investments = Assumptions$Investments %>% round_any(100),
      Annual_Savings = Assumptions$Savings %>% round_any(100),
      Annual_Cash = Assumptions$Cash %>% round_any(100),
      Growth = 0.06,
      Investment_Interest = Annual_Investments * Growth * N,
      Change = Annual_Investments + Annual_Savings + Annual_Cash + Investment_Interest,
      Running_Change = cumsum(Change)
    )
  ) %>%
  mutate(
    Cash_Base = cumsum(Annual_Investments + Annual_Savings + Annual_Cash),

    Net_Worth =
      case_when(is.na(N) ~ Net_Worth,
                !is.na(N) ~ Running_Change + Starting_Point$Net_Worth),
    Independent =
      if_else(Net_Worth >= (Annual_Expense / PullRate),
              "***", ".")
    )




# The Plot ----------------------------------------------------------------

FI_Table %>%
  ggplot(aes(x = Year %>% as.integer(), y = Net_Worth)) +
  geom_line() +
  geom_line(aes(x = Year, y = Cash_Base),
            color = "gray") +
  geom_vline(xintercept = FI_Table %>% filter(N == 1) %>% pull(Year),
             linetype = 2,
             color = "gray") +
  geom_vline(xintercept = FI_Table %>% filter(Independent == "***") %>% slice(1) %>% pull(Year),
             linetype = 2,
             color = "red") +
  theme_minimal()


FI_Table %>%
  group_by(Growth) %>%
  plot_ly(x = ~Year, y = ~Net_Worth, color = ~factor(Growth)) %>%
  add_lines() %>%
  add_markers(x = ~Year, y = ~Net_Worth,
              data = FI_Table %>% group_by(Growth) %>% filter(Independent == "***") %>% slice(1))



# Stock Performance -------------------------------------------------------



today <- Sys.Date() %>% as.character()

stocks <-
  tribble(~symbol, ~account, ~initial, ~final, ~distribution,
          "vffvx", "IRA",  "2017-01-12", "2017-10-09", 1.00,
          "vbmfx", "IRA",  "2017-10-10", today,        0.20,
          "vgtsx", "IRA",  "2017-10-10", today,        0.30,
          "vtsmx", "IRA",  "2017-10-10", today,        0.50,
          "vitsx", "401k", "2017-06-13", today,        0.50,
          "vtiax", "401k", "2017-06-13", today,        0.25,
          "vbtix", "401k", "2017-06-13", today,        0.25,
          "vbtlx", "HSA",  "2017-10-16", today,        1.00)

stocks_roi <-
  stocks %>%
  mutate(initial = anydate(initial),
         final = anydate(final),

         stock = map(symbol, ~tq_get(.x) %>% select(date, adjusted)),

         roi_me = pmap_dbl(lst(stock, initial, final),
                           ~ filter(.data = ..1, date >= ..2, date <= ..3) %>%
                             summarise(r = (last(adjusted) - first(adjusted)) / first(adjusted)) %>%
                             pull(r)))

# Apply this information
stocks %>% mutate(dr = roi_me*distribution)

# Between these dates
# returns <-
  # Narrow_D %>%
  # filter(Category == "Savings") %>%
  # select(Date = Day, Type, Amount) %>%
  # mutate(Type = str_replace_all(Type, " - Income", "")) %>%
  #   left_join(stocks_roi, by = c("Type" = "account"))

# %>%
#   spread(Type, Amount) %>%
#
#   mutate(c_401k = cumsum(`401k`),
#          c_HSA = cumsum(HSA),
#          c_IRA = cumsum(IRA))
#
# returns %>%
#   mutate(
#     r_401k = cut(Date, breaks = )
#   )


# foreach(i = nrow(stocks)) %do% {
#
#   symbol <- stocks$symbol[[i]]
#   initial <- stocks$initial[[i]]
#   final <- stocks$final[[i]]
#
#   tq_get(symbol) %>%
#     select(date, adjusted) %>%
#     filter(date >= initial,
#            date <= final) %>%
#     rename_at("adjusted", ~paste(symbol))
#
# }

function_stock <- function(symbol, initial_date, final_date) {

  tq_get(symbol) %>%
    select(date, adjusted) %>%
    filter(date >= initial,
           date <= final) %>%
    rename_at("adjusted", ~paste(symbol))

}

s1 <-
  stocks %>%
  transmute(performance = pmap(lst(symbol, initial, final),
                               function_stock))

s2 <- s1 %>% unnest()

s3 <- s2 %>% gather(symbol, share_price, -date)

left_join(s3, stocks %>% select(symbol, account)) %>%
  distinct(date, symbol, account, .keep_all = TRUE) %>%


e0 <-
  Narrow_D %>%
  filter(Category == "Savings",
         Amount > 0) %>%
  select(Date = Day, Type, Amount) %>%
  mutate(Type = str_replace_all(Type, " - Income", ""))


s3

c0 <-
  left_join(e0,
            stocks %>% select(symbol, account),
            by = c("Type" = "account")) %>%
  select(-Type, Date, symbol, cash = Amount)

e0 %>% View()
c0 %>% View()

c0 %>% left_join(s3, by = c("Date" = "date", "symbol")) %>% View()
