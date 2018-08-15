
# KP's Finances: From 2016 to Present -------------------------------------------

pacman::p_load(tidyverse, lubridate, forcats, readxl)


# Import ------------------------------------------------------------------

d_expense <-
  read_excel("data/My Money.xlsx", sheet = 1) %>%
  mutate_at(vars(Date), as_date) %>%
  drop_na(Date) %>%
  arrange(Date)



# Prepare -----------------------------------------------------------------
# The value 15229.15 is the amount present in my bank account at the end of 2015
# I've forgoten where the 150 comes from
# Multiplier is a net worth multiplier,
# # IRA shifts money around so it's 0,
# # HSA and 401k are not measured by value of direct deposit, so value of 1 adds to N.W.

d_daily <-
  d_expense %>%
  mutate(Day = as.Date(Date)) %>%
  group_by(Day, Type) %>%
  summarize(Amount = sum(Amount)) %>%
  ungroup() %>%
  complete(Day, Type, fill = list(Amount = 0)) %>%
  arrange(Day) %>%
  mutate(
    Month = month(Day),
    Year = year(Day),

    Category = fct_collapse(as_factor(Type),
                            "Savings" = c("IRA", "HSA - Income", "401k - Income"),
                            "Income"  = c("Income"),
                            "Debt"    = c("Student Loan", "Debt"),
                            "Expense" = c("Auto", "Dining", "Drinks", "Entertainment",
                                          "Fees", "Fuel", "Groceries", "Healthcare",
                                          "Misc", "Rent", "Subscription", "Travel",
                                          "Utilities")),

    Multiplier = case_when(Type == "IRA" ~ 0,
                           Type %in% c("Income", "HSA - Income", "401k - Income") ~ 1,
                           TRUE ~ -1),

    Net_Worth = cumsum(Multiplier * Amount) + 15229.15 + 150)



# Annual Tables -----------------------------------------------------------

# -- Totals
d_annually <-
  d_daily %>%
  filter(!is.na(Year)) %>%
  group_by(Year, Category) %>%
  summarise(Sum = sum(Amount)) %>%
  ungroup() %>%
  mutate(Category = fct_relevel(Category, "Income", "Savings", "Debt", "Expense"),
         Year = fct_rev(factor(Year)))


# -- Rates
d_rates <-
  d_annually %>%
  spread(Category, Sum) %>%
  group_by(Year) %>%
  summarise(Expense_Rate = (Expense + Debt) / Income,
            Savings_Rate = Savings / Income)



