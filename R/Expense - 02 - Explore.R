
# Expenses - Data Exploration ---------------------------------------------

source('R/Expense - 01 - Data Prep.R')


pacman::p_load(
  tidyquant,
  ggthemes,
  viridis,
  highcharter,
  plotly,
  rio,
  caTools,
  lubridate,
  forcats,
  tidyverse,
  formattable,
  RColorBrewer
)


Expense_List <-
  c("Dining",
    "Drinks",
    "Entertainment",
    "Travel",
    "Utilities",
    "Subscription",
    "Rent",
    "Fuel",
    "Groceries",
    "Misc",
    "Auto",
    "Healthcare")




# | ggplot - Individual Expenses ------------------------------------------

# G_Individual_Expenses <-
#   Narrow_D %>%
#
#   filter(!Type %in% c("Rent", "Auto", "Healthcare", "Fees"),
#          Category == "Expense") %>%
#   mutate(Week = floor_date(Day, "week")) %>%
#   group_by(Week, Type) %>%
#   summarise(Week_Sum = sum(Amount)) %>%
#
#   ggplot(aes(x = Week, y = Week_Sum)) +
#   facet_wrap(~Type, scales = "free_y") +
#   scale_y_continuous(name = NULL, labels = NULL) +
#   geom_line(color = "darkgray") +
#   geom_smooth(span = .2, se = FALSE) +
#   theme_tq() +
#   scale_x_date(name = NULL, date_labels = "%b %Y")




# | Plotly - Net Worth ------------------------------------------------------

P_Net_Worth <-
  Narrow_D %>%

  plot_ly(x = ~Day,
          y = ~Net_Worth,
          type = "scatter",
          mode = "lines",
          text = ~paste(currency(Net_Worth)),
          hoverinfo = "text",
          hoveron = "points",
          line = list(width = 1, color = "black", shape = "step")) %>%

  add_trace(y = 0,
            hoverinfo = "none",
            line = list(color = "red", width = 1)) %>%

  layout(font = list(size = 11),
         showlegend = FALSE,
         hovermode = "x",

         xaxis = list(title = "",
                      showgrid = TRUE,
                      showline = FALSE,
                      autotick = TRUE,
                      zeroline = TRUE),

         yaxis = list(title = "",
                      showgrid = TRUE,
                      showticklabels = TRUE,
                      showline = TRUE,
                      autotick = TRUE,
                      ticks = "outside",
                      tickwidth = 1,
                      ticklen = 4,
                      zeroline = TRUE,
                      tozero = TRUE)
  )



# | Plotly - Savings v Expense --------------------------------------------

P_Savings_Expense <-
  Table_Years %>%

  group_by(Year) %>%
  mutate(Prop = Sum / dplyr::last(Sum)) %>%

  plot_ly(x = ~Sum,
          y = ~Year,
          color = ~Category,
          colors = c("#66bd63", "#a6d96a", "#fdae61", "#f46d43"),
          type = "bar",
          text = ~paste(percent(Prop, 0), "\n", currency(Sum)),
          hoverinfo = "text",
          marker = list(line = list(color = "white", width = 1))) %>%

  layout(xaxis = list(title = "",
                      zeroline = FALSE),
         yaxis = list(title = "",
                      autorange = "reversed",
                      zeroline = FALSE)
  )




# | Plotly - Expense Proportions ------------------------------------------

# Narrow_D %>% count(Type)

P_Expense_Proportion <-
  Narrow_D %>%

  filter(Type %in% Expense_List,
         Day < floor_date(Sys.Date(), "month")) %>%
  group_by(Year, Type) %>%
  summarise(Sum = sum(Amount)) %>%
  group_by(Year) %>%
  mutate(Proportion = Sum / sum(Sum),
         Cu_Prop = cumsum(Proportion),
         Y_Axis = cumsum(Proportion) - (Proportion / 2)) %>%


  plot_ly() %>%
  add_bars(x = ~factor(Year),
           y = ~Proportion,
           text = ~paste(Type, "\n", percent(Proportion, 0), "\n", currency(Sum)),
           color = ~Type,
           colors = brewer.pal(10, "Set3"),
           marker = list(line = list(color = "black", width = 1)),
           hoverinfo = "text") %>%

  layout(
    barmode = "stack",
    xaxis = list(title = ""),
    yaxis = list(title = "", showticklabels = FALSE))



# | Plotly - Monthly Expenses ---------------------------------------------


P_Monthly_Expenses <-
  Narrow_D %>%

  filter(Type %in% Expense_List,
         Day < floor_date(Sys.Date(), "month")) %>%
  group_by(Year, Month, Type) %>%
  summarise(Month_Sum = sum(Amount)) %>%
  group_by(Year, Type) %>%
  summarise(Month_Avg = mean(Month_Sum)) %>%

  plot_ly(x = ~factor(Year),
          y = ~Month_Avg,
          color = ~Type,
          type = "bar",
          text = ~paste(Type, "\n", currency(Month_Avg)),
          hoverinfo = "text",

          colors = brewer.pal(10, "Set3"),
          marker = list(line = list(color = "black", width = 1))) %>%

  layout(barmode = "stack",
         xaxis = list(title = "", showline = TRUE),
         yaxis = list(title = "", zeroline = FALSE)
  )



# | Plotly - Weekly Expenses ----------------------------------------------
# between includes -1 in order to exclude the current week

P_Weekly_Expense <-
  Narrow_D %>%
  mutate(Week = floor_date(Day, "week")) %>%
  filter(Amount > 0,
         Type %in% setdiff(Expense_List, "Rent"),
         between(Week, as_date("2016-01-01"), floor_date(Sys.Date(), "week") - 1)) %>%
  group_by(Year, Week) %>%
  summarise(Amount = sum(Amount)) %>%
  ungroup() %>%
  mutate(Average = runmean(Amount, k = 12, align = "right")) %>%

  plot_ly(x = ~Week,
          y = ~Amount,
          type = "scatter",
          mode = "lines",
          hoverinfo = "text",
          hoveron = "points",
          text = ~paste(currency(Amount)),
          line = list(shape = "spline",
                      width = .5,
                      color = "black")) %>%

  add_trace(y = ~Average,
            mode = "lines",
            line = list(shape = "line",
                        color = "blue",
                        width = 1.5),
            hoverinfo = "none") %>%

  add_trace(y = 200,
            mode = "lines",
            line = list(color = "red", width = 1),
            hoverinfo = "none") %>%

  layout(
    font = list(size = 11),
    showlegend = FALSE,
    hovermode = "x",

    xaxis = list(title = "",
                 showgrid = TRUE,
                 showline = FALSE,
                 autotick = TRUE,
                 zeroline = TRUE),

    yaxis = list(title = "",
                 showgrid = TRUE,
                 showticklabels = TRUE,
                 showline = TRUE,
                 autotick = TRUE,
                 ticks = "outside",
                 tickwidth = 1,
                 ticklen = 4,
                 zeroline = FALSE,
                 tozero = TRUE)
  )




# | Plotly - Individual Expenses --------------------------------------------

D0 <-
  Narrow_D %>%
  filter(!Type %in% c("Rent", "Auto", "Healthcare", "Fees",
                      "Utilities"),
         Category == "Expense") %>%
  mutate(Week = floor_date(Day, "week"),
         Month = floor_date(Day, "month")) %>%
  filter(Week < floor_date(Sys.Date(), "week"),
         Week > as_date("2016-01-01")) %>%
  select(Week, Month, Type, Amount)


DW <-
  D0 %>%
  rename(Date = Week) %>%
  group_by(Date, Type) %>%
  summarise(Sum = sum(Amount)) %>%
  ungroup() %>% group_by(Type) %>%
  mutate(Avg = runmean(Sum, k = 8, align = "right")) %>%
  ungroup()


DM <-
  D0 %>%
  rename(Date = Month) %>%
  group_by(Date, Type) %>%
  summarise(Sum = sum(Amount)) %>%
  ungroup() %>% group_by(Type) %>%
  mutate(Avg = runmean(Sum, k = 6, align = "right")) %>%
  ungroup()






# --- The Plots

# D_Types <- D0 %>% distinct(Type) %>% pull()
# Data <- DM
# Type <- quo(D_Types[[7]])
#
#   Data %>%
#   filter(Type == !!Type) %>%
#
#   plot_ly(x = ~Date, y = ~Sum, type = "scatter", mode = "lines",
#           text = ~paste(currency(Sum)), hoverinfo = "text", hoveron = "points",
#
#           line = list(shape = "hv", width = .5, color = "#3182bd")) %>%
#
#   add_trace(y = ~Avg, mode = "lines", hoverinfo = "none",
#             line = list(shape = "spline", color = "red", width = 1.5)) %>%
#   layout(
#     font = list(size = 11),
#     showlegend = FALSE,
#     hovermode = "x",
#     title = ~paste("Expense:", D_Types[[7]]),
#     xaxis = list(title = "",
#                  showgrid = TRUE,
#                  showline = FALSE,
#                  autotick = TRUE,
#                  zeroline = TRUE),
#
#     yaxis = list(title = "",
#                  showgrid = TRUE,
#                  showticklabels = TRUE,
#                  showline = TRUE,
#                  autotick = TRUE,
#                  ticks = "outside",
#                  tickwidth = 1,
#                  ticklen = 4,
#                  zeroline = FALSE,
#                  tozero = TRUE)
#   )




# | Plot - Function ---------------------------------------------------------


fx_type_plot <- function(data, facet = NULL) {


  data %>%

    plot_ly(
      x = ~ Date,
      y = ~ Sum,
      type = "scatter",
      mode = "lines",
      text = ~ paste(currency(Sum)),
      hoverinfo = "text",
      hoveron = "points",
      line = list(shape = "hv",
                  width = .25,
                  color = "black")) %>%

    add_trace(
      y = ~ Avg,
      mode = "lines",
      hoverinfo = "none",
      line = list(shape = "spline",
                  color = "red",
                  width = 1.5)) %>%
    layout(
      font = list(size = 11),
      showlegend = FALSE,
      hovermode = "x",
      # title = ~sprintf("Expense: %s", facet),
      xaxis = list(title = "",
                   showgrid = TRUE,
                   showline = FALSE,
                   autotick = TRUE,
                   zeroline = TRUE),

      yaxis = list(title = "",
                   showgrid = TRUE,
                   showticklabels = TRUE,
                   showline = TRUE,
                   autotick = TRUE,
                   ticks = "outside",
                   tickwidth = 1,
                   ticklen = 4,
                   zeroline = FALSE,
                   tozero = TRUE)
    )

}




plot_type_nest <-
  left_join(DW %>% group_by(Type) %>% nest(.key = "Week"),
            DM %>% group_by(Type) %>% nest(.key = "Month")) %>%
  mutate(Week = map(Week, fx_type_plot),
         Month = map(Month, fx_type_plot)) %>%
  gather(Period, Plot, Week:Month) %>%
  unite(Type_Plot, Period, Type) %>%
  deframe()




# | Plotly - Savings ------------------------------------------------------

Savings_Cash <-
  Table_Years %>%
  group_by(Year) %>%
  spread(Category, Sum) %>%
  transmute(Amount = Income - Debt - Expense - Savings,
            Type = "Cash")


Savings_Income <-
  Narrow_D %>%
  filter(Category %in% c("Income", "Savings")) %>%
  group_by(Year, Category, Type) %>%
  summarise(Amount = sum(Amount)) %>%
  mutate(Type = str_replace(Type, " - Income", "")) %>%
  ungroup() %>%
  mutate_at(1:2, factor) %>%
  filter(Category != "Income") %>%
  select(-Category) %>%
  bind_rows(Savings_Cash)

P_Savings <-
  Savings_Income %>%
  plot_ly(
    x = ~ Year,
    y = ~ Amount,
    color = ~ fct_relevel(Type, c("HSA", "IRA", "401k", "Cash")),
    colors = c("#41ab5d", "#78c679", "#addd8e", "#f7fcb9"),
    type = "bar",
    hoverinfo = "text",
    text = ~paste(Type, currency(Amount), sep = ": "),
    marker = list(line = list(color = "black", width = 1))) %>%
    layout(barmode = "relative",
           xaxis = list(title = ""),
           yaxis = list(title = ""))
