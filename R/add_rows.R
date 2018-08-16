
# Add Rows to Table -------------------------------------------------------
# Set year and month
# Type string with shortcuts and commas
# Hit enter to build a new table
# Enter new string to add to new table
# Hit final button to bind_cols with original table and write_csv to save?
# Need to migrate mom's table tracking to different location




# d_expense %>% distinct(Type) %>% pull()

type_shortcut <-
  tribble(~Type, ~Shortcut,
          "Groceries", "gro",
          "Income", "inc",
          "Rent", "rnt",
          "Utilities", "utl",
          "Entertainment", "ent",
          "Fuel", "fl",
          "Drinks", "drk",
          "Misc", "msc",
          "Student Loan", "sl",
          "HSA - Income", "hsa",
          "Dining", "dn",
          "Subscription", "sub",
          "Healthcare", "hc",
          "Travel", "tr",
          "Auto", "aut",
          "Fees", "fee",
          "IRA", "ira",
          "Debt", "dbt",
          "401k - Income", "401") %>%
  arrange(Type)


set_date <- "2018-08"
set_string <- "15, gro, Meijer, 20.50"

set_string <- "15, gro, Meijer, 20.50/ 15, gro, Meijer, 20.50"

# mod 3 works
set_string %>% str_count(",")
set_string %>% str_replace_all(", ", ",")

tibble(i = set_string) %>%
  separate_rows(i, sep = "/") %>%
  separate(i, sep = ",",
           into = c("Date", "Shortcut", "Description", "Amount")) %>%
  mutate_all(str_trim) %>%
  mutate(Date = str_c(set_date, Date, sep = "-") %>% as.Date(),
         Amount = Amount %>% as.numeric()) %>%
  left_join(type_shortcut, by = "Shortcut") %>%
  select(Date, Type, Description, Amount)





str_count(set_string, ",")

tibble(i = set_string) %>%
  separate(i,
           c("Date", "Shortcut", "Description", "Amount"),
           sep = ", ")

set_string %>%
  str_split_fixed(", ", n = 4) %>%
  as_tibble() %>%
  set_names(c("Date", "Shortcut", "Description", "Amount")) %>%
  bind_cols() %>%
