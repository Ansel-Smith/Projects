# David A. Smith - The University of Alabama
# Last Edited December 5, 2024

library("tidyverse")
library("readxl")
library("plotly")
library("gt")

collection <- read_excel("university_of_alabama_enslavement_financial_transactions_1828_to_1864.xlsx", sheet="Tidy Data") #CHANGE
collection <- as_tibble(collection)

# function to calculate the mode of a category
my_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Table 1
# Grand Summary
#===============================================================================
amount_summary <- collection %>%
  summarize(`Spent` = sum(amount_today, na.rm=TRUE),
            `Duration of Labor (days)` = sum(duration, na.rm=TRUE),
            `Enslaved` = length(unique(as.vector((enslaved)))),
            `Payees` = length(unique(as.vector((payee)))),
            `Payers` = length(unique(as.vector((received_of)))),
            `Certifiers` = length(unique(as.vector((certifier)))),
            `Recipients` = length(unique(as.vector((to)))),
            `Senders` = length(unique(as.vector((from)))),
            `Documents` = n(),
            `Document Types` = length(unique(as.vector((document_type)))),
            `Purposes of Transaction` = length(unique(as.vector((purpose))))
  )

grand_summary_table <- 
  gt(amount_summary) %>% 
  tab_options(
    summary_row.background.color = "grey",
    grand_summary_row.background.color = "black"
  ) %>%
  tab_header(
    title = "Grand Summary"
  ) %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Day") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Sum of transaction amounts, duration of labor of enslaved individuals, count unique agents involved in transactions, count of source documents, types of documents, and purposes of each transaction."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(
    columns=c(`Spent`)
  ) %>%
  fmt_number(columns=`Duration of Labor (days)`, decimals=2)%>%
  cols_align(align = "center", columns = everything()) %>%
  gt_split(col_slice_at = `Certifiers`)

gtsave(grand_summary_table, "grand_summary_table.html", path="./tables")
#===============================================================================

#Table 2
# Transaction Amounts Spent by the University of Alabama - Adjusted for Inflation
#===============================================================================

amount_summary <- collection %>% 
  group_by(year) %>% 
  summarize(`M`=mean(amount_today, na.rm=TRUE),
            `SD`=sd(amount_today, na.rm=TRUE), 
            `Min`=min(amount_today, na.rm=TRUE),
            `Max`=max(amount_today, na.rm=TRUE),
            `Sum` = sum(amount_today, na.rm=TRUE))

amount_year_table <- 
  gt(amount_summary, rowname_col="year") %>% 
  tab_header(
    title = "Transaction Amounts Spent by the University of Alabama - Adjusted for Inflation") %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>%
  tab_source_note(
    source_note = "Mean, Standard Deviation, Minimum Value, Maximum Value, and Sum Total of transaction amounts by year and decade. 'NA' values in Standard Deviation indicate the value is not applicable because only one transaction value occurs for the given year."
  ) %>%
  tab_stubhead(label = "Year") %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  fmt_currency(
    columns=everything()
  ) %>%
  tab_row_group(
    label = "1820s-1830s",
    rows = 1:6
  ) %>%
  tab_row_group(
    label = "1840s",
    rows = 7:16
  ) %>%
  tab_row_group(
    label = "1850s",
    rows = 17:25
  ) %>%
  tab_row_group(
    label = "1860s",
    rows = 26:27
  ) %>%
  summary_rows(
    groups = everything(),
    columns = `Sum`,
    fns = list(
      sum = "sum"
    ),
    fmt = list(~ fmt_currency(., decimals = 2))
  ) %>%
  grand_summary_rows(
    columns = `Sum`,
    fns = list(
      sum = "sum"
    ),
    fmt = list(~ fmt_currency(., decimals = 2))
  )
amount_year_table
gtsave(amount_year_table, "amount_year_table.html", path="./tables")
#===============================================================================

# Table 3
# Transaction Amounts Received by Payees
#===============================================================================
# limit rows
subcollection <- collection %>% 
  filter(document_type != 'Order for Appropriation')

payee_summary <- subcollection %>%
  group_by(payee) %>% 
  summarize(`n` = n(),
            `M`=mean(amount_today, na.rm=TRUE),
            `Min`=min(amount_today, na.rm=TRUE),
            `Max`=max(amount_today, na.rm=TRUE),
            `Sum`=sum(amount_today, na.rm=TRUE),
            `Mode Payer`= my_mode(received_of),
            `Mode Certifier`=my_mode(certifier),
            `Mode Enslaved` = my_mode(enslaved)
  ) %>% 
  arrange(desc(`Sum`))

payee_amount_table <- 
  gt(payee_summary, rowname_col="payee") %>% 
  tab_header(
    title = "Transaction Amounts Received by Payees"
  ) %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Payee") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Count, Mean, Minimum Value, Maximum Value, and Sum Total of transaction amounts received by payees from the University of Alabama for the hire, board, and purchase of enslaved individuals. 'NA' values in Standard Deviation indicate the value is not applicable because only one transaction value occurs for the given year, along with the most common certifier, payer, and enslaved individuals involved with each payee. 'NA' values among Agent data indicates missing or non-applicable data."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  tab_source_note(
    source_note = "'GW C[oply]' is the name assigned to an unreadable name"
  ) %>%
  tab_source_note(
    source_note = "'NA' value in 'Payee' indicates transactions without an explicit payee mentioned."
  ) %>%
  tab_row_group(
    label = "Top 10",
    rows = 1:10
  ) %>%
  fmt_currency(
    columns=c(`M`,`Min`,`Max`,`Sum`)
  ) %>%
  summary_rows(
    groups = everything(),
    columns = c(`Sum`, `n`),
    fns = list(
      sum = "sum"
    ),
    fmt = list(~ fmt_currency(.,columns=`Sum`, decimals = 2))
  ) %>%
  tab_spanner(label="Agents", columns=c(`Mode Certifier`, `Mode Payer`, `Mode Enslaved`)
  )
gtsave(payee_amount_table, "payee_amount_table.html", path="./tables")
#===============================================================================

# Table 4
# Transaction Amounts Reviewed by Certifiers
#===============================================================================
certifier_summary <- collection %>%
  group_by(certifier) %>% 
  summarize(`n` = n(),
            `M`=mean(amount_today, na.rm=TRUE),
            `Min`=min(amount_today, na.rm=TRUE),
            `Max`=max(amount_today, na.rm=TRUE),
            `Sum`= sum(amount_today, na.rm=TRUE),
            `Mode Payee` = my_mode(payee),
            #`mcp_c = max(tabulate(match(payee, unique(payee)))),
            `Mode Payer`= my_mode(received_of),
            #mcro_count = max(tabulate(match(received_of, unique(received_of)))),
            `Mode Enslaved` = my_mode(enslaved)
            #mce_count = max(tabulate(match(enslaved, unique(enslaved))))
  ) %>% 
  arrange(desc(`Sum`))
certifier_summary

certifier_amount_table <- 
  gt(certifier_summary, rowname_col="certifier") %>% 
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Certifier") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_header(
    title = "Transaction Amounts Reviewed by Certifiers"
  ) %>% 
  tab_row_group(
    label = "Top 10",
    rows = 1:10
  ) %>%
  fmt_currency(
    columns=c(`M`, `Min`, `Max`, `Sum`)
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="left")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_source_note(
    source_note = "Count, Mean, Minimum Value, Maximum Value, and Sum Total of transaction amounts reviewed by certifiers of transactions from the University of Alabama for the hire, board, and purchase of enslaved individuals, along with the most common payee, payer, and enslaved individuals involved with each certifier. 'NA' values among Agent data indicates missing or non-applicable data."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(
    columns=c(`M`, `Min`,`Max`, `Sum`)
  ) %>%
  summary_rows(
    groups = everything(),
    columns = c(`Sum`, `n`),
    fns = list(
      sum = "sum"
    ),
    fmt = list(~ fmt_currency(.,columns=`Sum`, decimals = 2))
  ) %>%
  tab_spanner(label="Agents", columns=c(`Mode Payee`, `Mode Payer`, `Mode Enslaved`)
  )
gtsave(certifier_amount_table, "certifier_amount_table.html", path="./tables")
#===============================================================================

# Table 5
# Transaction Amounts Spent by Day of the Month
#===============================================================================
day_summary <- collection %>%
  group_by(day) %>% 
  summarize(`n` = n(),
            `Sum Duration` = sum(duration, na.rm=TRUE),
            `Sum` = sum(amount_today, na.rm=TRUE),
            `Mode Enslaved` = my_mode(enslaved),
            `Mode Payee` = my_mode(payee)
  ) %>% 
  arrange(as.numeric(day))
day_summary

day_amount_table <- 
  gt(day_summary, rowname_col="day") %>% 
  tab_header(
    title = "Transaction Amounts Spent by Day of the Month"
  ) %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Day") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Count of transactions, most frequent occuring enslaved individuals, most frequent occuring payees, Sum Total of duration of labor of enslaved individuals in days, and Sum Total of transaction amounts paid by The University of Alabama organized by day of the month (1-31)."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(columns = c(`Sum`))

day_amount_table
gtsave(day_amount_table, "day_amount.html", path="./tables")
#===============================================================================

# Table 6
# Document Type
#===============================================================================
document_type_summary <- collection %>%
  group_by(document_type) %>% 
  summarize(`n` = n(),
            `Sum` = sum(amount_today),
            `Mode Payee` = my_mode(payee),
            `Mode Enslaved` = my_mode(enslaved),
            `Mode Payer` = my_mode(received_of),
            `Mode Recipient` = my_mode(to),
            `Mode Sender` = my_mode(from)
  ) %>%
  arrange(desc(`Sum`))
document_type_summary

documenttype_table <- 
  gt(document_type_summary, rowname_col="document_type") %>% 
  tab_options(
    summary_row.background.color = "grey"
  ) %>% 
  tab_header(
    title = "Document Type"
  ) %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Document Type") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Count of types of documents."
  )%>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(columns=`Sum`)

gtsave(documenttype_table, "documenttype_table.html", path="./tables")
#===============================================================================

# Table 7
# Duration of Enslaved Labor in Days
#===============================================================================
subcollection <- collection %>% drop_na(duration)

duration_summary <- subcollection %>%
  group_by(enslaved) %>% 
  summarize(`Sum` = sum(duration, na.rm=TRUE),
            `M` = mean(duration, na.rm=TRUE),
            `SD` = sd(duration, na.rm=TRUE),
            `Min` = min(duration, na.rm=TRUE),
            `Max` = max(duration, na.rm=TRUE)
  ) %>%
  arrange(desc(`Sum`))
duration_summary

enslaved_duration_table <- 
  gt(duration_summary, rowname_col="enslaved") %>% 
  tab_header(
    title = "Duration of Enslaved Labor in Days"
  ) %>% tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Enslaved") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Sum, mean, standard deviation (if more than one transaction), minimum, and maximum number of days worked by each enslaved individual. The duration of labor of groups of enslaved people are calculated per each member of the group. The groups arise from mentions of each enslaved individual in the same document together."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_number(
    columns = c(`M`, `SD`),
    decimals=2    
  ) %>%
  grand_summary_rows(
    columns = c(`Sum`),
    fns = list(
      sum = "sum",
      mean = "mean"
    ),
    formatter=fmt_number,
    decimals = 2
  ) %>%
  fmt_number(columns = everything(), decimals=2)

gtsave(enslaved_duration_table, "enslaved_duration_table.html", path="./tables")
#===============================================================================

# Table 8
# Amounts Associated with Enslaved Individual
#===============================================================================
subcollection <- collection %>% 
  filter(document_type != 'Order for Appropriation')

enslaved_summary <- subcollection %>%
  group_by(enslaved) %>% 
  summarize(`n` = n(),
            `M`=mean(amount_today, na.rm=TRUE),
            `Min`=min(amount_today, na.rm=TRUE),
            `Max`=max(amount_today, na.rm=TRUE),
            `Sum`=sum(amount_today, na.rm=TRUE),
            `Payee Mode` = my_mode(payee),
            `n Payee Mode` = max(tabulate(match(
              payee, unique(payee))))
  ) %>% 
  arrange(desc(`Sum`))

enslaved_amount_table <- 
  gt(enslaved_summary, rowname_col="enslaved") %>% 
  tab_options(
    summary_row.background.color = "grey"
  ) %>% 
  tab_header(
    title = "Amounts Associated with Enslaved Individuals") %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Enslaved") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Count, mean, minimum, maximum, sum, most frequently associated payee, and the count of occurrences of the payee for the transaction amounts spent on the labor, purchase, board, and other costs for enslaved individuals."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(
    columns=c(`M`, `Min`, `Max`, `Sum`)
  )

gtsave(enslaved_amount_table, "enslaved_amount_table.html", path="./tables")
#===============================================================================

# Table 9
# Transaction Amounts by Month
#===============================================================================
month_summary <- collection %>%
  group_by(month) %>% 
  summarize(`n` = n(),
            `Sum` = sum(amount_today, na.rm=TRUE),
            `Sum Duration` = sum(duration, na.rm=TRUE),
            `Mode Enslaved` = my_mode(enslaved),
            `Mode Payee` = my_mode(payee)
  ) %>% 
  arrange(as.numeric(month))

month_amount_table <- 
  gt(month_summary, rowname_col="month") %>% 
  tab_options(
    summary_row.background.color = "grey"
  ) %>% tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Month") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_header(
    title = "Transaction Amounts by Month"
  ) %>% 
  tab_source_note(
    source_note = "The count of documents, sum of transaction amounts, count of unique enslaved individuals and payees by month."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(columns = c(`Sum`))

month_amount_table
gtsave(month_amount_table, "month_amount_table.html", path="./tables")
#===============================================================================

# Table 10
# Purpose of Transactions
#===============================================================================
purpose_summary <- collection %>%
  group_by(purpose) %>% 
  summarize(`n` = n(),
            `Sum` = sum(amount_today, na.rm=TRUE),
            `Sum Duration` = sum(duration, na.rm=TRUE),
            `Payee Mode` = my_mode(payee),
            `Enslaved Mode` = my_mode(enslaved),
            `Payer Mode` = my_mode(received_of),
            `Recipient Mode` = my_mode(to),
            `Sender Mode` = my_mode(from)
  ) %>%
  arrange(desc(`Sum`))

purpose_table <- 
  gt(purpose_summary, rowname_col="purpose") %>% 
  tab_options(
    summary_row.background.color = "grey"
  ) %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Purpose") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_header(
    title = "Purpose of Transactions") %>% 
  tab_source_note(
    source_note = "The count, transaction amount sum, sum of duration of enslaved labor, and most common payee, enslaved individual, payer, recipient of payment, and transaction sender grouped by the purpose of each document."
  ) %>%
  tab_source_note(
    source_note="NA value denotes missing, unavailable, or non-applicable information"
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(columns = c(`Sum`)) %>%
  fmt_number(columns=`Sum Duration`, decimals=2)

gtsave(purpose_table, "purpose_table.html", path="./tables")

#===============================================================================

# Table 11
# Payers of Financial Transactions
#===============================================================================
receivedof_summary <- collection %>%
  group_by(received_of) %>% 
  summarize(`n` = n(),
            `M`=mean(amount_today, na.rm=TRUE),
            `Min`=min(amount_today, na.rm=TRUE),
            `Max`=max(amount_today, na.rm=TRUE),
            `Sum` = sum(amount_today, na.rm=TRUE)
  ) %>% 
  arrange(desc(`Sum`))

receivedof_amount_table <- 
  gt(receivedof_summary, rowname_col="received_of") %>% 
  tab_header(
    title = "Payers of Financial Transactions") %>% 
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Payer") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Mean transaction amount, minimum and maximum transaction amount, sum of transaction amounts, and count of transactions associated with payers of transaction amounts."
  ) %>%
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  fmt_currency(
    columns=c(`M`, `Min`, `Max`, `Sum`)
  )

gtsave(receivedof_amount_table, "receivedof_amount_table.html", path="./tables")
#===============================================================================

# Table 12
# Documents by Year
#===============================================================================
year_summary <- collection %>%
  group_by(year) %>% 
  summarize("n" = n(),
            "n Enslaved" = length(as.vector((unique(enslaved)))),
            "n Payee" = length(as.vector((unique(payee)))),
            "Sum Duration" = sum(duration, na.rm=TRUE)
  ) %>% 
  arrange(year)
year_summary

year_amount_table <- 
  gt(year_summary, rowname_col="year") %>% 
  tab_header(
    title = "Documents by Year"
  ) %>% tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style=list(cell_text(align="center")),
    locations = list(cells_body(), cells_stubhead())
  ) %>%
  tab_style(
    style=list(cell_text(style="italic", align="center")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style=list(cell_text(align="left")),
    locations = list(cells_stub(), cells_title())
  ) %>% 
  tab_stubhead(label = "Year") %>%
  opt_table_font(
    font = list(google_font(name = "Calibri")),
    size = 11
  ) %>%
  tab_source_note(
    source_note = "Count of documents, count of unique enslaved individuals and payees, and sum of duration of enslaved labor grouped by year."
  ) %>% 
  tab_source_note(
    source_note = "Consumer Price Index data collected from https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1800-"
  ) %>%
  grand_summary_rows(
    columns=c(`n`, `Sum Duration`),
    fns = list(sum="sum")
  ) %>%
  fmt_number(columns=`Sum Duration`, decimals=2)

gtsave(year_amount_table, "year_amount_table.html", path="./tables")
#===============================================================================