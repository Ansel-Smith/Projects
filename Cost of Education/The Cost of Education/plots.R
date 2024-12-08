# David A. Smith - The University of Alabama
# Last Edited December 5, 2024

# If you need to install these packages, run the below line once
#install.packages("tidyverse", "readxl", "extrafont", "gt", "plotly", "officer")

library("tidyverse")
library("readxl")
library("extrafont")
library("gt")
library("plotly")
library("officer")

# Run the program within the folder "The Cost of Education Data"
font_import() 

collection <- read_excel("university_of_alabama_enslavement_financial_transactions_1828_to_1864.xlsx",
                         sheet="Tidy Data")
collection <- as_tibble(collection)

# Figure 1
# Duration of Enslaved Labor vs Transaction Amount
#===============================================================================
duration_transaction_plot <- ggplot(data = collection, aes(duration, amount_today)) +
  geom_point() +
  scale_x_continuous(name = "Duration (days)",
                     limits=c(1,150))+
  scale_y_continuous(name = "Transaction Amount ($)",
                     limits=c(0,2500))+
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Duration of Enslaved Labor vs Transaction Amount"
  ) + theme(text=element_text(size = 11, family="Calibri"),
            plot.title = element_text(hjust=0, face="italic")
  )

ggplotly(duration_transaction_plot)
ggsave("amount_duration_plot.png", plot=duration_transaction_plot, width=6, height=4, path="./plots")
#===============================================================================

# Figure 2
# Duration of Enslaved Labor by Year
#===============================================================================
duration_year_plot <- ggplot(data = collection, aes(year, duration)) +
  geom_col() +
  scale_x_discrete(name = "Year", breaks = c(1820, 1830, 1840, 1850, 1860)) +
  scale_y_continuous(name = "Duration of Labor (days)",
                     position="left") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Duration of Enslaved Labor by Year"
  ) + theme(text=element_text(size = 11, family="Calibri"),
            plot.title = element_text(hjust=0, face="italic")
  )

ggplotly(duration_year_plot)
ggsave("duration_year_plot.png", plot=duration_year_plot, width=6, height=4, path="./plots")
#===============================================================================

# Figure 3
# Mention of Enslaved Individuals by Year
#===============================================================================
subcollection <- collection %>%  filter(enslaved == "William" |
                                       enslaved == "Arthur" |
                                       enslaved == "Moses" |
                                       enslaved == "Anderson" |
                                       enslaved =="Leroy" | 
                                       enslaved == "Cannon" |
                                       enslaved == "Sam" |
                                       enslaved == "Isaac" |
                                       enslaved == "Jack" |
                                       enslaved ==  "Armistead"
                                      )

enslaved_year_plot <- ggplot(data = subcollection, aes(year, enslaved)) +
  geom_count() +
  scale_x_discrete(name = "Year") +
  scale_y_discrete(name = "Enslaved Individuals") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Mention of Enslaved Individuals by Year") + 
  theme(text=element_text(size = 11, family="Calibri"),
        plot.title = element_text(hjust=0, face="italic")
  ) +
  labs(caption="The top 10 most frequently mentioned individuals are listed.The size of each point indicates the relative frequency of the mention of each enslaved individual in transactions.")

ggplotly(enslaved_year_plot)
ggsave("enslaved_year_plot.png", plot=enslaved_year_plot, width=10, height=4, path="./plots")
#===============================================================================

# Figure 4
# Payee vs Duration of Enslaved Labor
#===============================================================================
subcollection <- collection %>%
  group_by(payee) %>%
  arrange(desc(duration)) %>%
  head(n=20)

payee_duration_plot <- ggplot(data = subcollection, aes(payee, duration)) +
  geom_col() +
  scale_x_discrete(name = "Payee") +
  scale_y_continuous(name = "Duration of Labor (days)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Payee vs Duration of Enslaved Labor") +
  theme(text=element_text(size = 11, family="Calibri"),
        plot.title = element_text(hjust=0, face="italic")
  ) + labs(caption="Duration of enslaved labor associated with each payee. 10 highest paid payees are shown. The blank payee indicates unavailable data.")

ggplotly(payee_duration_plot)
ggsave("payee_duration_plot.png", plot=payee_duration_plot, width=8, height=8, path="./plots")
#===============================================================================

# Figure 5
# Top 10 Highest Paid Payees
#===============================================================================
subcollection <- collection %>% filter(payee == "A. B. Rucker" |
               payee == "Morgan Chambers & Co." |
               payee == "Isabel A. Pratt" |
               payee == "Lafayette M. Minor" |
               payee =="Frances P. Ashe" | 
               payee == "Benjamin Whitfield" |
               payee == "John P. Boyle" |
               payee == "George Benagh" |
               payee == "S. M. Stafford" |
               payee ==  "L.C. Garland"
              )

year_payee_plot <- ggplot(data = subcollection, aes(year, payee)) +
  geom_count() +
  scale_x_discrete(name = "Year") +
  scale_y_discrete(name = "Payee") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Frequency of Payees by Year") +
  theme(text=element_text(size = 11, family="Calibri"),
        plot.title = element_text(hjust=0, face="italic")
  ) + labs(caption = "Comparison of the 10 most frequently appearing payees.")

ggplotly(year_payee_plot)

ggsave("year_payee_plot.png", plot= year_payee_plot, width=6, height=4, path="./plots")
#===============================================================================