# Data Handling - Cost of Living Report

# Packages 
library(tidyverse)
library(readxl)
library(reshape)

# Data Loading 
coicop_2020_cleaned <- read_excel("data/coicop_A6_cleaned_2020.xlsx", 
                                  col_types = c("text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric"))

coicop_2021_cleaned <- read_excel("data/coicop_A6_cleaned_2021.xlsx", 
                                col_types = c("text", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))


# Split COICOP Data into hierarchies 

coicop_1_2020 <- coicop_2020_cleaned[1:42,]
coicop_2_2020 <- coicop_2020_cleaned[43:51,]
coicop_3_2020 <- coicop_2020_cleaned[52:65,]
coicop_4_2020 <- coicop_2020_cleaned[66:77,]
coicop_5_2020 <- coicop_2020_cleaned[78:89,]
coicop_6_2020 <- coicop_2020_cleaned[90:94,]
coicop_7_2020 <- coicop_2020_cleaned[95:109,]
coicop_8_2020 <- coicop_2020_cleaned[110:115,]
coicop_9_2020 <- coicop_2020_cleaned[116:142,]
coicop_10_2020 <- coicop_2020_cleaned[143:145,]
coicop_11_2020 <- coicop_2020_cleaned[146:156,]
coicop_12_2020 <- coicop_2020_cleaned[157:174,]
coicop_13_2020 <- coicop_2020_cleaned[176:183,]
coicop_14_2020 <- coicop_2020_cleaned[185:193,]
coicop_total_2020 <- coicop_2020_cleaned[c(184, 175, 
                                           1, 43, 52, 66, 78, 90, 95, 110, 116, 
                                           143, 146, 157, 176, 185),]

coicop_1_2021 <- coicop_2021_cleaned[1:42,]
coicop_2_2021 <- coicop_2021_cleaned[43:51,]
coicop_3_2021 <- coicop_2021_cleaned[52:65,]
coicop_4_2021 <- coicop_2021_cleaned[66:77,]
coicop_5_2021 <- coicop_2021_cleaned[78:89,]
coicop_6_2021 <- coicop_2021_cleaned[90:94,]
coicop_7_2021 <- coicop_2021_cleaned[95:109,]
coicop_8_2021 <- coicop_2021_cleaned[110:115,]
coicop_9_2021 <- coicop_2021_cleaned[116:142,]
coicop_10_2021 <- coicop_2021_cleaned[143:145,]
coicop_11_2021 <- coicop_2021_cleaned[146:156,]
coicop_12_2021 <- coicop_2021_cleaned[157:174,]
coicop_13_2021 <- coicop_2021_cleaned[176:183,]
coicop_14_2021 <- coicop_2021_cleaned[185:193,]
coicop_total_2021 <- coicop_2021_cleaned[c(184, 175, 
                                    1, 43, 52, 66, 78, 90, 95, 110, 116, 
                                    143, 146, 157, 176, 185),]

# Calculate proportional representation of each COICOP area

# Array creation for coicop dataframes 
coicop_list <- c("coicop_1_2020", "coicop_2_2020", "coicop_3_2020", "coicop_4_2020", "coicop_5_2020",
                 "coicop_6_2020", "coicop_7_2020", "coicop_8_2020", "coicop_9_2020", "coicop_10_2020",
                 "coicop_11_2020", "coicop_12_2020", "coicop_13_2020", "coicop_14_2020", "coicop_total_2020",
                 "coicop_1_2021", "coicop_2_2021", "coicop_3_2021", "coicop_4_2021", "coicop_5_2021",
                 "coicop_6_2021", "coicop_7_2021", "coicop_8_2021", "coicop_9_2021", "coicop_10_2021",
                 "coicop_11_2021", "coicop_12_2021", "coicop_13_2021", "coicop_14_2021", "coicop_total_2021")



# Empty List
coicop_grouped <- list()

# Mutate Proportational representation 
for(i in 1:length(coicop_list)){
  temp_coicop <- get(coicop_list[i])
  temp_coicop <- temp_coicop %>%
    mutate(Lowest_10_prop = Lowest_10/Lowest_10[1]) %>%
    mutate(Second_10_prop = Second_10/Second_10[1]) %>%
    mutate(Third_10_prop = Third_10/Third_10[1]) %>%
    mutate(Fourth_10_prop = Fourth_10/Fourth_10[1]) %>%
    mutate(Fifth_10_prop = Fifth_10/Fifth_10[1]) %>%
    mutate(Sixth_10_prop = Sixth_10/Sixth_10[1]) %>%
    mutate(Seventh_10_prop = Seventh_10/Seventh_10[1]) %>%
    mutate(Eighth_10_prop = Eighth_10/Eighth_10[1]) %>%
    mutate(Ninth_10_prop = Ninth_10/Ninth_10[1]) %>%
    mutate(Highest_10_prop = Highest_10/Highest_10[1]) %>%
    mutate(All_household_prop = All_household/All_household[1])
  
  coicop_grouped[[i]] <- temp_coicop
}

coicop_total_2020 <- coicop_grouped[[15]]
coicop_total_2021 <- coicop_grouped[[30]]

## Join Total Datasets

coicop_total_joined <- merge(by = c("COPI", "Commodity"),
                             x = coicop_total_2020, y = coicop_total_2021)
coicop_total_joined[[1,1]] <- "0.0.0"
coicop_total_joined[[2,1]] <- "0.0.1"
colnames(coicop_total_joined) <- c("COICOP", "Commodity", 
                                   "Lowest_10_2020", "Second_10_2020", "Third_10_2020",
                                   "Fourth_10_2020", "Fifth_10_2020", "Sixth_10_2020",
                                   "Seventh_10_2020", "Eighth_10_2020", "Ninth_10_2020",
                                   "Highest_10_2020", "ALl_household_2020", 
                                   "Lowest_10_prop_2020", "Second_10_prop_2020", "Third_10_prop_2020",
                                   "Fourth_10_prop_2020", "Fifth_10_prop_2020", "Sixth_10_prop_2020",
                                   "Seventh_10_prop_2020", "Eighth_10_prop_2020", "Ninth_10_prop_2020",
                                   "Highest_10_prop_2020", "All_household_prop_2020",
                                   "Lowest_10_2021", "Second_10_2021", "Third_10_2021",
                                   "Fourth_10_2021", "Fifth_10_2021", "Sixth_10_2021",
                                   "Seventh_10_2021", "Eighth_10_2021", "Ninth_10_2021",
                                   "Highest_10_2021", "ALl_household_2021", 
                                   "Lowest_10_prop_2021", "Second_10_prop_2021", "Third_10_prop_2021",
                                   "Fourth_10_prop_2021", "Fifth_10_prop_2021", "Sixth_10_prop_2021",
                                   "Seventh_10_prop_2021", "Eighth_10_prop_2021", "Ninth_10_prop_2021",
                                   "Highest_10_prop_2021", "All_household_prop_2021")

# Remove 0.0.0, 0.0.1 & 14.0.0
coicop_total_joined <- coicop_total_joined[c(3:7,9:16),]


# Narrow Data to examine only the Highest and Lowest 10%
coicop_highlow <- coicop_total_joined[,c(1, 2, 14, 36, 23, 45)]
coicop_highlow <- reshape::melt(coicop_highlow, id = c("COICOP", "Commodity"))

coicop_highlow$COICOP <- factor(coicop_highlow$COICOP,
                                levels = c("1.0.0", "2.0.0",   "3.0.0",   "4.0.0",   
                                           "5.0.0", "6.0.0",   "7.0.0",   "8.0.0",   
                                           "9.0.0", "10.0.0",  "11.0.0",  "12.0.0",  
                                           "13.0.0"))
coicop_highlow$variable <- factor(coicop_highlow$variable)

# Create Plot 

coicophighlow_plot <- ggplot(data = coicop_highlow) + 
      geom_bar(mapping = aes(x = COICOP, 
                             y = value, 
                             fill = variable), 
               stat = "identity",
               position = "dodge") + 
      labs(y = "Propotion of Total Household Expenditure", 
           x = "Commodity and Service Areas",
           fill = "Household Decile by Year", 
           caption = "Figure 1: Comparison of proportional household expenditure for the lowest and highest decile of households") + 
      scale_y_continuous(limits = c(0, 0.3), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) + 
      scale_fill_manual(labels = c("Lowest decile, 2020", "Lowest decile, 2021", 
                                   "Highest decile, 2020", "Highest decile, 2021"),
                        values = c("#12436D", "#28A197","#801650", "#F46A25")) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.8), 
            legend.position = "bottom") + 
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))


## 
