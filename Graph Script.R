
#Graph Creation
Graph1 <- DataChange %>%
  select(Country, Year, ChangeinWage, Wage) %>%
  pivot_longer(cols = c(ChangeinWage, Wage),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year),
         Metric = factor(Metric, levels = c("Wage", "ChangeinWage")))  
Wage <- ggplot(Graph1, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Wage for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph1$Year))


Graph2 <- DataChange %>%
  select(Country, Year, Inflation) %>%
  pivot_longer(cols = c(Inflation),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year)) 
Inflation <- ggplot(Graph2, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Inflation for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph2$Year))


Graph3 <- DataChange %>%
  select(Country, Year, Firms, ChangeinFirms) %>%
  pivot_longer(cols = c(Firms, ChangeinFirms),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year))
Firms <- ggplot(Graph3, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Firms for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph3$Year))


Graph4 <- DataChange %>%
  select(Country, Year, Employees, ChangeinEmployees, Population) %>%
  mutate(Population = Population/1000000) %>%
  pivot_longer(cols = c(Employees, ChangeinEmployees, Population),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year))
Employees <- ggplot(Graph4, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Employees for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph4$Year))


Graph5 <- DataChange %>%
  select(Country, Year, Turnover, ChangeinTurnover) %>%
  pivot_longer(cols = c(Turnover, ChangeinTurnover),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year))
Turnover <- ggplot(Graph5, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Turnover for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph5$Year))


Graph6 <- DataChange %>%
  select(Country, Year, CapitaTurnover, ChangeinCapitaTurnover) %>%
  pivot_longer(cols = c(CapitaTurnover, ChangeinCapitaTurnover),
               names_to = "Metric",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year))
CapitaTurnover <- ggplot(Graph6, aes(x = Year, y = Value, color = Country)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~Metric, scales = "free_y", nrow = 1) +
  labs(title = "Turnover per Capita for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       y = "Value",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("Denmark" = "red", "Germany" = "blue")) +
  scale_x_continuous(breaks = unique(Graph6$Year))


Graph7 <- Size %>%
  mutate(Year = as.numeric(Year))
Size1 <- ggplot(Graph7, aes(x = Year, y = FirmsBySize, fill = Size)) +
  geom_col(position = "stack") +
  facet_wrap(~Country, nrow = 1, scales = "free_y") +
  labs(title = "Firms by Size for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
      y = "Total Firms",
      fill = "Size Category") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(breaks = unique(Graph7$Year))


Graph8 <- SizeChange %>%
  select(Country, Year, Size, ChangeinFirmsBySize) %>%
  mutate(Year = as.numeric(Year))
Size2 <- ggplot(Graph8, aes(x = Year, y = ChangeinFirmsBySize, color = Size, group = Size)) +
  geom_line(linewidth = 0.2) +
  geom_point() +
  facet_wrap(~Country, nrow = 1, scales = "free_y") +
  labs(title = "Change Firms by Size for Denmark and Germany in the Pharmaceutical Industry (09 - 21)",
       x = "Year",
       y = "Change in Number of Firms",
       color = "Firm Size") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(Graph8$Year))


#Graph and Interactive Graph Saving
ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Wage.jpg", 
       plot = Wage,
       width = 12, height = 6, dpi = 300)
IWage <- ggplotly(Wage)
saveWidget(widget = IWage,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Wage.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Inflation.jpg", 
       plot = Inflation,
       width = 12, height = 6, dpi = 300)
IInflation <- ggplotly(Inflation)
saveWidget(widget = IInflation,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Inflation.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Firms.jpg", 
       plot = Firms,
       width = 12, height = 4, dpi = 300)
IFirms <- ggplotly(Firms)
saveWidget(widget = IFirms,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Firms.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Employees.jpg", 
       plot = Employees,
       width = 16, height = 6, dpi = 300)
IEmployees <- ggplotly(Employees)
saveWidget(widget = IEmployees,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Employees.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Turnover.jpg", 
       plot = Turnover,
       width = 12, height = 6, dpi = 300)
ITurnover <- ggplotly(Turnover)
saveWidget(widget = ITurnover,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Turnover.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/CapitaTurnover.jpg", 
       plot = CapitaTurnover,
       width = 12, height = 6, dpi = 300)
ICapitaTurnover <- ggplotly(CapitaTurnover)
saveWidget(widget = ICapitaTurnover,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/CapitaTurnover.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Size.jpg", 
       plot = Size1,
       width = 12, height = 6, dpi = 300)
ISize1 <- ggplotly(Size1)
saveWidget(widget = ISize1,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Size1.html",
           selfcontained = TRUE)

ggsave("C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/SizeChange.jpg", 
       plot = Size2,
       width = 12, height = 8, dpi = 300)
ISize2 <- ggplotly(Size2)
saveWidget(widget = ISize2,
           file = "C:/Users/Usuario/Documents/2nD Desktop/BE Project - VS Code/Data/Size2.html",
           selfcontained = TRUE)


