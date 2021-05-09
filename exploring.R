# check for missing values
sum(is.na(joined))

# plot NRtg against W using a scatterplot
ggplot(data = joined, aes(x = NRtg, y = W)) +
  geom_point()

# plot NRtg from worst to best using a bar graph
ggplot(data = joined, aes(x = reorder(Team, NRtg), y = NRtg)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8))

# plot distribution of ORtg using a histogram
joined %>%
  ggplot(aes(x = ORtg)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", colour = "black")

# plot FT against ORtg using a scatterplot
ggplot(data = joined, aes(x = FT, y = ORtg)) +
  geom_point()

# plot P2 against ORtg using a scatterplot
ggplot(data = joined, aes(x = P2, y = ORtg)) +
  geom_point()

# plot P3 against ORtg using a scatterplot
ggplot(data = joined, aes(x = P3, y = ORtg)) +
  geom_point()

# plot FT against ORtg and account for FTA, save into a new object
FTA_plot <- ggplot(data = joined, aes(x = FT, y = ORtg, label = Team)) +
  geom_point(aes(colour = FTA, size = FTA)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(FTA_plot)

# plot P2 against ORtg and account for P2A, save into a new object
P2A_plot <- ggplot(data = joined, aes(x = P2, y = ORtg, label = Team)) +
  geom_point(aes(colour = P2A, size = P2A)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(P2A_plot)

# plot P3 against ORtg and account for P3A, save into a new object
P3A_plot <- ggplot(data = joined, aes(x = P3, y = ORtg, label = Team)) +
  geom_point(aes(colour = P3A, size = P3A)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(P3A_plot)

# plot FT against ORtg and account for AST, save into a new object
FT_AST_plot <- ggplot(data = joined, aes(x = FT, y = ORtg, label = Team)) +
  geom_point(aes(colour = AST, size = AST)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(FT_AST_plot)

# plot P2 against ORtg and account for AST, save into a new object
P2_AST_plot <- ggplot(data = joined, aes(x = P2, y = ORtg, label = Team)) +
  geom_point(aes(colour = AST, size = AST)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(P2_AST_plot)

# plot P3 against ORtg and account for AST, save into a new object
P3_AST_plot <- ggplot(data = joined, aes(x = P3, y = ORtg, label = Team)) +
  geom_point(aes(colour = AST, size = AST)) +
  scale_colour_gradient(low = "red", high = "green")
# view the new plot in an interactive way
ggplotly(P3_AST_plot)

# label teams as either "above" or "below" average based on TSp
joined <- joined %>%
  mutate(TSp_avg = if_else(TSp > mean(TSp), "above_avg", "below_avg"))

# view TSp_avg in a box plot
ggplot(data = joined, aes(x = TSp_avg, y = ORtg)) +
  geom_boxplot(aes(fill = TSp_avg))

# use a facet wrap to view the distribution of each level of the TSp_avg variable
ggplot(data = joined, aes(x = ORtg, fill = TSp_avg)) +
  geom_histogram(colour = "black", binwidth = 2) +
  facet_wrap(~TSp_avg, nrow = 2) +
  theme(legend.position = "none")

# view the relationship between TSp and ORtg using a scatter plot
ggplot(data = joined, aes(x = TSp, y = ORtg)) +
  geom_point(colour = "dodgerblue", size = 2) +
  geom_smooth(method = lm, colour = "magenta")


