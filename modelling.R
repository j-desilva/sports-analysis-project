# normalize W, TSp, AST and ORB
joined <- joined %>%
  mutate(win_percentage = W / G * 100,
         TSp_norm = TSp * 100,
         AST_per_game = AST / G,
         ORB_per_game = ORB / G)


# plot the relationship between normalized TSp and W
ggplot(data = joined, aes(x = TSp_norm, y = win_percentage)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = lm, colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed") +  # add a horizontal dashed line halfway
  ylim(0, 100)  # set the y axis limits between 0 and 100


# check the correlation coefficient of TSp_norm and win_percentage 
with(joined, cor(x = TSp_norm, y = win_percentage))


# create a multiple regression and label it `fit` (to used for further analysis)
fit <- lm(win_percentage ~ TSp_norm + AST_per_game + ORB_per_game, data = joined)
tidy(fit, conf.int = TRUE)


# view a summary of our `fit` model
summary(fit)


# check for independence of our `fit` model. Use `install.packages(car)` if this is not already installed, then run the following code
car::durbinWatsonTest(fit)


# check for outliers in our `fit` model
std_res <- rstandard(fit)
points <- 1:length(std_res)

# view outliers in a scatterplot
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")


# check for high leverage points in our model
hats <- hatvalues(fit) 

# view points of possible high leverage in a scatter plot
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

# create labels for points greater than 0.25
hat_labels <- if_else(hats > 0.25, paste(points), "")

# view hat_labels on the same scatter plot
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text_repel(aes(label = hat_labels))


# check for points of high influence
cook <- cooks.distance(fit)

# view `cook` in a scatter plot
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

# label potential high influence points above 0.075
cook_labels <- if_else(cook > 0.075, paste(points), "")

# view cook_labels on the same scatter plot as in Step 23.
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text_repel(aes(label = cook_labels))


# specify the outliers 
outliers <- c(2, 7, 9, 21, 26, 28, 30)

# filter the joined data to eliminate the outliers
joined_filtered <- joined %>%
  filter(!cook_labels %in% outliers)

# run a new regression labeled `fit2`
fit2 <- lm(win_percentage ~ TSp_norm + AST_per_game + ORB_per_game, data = joined_filtered)
tidy(fit2, conf.int = TRUE)

# provide summary statistics for `fit2`
summary(fit2)


# check for homoscedasticity
res <- residuals(fit)
fitted <- predict(fit)

# view potential homoscedasticity in a scatter plot
ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")


# using a histogram view the distribution of residuals 
ggplot(data = NULL, aes(x = res)) +
  geom_histogram(colour = "black", fill = "dodgerblue", binwidth = 6)


# using a stat_qq plot view the distribution of the residuals
ggplot(data = NULL, aes(sample = res)) +
  stat_qq() + stat_qq_line()


# use install.packages("car") if not already installed
# check for multicolinearity
car::vif(fit)

# check for linearity
car::avPlots(fit)


