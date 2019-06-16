ggplot(train_rpart)


active_flights %>%
  summarize(
    sd
  )
ggplot(aes(x=DEP_DELAY)) + 
  geom_histogram(binwidth = 1)

active_flights %>%
  filter(DEP_DELAY > 0) %>%
  ggplot(aes(x=DEP_DELAY)) + 
  geom_histogram(binwidth = 5)

active_flights %>%
  filter(DEP_DELAY > 0) %>%
  ggplot(aes(x=DEP_DELAY, y=DEP_TIME_BLK)) + 
  geom_point()

active_flights %>%
  filter(DEP_DELAY > 0) %>%
  ggplot(aes(x=DEP_TIME_BLK)) + 
  geom_histogram(stat="count")


active_flights %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(
    n = n(),
    n_delayed = sum(DEP_DELAY > 0)
  ) %>%
  ggplot(aes(x=DAY_OF_WEEK)) + 
  geom_point(aes(y = n)) +
  geom_point(aes(y=n_delayed))

active_flights %>%
  filter(DEP_DELAY != 0) %>%
  ggplot(aes(x=DAY_OF_WEEK, y=DEP_DELAY)) + 
  geom_boxplot()

train_set %>% 
  mutate(y_hat = predict(cart_fit)) %>% 
  ggplot() +
  geom_point(aes(y_hat, DEP_DELAY)) +
  geom_step(aes(y_hat, DEP_DELAY), col="red")

active_flights %>%
  group_by(DAY_OF_WEEK) %>%
  summarize(
    avg = mean(DEP_DELAY),
    n = n()
  ) %>%
  ggplot(aes(DAY_OF_WEEK)) + 
  geom_bar()

active_flights %>%
  ggplot(aes(DEP_DELAY_GROUP)) + 
  geom_bar()

active_flights %>%
  ggplot(aes(DEP_DELAY_GROUP)) + 
  geom_bar()

sum(is.na(train_df$y))



library(rpart.plot)
#model = rpart(score ~., data = dataset)

rpart.plot(cart_fit)
rpart.rules(cart_fit)
prp(cart_fit, faclen = 2)


