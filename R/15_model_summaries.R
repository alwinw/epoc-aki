# Summarise into various tables
# One for cr_gradient, one for percentage change
# Multivariable models for different outcomes
# Add a plot showing cr vs time and predictive value



ts <- tribble(
  ~"hour", ~"cr", ~"colour",
  0, 70, 1,
  1, 72, 1,
  3, 75, 1,
  4, 73, 1,
  5.8, 78, 1,
  5.8 + 8.7, 110, 2,
  5.8 + 25.6, 110, 2
) %>%
  mutate(hour = make_datetime(hour = hour, min = hour %% 1 * 60))

ggplot(ts, aes(hour, cr, colour = colour)) +
  geom_point()
