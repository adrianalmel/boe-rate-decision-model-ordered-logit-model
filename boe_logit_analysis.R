

library(tidyverse)
library(MASS)


##### 1. Data #####
model_df <- read_csv("mpc_model_data.csv", show_col_types = FALSE) %>%
  mutate(decision = factor(decision, levels = c("cut", "hold", "hike"), ordered = TRUE)) %>%
  drop_na(decision, inf_gap, services_cpi, unemp_chg_3m, gdp_growth,
          oil_chg_3m, bank_rate, market_exp) %>%
  mutate(zlb = as.integer(bank_rate <= 0.5))

##### 2. Final model #####
m_final <- polr(decision ~ inf_gap + unemp_chg_3m + market_exp + zlb,
                data = model_df, Hess = TRUE)
summary(m_final)
exp(coef(m_final))

coef_table_final_model <- coef(summary(m_final)) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  mutate(p_value = 2 * pnorm(abs(`t value`), lower.tail = FALSE))

coef_table_final_model

##### 3. Real-time forecasts (expanding window) #####

# Put meetings in date order and find the ones to predict (2010 onwards)
model_df  <- model_df %>% arrange(meeting_date)
test_rows <- which(model_df$meeting_date >= as.Date("2010-01-01"))

# For each meeting: fit the model on past meetings only, then predict it
realtime <- tibble()

for (i in test_rows) {
  past <- model_df[1:(i - 1), ]
  fit  <- polr(formula(m_final), data = past)
  p    <- predict(fit, newdata = model_df[i, ], type = "probs")
  
  realtime <- bind_rows(realtime, tibble(
    meeting_date = model_df$meeting_date[i],
    decision     = model_df$decision[i],
    cut  = p[["cut"]],
    hold = p[["hold"]],
    hike = p[["hike"]]
  ))
}

##### 4. How good were the forecasts? #####

realtime <- realtime %>%
  mutate(
    pred  = c("cut", "hold", "hike")[max.col(cbind(cut, hold, hike))],
    pred  = factor(pred, levels = c("cut", "hold", "hike"), ordered = TRUE),
    brier = (cut  - (decision == "cut"))^2 +
      (hold - (decision == "hold"))^2 +
      (hike - (decision == "hike"))^2
  )

realtime %>% summarise(accuracy = mean(pred == decision), brier = mean(brier))

table(Predicted = realtime$pred, Actual = realtime$decision)

##### 5. Plot #####

realtime %>%
  pivot_longer(c(cut, hold, hike), names_to = "outcome", values_to = "prob") %>%
  ggplot(aes(x = meeting_date, y = prob, colour = outcome)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = realtime %>% filter(decision != "hold"),
             aes(x = meeting_date, y = 1, colour = as.character(decision)),
             inherit.aes = FALSE) +
  labs(x = NULL, y = "Predicted probability", colour = NULL,
       title = "Real-time probability of a cut, hold or hike",
       subtitle = "Dots mark actual cuts and hikes") +
  theme_minimal()

##### 6. What the model said before each actual cut and hike #####

realtime %>%
  filter(decision != "hold") %>%
  dplyr::select(meeting_date, decision, cut, hold, hike) %>%
  print(n = Inf)

##### 7. Real-time probabilities with Bank Rate overlaid #####

# Add Bank Rate to the real-time forecasts
plot_df <- realtime %>%
  left_join(model_df %>% dplyr::select(meeting_date, bank_rate), by = "meeting_date")

# Bank Rate is in %, probabilities run 0 to 1, so rescale Bank Rate to fit
rate_max <- ceiling(max(plot_df$bank_rate))   # e.g. 6, so 0-6% sits on the 0-1 scale

plot_df %>%
  pivot_longer(c(cut, hold, hike), names_to = "outcome", values_to = "prob") %>%
  ggplot(aes(x = meeting_date, y = prob, colour = outcome)) +
  # Bank Rate as a step line (it only changes at meetings)
  geom_step(data = plot_df,
            aes(x = meeting_date, y = bank_rate / rate_max, colour = "Bank Rate (right axis)"),
            inherit.aes = FALSE, linewidth = 0.9, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(data = plot_df %>% filter(decision != "hold"),
             aes(x = meeting_date, y = 1.03, colour = as.character(decision)),
             inherit.aes = FALSE) +
  scale_colour_manual(
    values = c(cut = "#F8766D", hold = "#619CFF", hike = "#00BA38",
               "Bank Rate (right axis)" = "grey30"),
    breaks = c("cut", "hold", "hike", "Bank Rate (right axis)")
  ) +
  scale_y_continuous(
    name     = "Predicted probability",
    limits   = c(0, 1.05),
    sec.axis = sec_axis(~ . * rate_max, name = "Bank Rate (%)")
  ) +
  labs(x = NULL, colour = NULL,
       title    = "Real-time probability of a cut, hold or hike, with Bank Rate",
       subtitle = "Dots mark actual cuts and hikes. Dashed line = Bank Rate (right axis)") +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(model_df, aes(x = meeting_date, y = bank_rate)) + 
  geom_line()

