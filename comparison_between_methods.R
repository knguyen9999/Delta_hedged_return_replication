library(dplyr)
library(tidyr)

#==============================================================================#
# COMBINE DELTA-HEDGED RETURNS COMPARISON METRICS
#==============================================================================#

# Load the saved comparison metrics from all three methods
cao_han_metrics <- readRDS("cao_han_sp500_comparison_metrics.rds")
noisy_prices_metrics <- readRDS("noisy_prices_sp500_comparison_metrics.rds")
day_night_metrics <- readRDS("day_night_sp500_comparison_metrics.rds")

# Function to extract relevant metrics from each list
extract_metrics <- function(metrics_list) {
  tibble(
    method = metrics_list$method,
    n_calls = metrics_list$n_call_options,
    n_puts = metrics_list$n_put_options,
    
    # Call metrics
    call_mean = metrics_list$call_mean_return,
    call_median = metrics_list$call_median_return,
    call_std = metrics_list$call_std_return,
    call_t_stat = metrics_list$call_t_stat,
    call_p_value = metrics_list$call_p_value,
    
    # Put metrics
    put_mean = metrics_list$put_mean_return,
    put_median = metrics_list$put_median_return,
    put_std = metrics_list$put_std_return,
    put_t_stat = metrics_list$put_t_stat,
    put_p_value = metrics_list$put_p_value
  )
}

# Combine all metrics into one table
comparison_table <- bind_rows(
  extract_metrics(cao_han_metrics),
  extract_metrics(noisy_prices_metrics),
  extract_metrics(day_night_metrics)
)

# Format the table for better display
comparison_table_formatted <- comparison_table |>
  mutate(
    # Round numeric values
    across(c(call_mean, call_median, call_std, put_mean, put_median, put_std), 
           ~round(.x, 4)),
    across(c(call_t_stat, put_t_stat), ~round(.x, 2)),
    
    # Format p-values
    call_p_value = format(call_p_value, scientific = TRUE, digits = 3),
    put_p_value = format(put_p_value, scientific = TRUE, digits = 3),
    
    # Add significance stars
    call_sig = case_when(
      as.numeric(call_p_value) < 0.01 ~ "***",
      as.numeric(call_p_value) < 0.05 ~ "**",
      as.numeric(call_p_value) < 0.10 ~ "*",
      TRUE ~ ""
    ),
    put_sig = case_when(
      as.numeric(put_p_value) < 0.01 ~ "***",
      as.numeric(put_p_value) < 0.05 ~ "**",
      as.numeric(put_p_value) < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

# Display the comparison table

print(comparison_table_formatted, n = Inf)

# Create a more readable summary table
summary_table <- comparison_table |>
  select(method, n_calls, n_puts, call_mean, call_t_stat, put_mean, put_t_stat) |>
  mutate(
    call_mean = paste0(round(call_mean, 2), "%"),
    put_mean = paste0(round(put_mean, 2), "%"),
    call_t_stat = round(call_t_stat, 2),
    put_t_stat = round(put_t_stat, 2)
  ) |>
  rename(
    "Method" = method,
    "N (Calls)" = n_calls,
    "N (Puts)" = n_puts,
    "Call Mean Return" = call_mean,
    "Call t-stat" = call_t_stat,
    "Put Mean Return" = put_mean,
    "Put t-stat" = put_t_stat
  )

cat("\n\nSUMMARY TABLE:\n")
cat("---------------\n")
print(summary_table)

# Create a transposed version for easier comparison
metrics_long <- comparison_table |>
  pivot_longer(
    cols = -method,
    names_to = "metric",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = method,
    values_from = value
  )

cat("\n\nTRANSPOSED COMPARISON:\n")
cat("----------------------\n")
print(metrics_long)

# Save the combined table
write.csv(comparison_table, "delta_hedged_comparison_table.csv", row.names = FALSE)
write.csv(summary_table, "delta_hedged_summary_table.csv", row.names = FALSE)

cat("\n\nTables saved to:\n")
cat("- delta_hedged_comparison_table.csv\n")
cat("- delta_hedged_summary_table.csv\n")

# Additional analysis: Check if methods agree on direction
cat("\n\nKEY FINDINGS:\n")
cat("-------------\n")

# Check sign consistency
call_signs <- sign(comparison_table$call_mean)
put_signs <- sign(comparison_table$put_mean)

if(all(call_signs == call_signs[1], na.rm = TRUE)) {
  cat("✓ All methods agree on the SIGN of call delta-hedged returns\n")
} else {
  cat("✗ Methods DISAGREE on the sign of call delta-hedged returns\n")
}

if(all(put_signs == put_signs[1], na.rm = TRUE)) {
  cat("✓ All methods agree on the SIGN of put delta-hedged returns\n")
} else {
  cat("✗ Methods DISAGREE on the sign of put delta-hedged returns\n")
}

# Report ranges
cat("\nCall Returns Range: ", 
    round(min(comparison_table$call_mean, na.rm = TRUE), 2), "% to ",
    round(max(comparison_table$call_mean, na.rm = TRUE), 2), "%\n", sep = "")
cat("Put Returns Range: ", 
    round(min(comparison_table$put_mean, na.rm = TRUE), 2), "% to ",
    round(max(comparison_table$put_mean, na.rm = TRUE), 2), "%\n", sep = "")

# Check statistical significance
sig_calls <- sum(as.numeric(comparison_table$call_p_value) < 0.05, na.rm = TRUE)
sig_puts <- sum(as.numeric(comparison_table$put_p_value) < 0.05, na.rm = TRUE)

cat("\nStatistically significant at 5% level:\n")
cat("- Calls: ", sig_calls, " out of ", nrow(comparison_table), " methods\n", sep = "")
cat("- Puts: ", sig_puts, " out of ", nrow(comparison_table), " methods\n", sep = "")