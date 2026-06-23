#Compare 3 methods of the 2 papers

library(tidyverse)
library(gt)
library(ggplot2)
library(corrplot)

#Load results
mn_results <- readRDS("muravyev_ni_three_methods.rds")
djw_results <- readRDS("djw_three_methods.rds")

# ============================================================================
# COMBINE ALL RESULTS
# ============================================================================

maturity_order <- c("<7 days", "7-14 days", "15-30 days", "31-60 days", 
                   "61-90 days", "91-180 days", "181-360 days", "360+ days")

moneyness_order <- c("Deep OTM", "OTM", "ATM", "ITM", "Deep ITM")

all_maturity <- bind_rows(
  mn_results$maturity_stats,
  djw_results$maturity_stats
) |>
  mutate(
    maturity_bucket = factor(maturity_bucket, levels = maturity_order, ordered = TRUE)
  )

# Load moneyness stats
all_moneyness <- bind_rows(
  mn_results$moneyness_stats,
  djw_results$moneyness_stats
)|>
  mutate(
    moneyness_bucket = factor(moneyness_bucket, levels = moneyness_order, ordered = TRUE)
  )

# ============================================================================
# CREATE DETAILED TABLE FUNCTION
# ============================================================================

create_detailed_table <- function(data, option_type = "C") {
  
  table_data <- data |>
    filter(cp_flag == option_type) |>
    mutate(
      # Format values - handle NAs properly
      mean_str = sprintf("%.4f", mean_return),
      t_stat_str = ifelse(is.na(t_stat_clustered), 
                          "—", 
                          sprintf("%.2f", t_stat_clustered)),
      p_value_str = ifelse(is.na(p_value_clustered), 
                           "—", 
                           sprintf("%.4f", p_value_clustered)),
      n_str = format(n_obs, big.mark = ",", trim = TRUE),
      
      # Create row identifier
      row_base = paste(paper, "-", method)
    ) |>
    select(row_base, maturity_bucket, mean_str, t_stat_str, p_value_str, n_str)
  
  # Create separate rows for each statistic
  mean_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [Mean]")) |>
    select(row_id, maturity_bucket, value = mean_str) |>
    pivot_wider(names_from = maturity_bucket, values_from = value, values_fill = "—")
  
  tstat_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [t-stat]")) |>
    select(row_id, maturity_bucket, value = t_stat_str) |>
    pivot_wider(names_from = maturity_bucket, values_from = value, values_fill = "—")
  
  pval_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [p-value]")) |>
    select(row_id, maturity_bucket, value = p_value_str) |>
    pivot_wider(names_from = maturity_bucket, values_from = value, values_fill = "—")
  
  n_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [N]")) |>
    select(row_id, maturity_bucket, value = n_str) |>
    pivot_wider(names_from = maturity_bucket, values_from = value, values_fill = "—")
  
  # Combine all rows in order
  combined <- bind_rows(
    mean_rows |> mutate(stat_type = 1, method_order = str_replace(row_id, " \\[Mean\\]", "")),
    tstat_rows |> mutate(stat_type = 2, method_order = str_replace(row_id, " \\[t-stat\\]", "")),
    pval_rows |> mutate(stat_type = 3, method_order = str_replace(row_id, " \\[p-value\\]", "")),
    n_rows |> mutate(stat_type = 4, method_order = str_replace(row_id, " \\[N\\]", ""))
  ) |>
    arrange(method_order, stat_type) |>
    select(-stat_type, -method_order)
  
  #Reorder columns
  col_order <- c("row_id", maturity_order)
  
  combined |>
    select(all_of(intersect(col_order, names(combined))))
}

# ============================================================================
# GENERATE DETAILED TABLES
# ============================================================================

#TABLE 1: CALL OPTIONS - DETAILED STATISTICS")
calls_detailed <- create_detailed_table(all_maturity, "C")
print(calls_detailed, n = Inf, width = Inf)

#TABLE 2: PUT OPTIONS - DETAILED STATISTICS
puts_detailed <- create_detailed_table(all_maturity, "P")
print(puts_detailed, n = Inf, width = Inf)

# ============================================================================
# CREATE MONEYNESS DETAILED TABLES
# ============================================================================

create_moneyness_detailed <- function(data, option_type = "C") {
  
  table_data <- data |>
    filter(cp_flag == option_type) |>
    mutate(
      mean_str = sprintf("%.4f", mean_return),
      t_stat_str = ifelse(is.na(t_stat_clustered), 
                          "—", 
                          sprintf("%.2f", t_stat_clustered)),
      p_value_str = ifelse(is.na(p_value_clustered), 
                           "—", 
                           sprintf("%.4f", p_value_clustered)),
      n_str = format(n_obs, big.mark = ",", trim = TRUE),
      row_base = paste(paper, "-", method)
    ) |>
    select(row_base, moneyness_bucket, mean_str, t_stat_str, p_value_str, n_str)
  
  # Create separate rows
  mean_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [Mean]")) |>
    select(row_id, moneyness_bucket, value = mean_str) |>
    pivot_wider(names_from = moneyness_bucket, values_from = value, values_fill = "—")
  
  tstat_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [t-stat]")) |>
    select(row_id, moneyness_bucket, value = t_stat_str) |>
    pivot_wider(names_from = moneyness_bucket, values_from = value, values_fill = "—")
  
  pval_rows <- table_data |>
    mutate(row_id = paste0(row_base, " [p-value]")) |>
    select(row_id, moneyness_bucket, value = p_value_str) |>
    pivot_wider(names_from = moneyness_bucket, values_from = value, values_fill = "—")
  
  # Combine
  combined <- bind_rows(
    mean_rows |> mutate(stat_type = 1, method_order = str_replace(row_id, " \\[Mean\\]", "")),
    tstat_rows |> mutate(stat_type = 2, method_order = str_replace(row_id, " \\[t-stat\\]", "")),
    pval_rows |> mutate(stat_type = 3, method_order = str_replace(row_id, " \\[p-value\\]", ""))
  ) |>
    arrange(method_order, stat_type) |>
    select(-stat_type, -method_order)
  
  # Order columns
  col_order <- c("row_id", moneyness_order)
  combined |>
    select(all_of(intersect(col_order, names(combined))))
}

#TABLE 3: CALL OPTIONS - MONEYNESS DETAILED
calls_moneyness_detailed <- create_moneyness_detailed(all_moneyness, "C")
print(calls_moneyness_detailed, n = Inf, width = Inf)

#TABLE 4: PUT OPTIONS - MONEYNESS DETAILED
puts_moneyness_detailed <- create_moneyness_detailed(all_moneyness, "P")
print(puts_moneyness_detailed, n = Inf, width = Inf)

#TABLE 5: SUMMARY COMPARISON
summary_comparison <- all_maturity |>
  group_by(paper, method, cp_flag) |>
  summarise(
    n_total = sum(n_obs),
    mean_overall = weighted.mean(mean_return, n_obs, na.rm = TRUE),
    avg_t_stat = mean(abs(t_stat_clustered), na.rm = TRUE),
    pct_significant = mean(p_value_clustered < 0.05, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(
    Mean = sprintf("%.4f", mean_overall),
    `Avg |t|` = sprintf("%.2f", avg_t_stat),
    `% Sig` = sprintf("%.0f%%", pct_significant),
    `Total N` = format(n_total, big.mark = ",")
  ) |>
  mutate(Method = paste(paper, method, sep = " - ")) |>
  select(Method, Type = cp_flag, Mean, `Avg |t|`, `% Sig`, `Total N`)

print(summary_comparison, n = Inf)

# ============================================================================
# SAVE ALL OUTPUTS
# ============================================================================

write.csv(calls_detailed, "calls_maturity_detailed.csv", row.names = FALSE)
write.csv(puts_detailed, "puts_maturity_detailed.csv", row.names = FALSE)
write.csv(calls_moneyness_detailed, "calls_moneyness_detailed.csv", row.names = FALSE)
write.csv(puts_moneyness_detailed, "puts_moneyness_detailed.csv", row.names = FALSE)
write.csv(summary_comparison, "summary_comparison.csv", row.names = FALSE)

# ============================================================================
# TRIAL WITH HTML TABLE
# HTML TABLE 1: MATURITY - CALLS
# ============================================================================

prepare_stacked_table <- function(data, option_type) {
  
  # Process each method separately
  process_method <- function(data, method_name) {
    data |>
      filter(cp_flag == option_type, method == method_name) |>
      mutate(
        maturity_bucket = factor(maturity_bucket, levels = maturity_order, ordered = TRUE),
        Mean = sprintf("%.4f", mean_return),
        `t-stat` = sprintf("%.2f", ifelse(is.na(t_stat_clustered), 0, t_stat_clustered)),
        `p-value` = sprintf("%.4f", ifelse(is.na(p_value_clustered), 1, p_value_clustered)),
        N = format(n_obs, big.mark = ","),
        paper_short = if_else(paper == "Muravyev-Ni", "MN", "DJW")
      ) |>
      select(maturity_bucket, paper_short, Mean, `t-stat`, `p-value`, N) |>
      pivot_wider(
        names_from = paper_short,
        values_from = c(Mean, `t-stat`, `p-value`, N),
        names_glue = "{paper_short}_{.value}"
      ) |>
      mutate(Method = method_name)
  }
  
  # Stack all three methods
  bind_rows(
    process_method(data, "All_Observations"),
    process_method(data, "Midpoint"),
    process_method(data, "Random")
  ) |> 
    mutate(maturity_bucket = factor(maturity_bucket, levels = maturity_order, ordered = TRUE)) |>
    arrange(maturity_bucket) |>
    select(Method, maturity_bucket, 
           MN_Mean, DJW_Mean, 
           `MN_t-stat`, `DJW_t-stat`,
           `MN_p-value`, `DJW_p-value`,
           MN_N, DJW_N)
}

# Create calls table
calls_stacked <- prepare_stacked_table(all_maturity, "C")

gt_calls <- calls_stacked |>
  gt(groupname_col = "Method") |>
  tab_header(
    title = "Table 1: Call Options - Delta-Hedged Returns by Maturity",
    subtitle = "Side-by-side comparison: Muravyev-Ni (MN) vs Duarte-Jones-Wang (DJW)"
  ) |>
  cols_label(
    maturity_bucket = "Maturity",
    MN_Mean = "MN",
    DJW_Mean = "DJW",
    `MN_t-stat` = "MN",
    `DJW_t-stat` = "DJW",
    `MN_p-value` = "MN",
    `DJW_p-value` = "DJW",
    MN_N = "MN",
    DJW_N = "DJW"
  ) |>
  tab_spanner(
    label = "Mean Return",
    columns = c(MN_Mean, DJW_Mean)
  ) |>
  tab_spanner(
    label = "t-statistic",
    columns = c(`MN_t-stat`, `DJW_t-stat`)
  ) |>
  tab_spanner(
    label = "p-value",
    columns = c(`MN_p-value`, `DJW_p-value`)
  ) |>
  tab_spanner(
    label = "N Observations",
    columns = c(MN_N, DJW_N)
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#2E4057"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#E8E8E8"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#CCCCCC",
      weight = px(1)
    ),
    locations = cells_body(
      columns = c(DJW_Mean, `DJW_t-stat`, `DJW_p-value`)
    )
  ) |>
  cols_align(align = "center", columns = -maturity_bucket) |>
  opt_table_font(font = list("Helvetica", "Arial", "sans-serif"))

gtsave(gt_calls, "stacked_calls_maturity.html")


# ============================================================================
# HTML TABLE 2: MATURITY - PUTS
# ============================================================================

puts_stacked <- prepare_stacked_table(all_maturity, "P")

gt_puts <- puts_stacked |>
  gt(groupname_col = "Method") |>
  tab_header(
    title = "Table 2: Put Options - Delta-Hedged Returns by Maturity",
    subtitle = "Side-by-side comparison: Muravyev-Ni (MN) vs Duarte-Jones-Wang (DJW)"
  ) |>
  cols_label(
    maturity_bucket = "Maturity",
    MN_Mean = "MN",
    DJW_Mean = "DJW",
    `MN_t-stat` = "MN",
    `DJW_t-stat` = "DJW",
    `MN_p-value` = "MN",
    `DJW_p-value` = "DJW",
    MN_N = "MN",
    DJW_N = "DJW"
  ) |>
  tab_spanner(
    label = "Mean Return",
    columns = c(MN_Mean, DJW_Mean)
  ) |>
  tab_spanner(
    label = "t-statistic",
    columns = c(`MN_t-stat`, `DJW_t-stat`)
  ) |>
  tab_spanner(
    label = "p-value",
    columns = c(`MN_p-value`, `DJW_p-value`)
  ) |>
  tab_spanner(
    label = "N Observations",
    columns = c(MN_N, DJW_N)
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#8B4513"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#E8E8E8"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#CCCCCC",
      weight = px(1)
    ),
    locations = cells_body(
      columns = c(DJW_Mean, `DJW_t-stat`, `DJW_p-value`)
    )
  ) |>
  cols_align(align = "center", columns = -maturity_bucket) |>
  opt_table_font(font = list("Helvetica", "Arial", "sans-serif"))

gtsave(gt_puts, "stacked_puts_maturity.html")

# ============================================================================
# HTML TABLE 3: MONEYNESS - CALLS
# ============================================================================

all_moneyness <- bind_rows(
  mn_results$moneyness_stats,
  djw_results$moneyness_stats
)

prepare_stacked_moneyness <- function(data, option_type) {
  
  process_method <- function(data, method_name) {
    data |>
      filter(cp_flag == option_type, method == method_name) |>
      mutate(
        moneyness_bucket = factor(moneyness_bucket, levels = moneyness_order, ordered = TRUE),
        Mean = sprintf("%.4f", mean_return),
        `t-stat` = sprintf("%.2f", ifelse(is.na(t_stat_clustered), 0, t_stat_clustered)),
        `p-value` = sprintf("%.4f", ifelse(is.na(p_value_clustered), 1, p_value_clustered)),
        N = format(n_obs, big.mark = ","),
        paper_short = if_else(paper == "Muravyev-Ni", "MN", "DJW")
      ) |>
      select(moneyness_bucket, paper_short, Mean, `t-stat`, `p-value`, N) |>
      pivot_wider(
        names_from = paper_short,
        values_from = c(Mean, `t-stat`, `p-value`, N),
        names_glue = "{paper_short}_{.value}"
      ) |>
      mutate(Method = method_name)
  }
  
  bind_rows(
    process_method(data, "All_Observations"),
    process_method(data, "Midpoint"),
    process_method(data, "Random")
  ) |>
    mutate(moneyness_bucket = factor(moneyness_bucket, levels = moneyness_order, ordered = TRUE)) |>
    arrange(moneyness_bucket) |>
    select(Method, moneyness_bucket,
           MN_Mean, DJW_Mean,
           `MN_t-stat`, `DJW_t-stat`,
           `MN_p-value`, `DJW_p-value`,
           MN_N, DJW_N)
}

moneyness_calls_stacked <- prepare_stacked_moneyness(all_moneyness, "C")

gt_moneyness_calls <- moneyness_calls_stacked |>
  gt(groupname_col = "Method") |>
  tab_header(
    title = "Table 3: Call Options - Delta-Hedged Returns by Moneyness",
    subtitle = "Side-by-side comparison: Muravyev-Ni (MN) vs Duarte-Jones-Wang (DJW)"
  ) |>
  cols_label(
    moneyness_bucket = "Moneyness",
    MN_Mean = "MN",
    DJW_Mean = "DJW",
    `MN_t-stat` = "MN",
    `DJW_t-stat` = "DJW",
    `MN_p-value` = "MN",
    `DJW_p-value` = "DJW",
    MN_N = "MN",
    DJW_N = "DJW"
  ) |>
  tab_spanner(
    label = "Mean Return",
    columns = c(MN_Mean, DJW_Mean)
  ) |>
  tab_spanner(
    label = "t-statistic",
    columns = c(`MN_t-stat`, `DJW_t-stat`)
  ) |>
  tab_spanner(
    label = "p-value",
    columns = c(`MN_p-value`, `DJW_p-value`)
  ) |>
  tab_spanner(
    label = "N Observations",
    columns = c(MN_N, DJW_N)
  ) |>
  cols_align(align = "center", columns = -moneyness_bucket) |>
  opt_table_font(font = list("Helvetica", "Arial", "sans-serif"))

gtsave(gt_moneyness_calls, "stacked_moneyness_calls.html")

# ============================================================================
# HTML TABLE 4: MONEYNESS - PUTS
# ============================================================================

# Create puts moneyness
moneyness_puts_stacked <- prepare_stacked_moneyness(all_moneyness, "P")

gt_moneyness_puts <- moneyness_puts_stacked |>
  gt(groupname_col = "Method") |>
  tab_header(
    title = "Table 4: Put Options - Delta-Hedged Returns by Moneyness",
    subtitle = "Side-by-side comparison: Muravyev-Ni (MN) vs Duarte-Jones-Wang (DJW)"
  ) |>
  cols_label(
    moneyness_bucket = "Moneyness",
    MN_Mean = "MN",
    DJW_Mean = "DJW",
    `MN_t-stat` = "MN",
    `DJW_t-stat` = "DJW",
    `MN_p-value` = "MN",
    `DJW_p-value` = "DJW",
    MN_N = "MN",
    DJW_N = "DJW"
  ) |>
  tab_spanner(
    label = "Mean Return",
    columns = c(MN_Mean, DJW_Mean)
  ) |>
  tab_spanner(
    label = "t-statistic",
    columns = c(`MN_t-stat`, `DJW_t-stat`)
  ) |>
  tab_spanner(
    label = "p-value",
    columns = c(`MN_p-value`, `DJW_p-value`)
  ) |>
  tab_spanner(
    label = "N Observations",
    columns = c(MN_N, DJW_N)
  ) |>
  cols_align(align = "center", columns = -moneyness_bucket) |>
  opt_table_font(font = list("Helvetica", "Arial", "sans-serif"))

gtsave(gt_moneyness_puts, "stacked_moneyness_puts.html")


# ============================================================================
# HTML TABLE 5: SUMMARY COMPARISON
# ============================================================================

summary_data <- all_maturity |>
  group_by(paper, method, cp_flag) |>
  summarise(
    n_total = sum(n_obs),
    mean_overall = weighted.mean(mean_return, n_obs, na.rm = TRUE),
    avg_t_stat = mean(abs(t_stat_clustered), na.rm = TRUE),
    pct_significant = mean(p_value_clustered < 0.05, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(
    `Mean Return` = sprintf("%.4f", mean_overall),
    `Avg |t-stat|` = sprintf("%.2f", avg_t_stat),
    `% Significant` = sprintf("%.0f%%", pct_significant),
    `Total N` = format(n_total, big.mark = ","),
    Type = cp_flag
  ) |>
  select(Paper = paper, Method = method, Type, 
         `Mean Return`, `Avg |t-stat|`, `% Significant`, `Total N`)

gt_summary <- summary_data |>
  gt(groupname_col = "Paper") |>
  tab_header(
    title = "Table 5: Summary Statistics - Overall Comparison",
    subtitle = "Weighted averages across all maturity buckets"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#1E3A5F"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_fill(color = "#E8F4F8"),
    locations = cells_row_groups()
  ) |>
  cols_align(align = "center", columns = -Method) |>
  tab_footnote(
    footnote = "Mean Return is weighted by number of observations",
    locations = cells_column_labels(columns = `Mean Return`)
  ) |>
  tab_footnote(
    footnote = "% Significant shows proportion of buckets with p-value < 0.05",
    locations = cells_column_labels(columns = `% Significant`)
  )

gtsave(gt_summary, "comparison_summary.html")

# ============================================================================
# HTML TABLE 6: KEY BUCKETS COMPARISON (COMPACT)
# ============================================================================

key_buckets_data <- all_maturity |>
  filter(maturity_bucket %in% c("<7 days", "7-14 days", "31-60 days", "91-180 days")) |>
  mutate(
    `Return (t-stat)` = sprintf("%.4f (%.2f)", 
                                mean_return, 
                                ifelse(is.na(t_stat_clustered), 0, t_stat_clustered)),
    p_val = ifelse(is.na(p_value_clustered), "—", sprintf("%.4f", p_value_clustered)),
    ID = paste(paper, "-", method)
  ) |>
  select(ID, Type = cp_flag, Bucket = maturity_bucket, `Return (t-stat)`, `p-value` = p_val)

gt_key_buckets <- key_buckets_data |>
  gt(groupname_col = "ID") |>
  tab_header(
    title = "Table 6: Key Maturity Buckets - Compact View",
    subtitle = "Focus on short-term and medium-term options"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#4A5568"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_fill(color = "#F7FAFC"),
    locations = cells_row_groups()
  ) |>
  cols_align(align = "center", columns = everything()) |>
  tab_options(
    table.font.size = px(12),
    row_group.font.weight = "bold"
  )

gtsave(gt_key_buckets, "key_buckets_comparison.html")

# ============================================================================
# PREPARE DATA FOR CORRELATION ANALYSIS
# ============================================================================

#Combine maturity statistics for correlation
maturity_combined <- mn_results$maturity_stats |>
  select(maturity_bucket, cp_flag, method, 
         mn_mean = mean_return, mn_t_stat = t_stat_clustered, mn_n = n_obs) |>
  inner_join(
    djw_results$maturity_stats |>
      select(maturity_bucket, cp_flag, method, 
             djw_mean = mean_return, djw_t_stat = t_stat_clustered, djw_n = n_obs),
    by = c("maturity_bucket", "cp_flag", "method")
  ) |>
  mutate(
    maturity_bucket = factor(maturity_bucket, levels = maturity_order, ordered = TRUE)
  ) |>
  filter(!is.na(mn_mean) & !is.na(djw_mean))

# Combine moneyness statistics for correlation
moneyness_combined <- mn_results$moneyness_stats |>
  select(moneyness_bucket, cp_flag, method, 
         mn_mean = mean_return, mn_t_stat = t_stat_clustered, mn_n = n_obs) |>
  inner_join(
    djw_results$moneyness_stats |>
      select(moneyness_bucket, cp_flag, method, 
             djw_mean = mean_return, djw_t_stat = t_stat_clustered, djw_n = n_obs),
    by = c("moneyness_bucket", "cp_flag", "method")
  ) |>
  mutate(
    moneyness_bucket = factor(moneyness_bucket, levels = moneyness_order, ordered = TRUE)
  ) |>
  filter(!is.na(mn_mean) & !is.na(djw_mean))

# ============================================================================
# CALCULATE CORRELATIONS
# ============================================================================

# Overall correlations by method
overall_corr <- maturity_combined |>
  group_by(method) |>
  summarise(
    corr_mean = cor(mn_mean, djw_mean, use = "complete.obs"),
    corr_t_stat = cor(mn_t_stat, djw_t_stat, use = "complete.obs"),
    n_pairs = n(),
    .groups = "drop"
  )

# Correlations by method and option type
corr_by_type <- maturity_combined |>
  group_by(method, cp_flag) |>
  summarise(
    corr_mean = cor(mn_mean, djw_mean, use = "complete.obs"),
    corr_t_stat = cor(mn_t_stat, djw_t_stat, use = "complete.obs"),
    n_pairs = n(),
    .groups = "drop"
  )

# Correlations for moneyness
moneyness_corr <- moneyness_combined |>
  group_by(method) |>
  summarise(
    corr_mean = cor(mn_mean, djw_mean, use = "complete.obs"),
    corr_t_stat = cor(mn_t_stat, djw_t_stat, use = "complete.obs"),
    n_pairs = n(),
    .groups = "drop"
  )

# ============================================================================
# CREATE SCATTER PLOTS
# ============================================================================

# Function to create scatter plot
create_scatter <- function(data, x_var, y_var, title, subtitle = NULL) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(aes(color = cp_flag, shape = method), size = 3, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_smooth(method = "lm", se = TRUE, color = "blue", size = 0.5) +
    scale_color_manual(values = c("C" = "#2E7D32", "P" = "#C62828"),
                      labels = c("C" = "Calls", "P" = "Puts")) +
    scale_shape_manual(values = c("All_Observations" = 16, 
                                 "Midpoint" = 17, 
                                 "Random" = 15)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Muravyev-Ni",
      y = "Duarte-Jones-Wang",
      color = "Option Type",
      shape = "Method"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "right"
    ) +
    coord_equal()
}

# Create scatter plots for mean returns
scatter_mean <- create_scatter(
  maturity_combined,
  "mn_mean", "djw_mean",
  "Mean Returns: MN vs DJW",
  paste0("Overall correlation: ", round(cor(maturity_combined$mn_mean, 
                                           maturity_combined$djw_mean), 3))
)

# Create scatter plots for t-statistics
scatter_t <- create_scatter(
  maturity_combined |> filter(!is.na(mn_t_stat) & !is.na(djw_t_stat)),
  "mn_t_stat", "djw_t_stat",
  "t-Statistics: MN vs DJW",
  paste0("Overall correlation: ", 
         round(cor(maturity_combined$mn_t_stat, 
                  maturity_combined$djw_t_stat, use = "complete.obs"), 3))
)

# Save plots
ggsave("scatter_mean_returns.png", scatter_mean, width = 10, height = 8, dpi = 300)
ggsave("scatter_t_stats.png", scatter_t, width = 10, height = 8, dpi = 300)

# ============================================================================
# REGRESSION ANALYSIS
# ============================================================================

# Regression: DJW on MN returns
reg_returns <- lm(djw_mean ~ mn_mean, data = maturity_combined)
reg_returns_by_method <- lm(djw_mean ~ mn_mean * method, data = maturity_combined)

# Print regression summary
cat("\n=== REGRESSION: DJW Returns on MN Returns ===\n")
print(summary(reg_returns))

cat("\n=== REGRESSION WITH METHOD INTERACTION ===\n")
print(summary(reg_returns_by_method))

# ============================================================================
# CREATE CORRELATION TABLES
# ============================================================================

# Table 1: Overall Correlations
gt_overall <- overall_corr |>
  mutate(
    `Return Correlation` = sprintf("%.3f", corr_mean),
    `t-stat Correlation` = sprintf("%.3f", corr_t_stat),
    `N Pairs` = n_pairs
  ) |>
  select(Method = method, `Return Correlation`, `t-stat Correlation`, `N Pairs`) |>
  gt() |>
  tab_header(
    title = "Correlation Analysis: Muravyev-Ni vs Duarte-Jones-Wang",
    subtitle = "Correlation coefficients across maturity buckets"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#1E3A5F"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  cols_align(align = "center")

gtsave(gt_overall, "correlation_overall.html")

# Table 2: Correlations by Option Type
gt_by_type <- corr_by_type |>
  mutate(
    `Return Corr` = sprintf("%.3f", corr_mean),
    `t-stat Corr` = sprintf("%.3f", corr_t_stat),
    Type = ifelse(cp_flag == "C", "Calls", "Puts")
  ) |>
  select(Method = method, Type, `Return Corr`, `t-stat Corr`, N = n_pairs) |>
  gt() |>
  tab_header(
    title = "Correlations by Option Type",
    subtitle = "Separate analysis for calls and puts"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#2E4057"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |>
  cols_align(align = "center")

gtsave(gt_by_type, "correlation_by_type.html")

# ============================================================================
# DIFFERENCE ANALYSIS
# ============================================================================

#Calculate differences
diff_analysis <- maturity_combined |>
  mutate(
    diff_mean = djw_mean - mn_mean,
    diff_t_stat = djw_t_stat - mn_t_stat,
    ratio_n = djw_n / mn_n
  ) |>
  group_by(method, cp_flag) |>
  summarise(
    mean_diff = mean(diff_mean, na.rm = TRUE),
    sd_diff = sd(diff_mean, na.rm = TRUE),
    mean_t_diff = mean(diff_t_stat, na.rm = TRUE),
    sd_t_diff = sd(diff_t_stat, na.rm = TRUE),
    mean_n_ratio = mean(ratio_n, na.rm = TRUE),
    .groups = "drop"
  )

# Table 3: Difference Analysis
gt_diff <- diff_analysis |>
  mutate(
    Type = ifelse(cp_flag == "C", "Calls", "Puts"),
    `Mean Diff (DJW-MN)` = sprintf("%.4f (%.4f)", mean_diff, sd_diff),
    `t-stat Diff` = sprintf("%.2f (%.2f)", mean_t_diff, sd_t_diff),
    `N Ratio (DJW/MN)` = sprintf("%.2f", mean_n_ratio)
  ) |>
  select(Method = method, Type, `Mean Diff (DJW-MN)`, `t-stat Diff`, `N Ratio (DJW/MN)`) |>
  gt() |>
  tab_header(
    title = "Difference Analysis: DJW minus MN",
    subtitle = "Mean (SD) of differences across buckets"
  ) |>
  cols_align(align = "center")

gtsave(gt_diff, "difference_analysis.html")

# ============================================================================
# BUCKET-LEVEL COMPARISON HEATMAP
# ============================================================================

# Create heatmap data for maturity
heatmap_data <- maturity_combined |>
  mutate(diff_mean = djw_mean - mn_mean) |>
  select(method, maturity_bucket, cp_flag, diff_mean) |>
  pivot_wider(names_from = maturity_bucket, values_from = diff_mean)

# Create separate heatmaps for calls and puts
for(option_type in c("C", "P")) {
  
  plot_data <- heatmap_data |>
    filter(cp_flag == option_type) |>
    select(-cp_flag) |>
    column_to_rownames("method") |>
    as.matrix()
  
  # Create heatmap
  png(paste0("heatmap_diff_", ifelse(option_type == "C", "calls", "puts"), ".png"),
      width = 12, height = 6, units = "in", res = 300)
  
  corrplot(plot_data, 
           is.corr = FALSE,
           method = "color",
           type = "full",
           col = colorRampPalette(c("#C62828", "white", "#2E7D32"))(100),
           addCoef.col = "black",
           number.cex = 0.8,
           tl.col = "black",
           tl.srt = 45,
           title = paste(ifelse(option_type == "C", "Calls", "Puts"), 
                        ": DJW - MN Mean Return Differences"),
           mar = c(0, 0, 2, 0))
  
  dev.off()
}

# ============================================================================
# STATISTICAL TESTS
# ============================================================================

# Paired t-test for mean returns
t_test_results <- maturity_combined |>
  group_by(method) |>
  summarise(
    t_statistic = t.test(djw_mean, mn_mean, paired = TRUE)$statistic,
    p_value = t.test(djw_mean, mn_mean, paired = TRUE)$p.value,
    mean_diff = mean(djw_mean - mn_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Print test results
cat("\n=== PAIRED T-TESTS: DJW vs MN Mean Returns ===\n")
print(t_test_results)

# ============================================================================
# SUMMARY REPORT
# ============================================================================

cat("\n" ,rep("=", 70), "\n")
cat("CORRELATION ANALYSIS SUMMARY REPORT\n")
cat(rep("=", 70), "\n\n")

cat("1. OVERALL CORRELATIONS (Across all buckets):\n")
cat(rep("-", 40), "\n")
for(i in 1:nrow(overall_corr)) {
  cat(sprintf("   %s: Return Corr = %.3f, t-stat Corr = %.3f\n",
              overall_corr$method[i], 
              overall_corr$corr_mean[i], 
              overall_corr$corr_t_stat[i]))
}

cat("\n2. KEY FINDINGS:\n")
cat(rep("-", 40), "\n")

# Find highest and lowest correlations
max_corr <- max(overall_corr$corr_mean)
min_corr <- min(overall_corr$corr_mean)
max_method <- overall_corr$method[which.max(overall_corr$corr_mean)]
min_method <- overall_corr$method[which.min(overall_corr$corr_mean)]

cat(sprintf("   - Highest correlation: %s (%.3f)\n", max_method, max_corr))
cat(sprintf("   - Lowest correlation: %s (%.3f)\n", min_method, min_corr))

# Average differences
avg_diff <- mean(maturity_combined$djw_mean - maturity_combined$mn_mean)
cat(sprintf("   - Average difference (DJW - MN): %.4f%%\n", avg_diff))

cat("\n3. REGRESSION RESULTS:\n")
cat(rep("-", 40), "\n")
cat(sprintf("   - Slope: %.3f (SE: %.3f)\n", 
            coef(reg_returns)[2], 
            summary(reg_returns)$coefficients[2, 2]))
cat(sprintf("   - Intercept: %.4f\n", coef(reg_returns)[1]))
cat(sprintf("   - R-squared: %.3f\n", summary(reg_returns)$r.squared))

cat("\n4. INTERPRETATION:\n")
cat(rep("-", 40), "\n")

if(max_corr > 0.8) {
  cat("   - Strong positive correlation between methods\n")
} else if(max_corr > 0.5) {
  cat("   - Moderate positive correlation between methods\n")
} else {
  cat("   - Weak correlation between methods\n")
}

if(abs(coef(reg_returns)[2] - 1) < 0.1) {
  cat("   - Slope near 1 suggests similar magnitudes\n")
} else if(coef(reg_returns)[2] > 1) {
  cat("   - DJW returns are amplified relative to MN\n")
} else {
  cat("   - DJW returns are dampened relative to MN\n")
}

cat("\n", rep("=", 70), "\n")

# Save all results
save(maturity_combined, moneyness_combined, overall_corr, corr_by_type, 
     diff_analysis, t_test_results, reg_returns,
     file = "correlation_analysis_results.RData")
