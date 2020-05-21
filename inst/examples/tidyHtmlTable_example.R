mtcars %>%
  rownames_to_column() %>%
  select(rowname, cyl, gear, hp, mpg, qsec) %>%
  gather(per_metric, value, hp, mpg, qsec) %>%
  group_by(cyl, gear, per_metric) %>%
  summarise(
    Mean = round(mean(value), 1),
    SD = round(sd(value), 1),
    Min = round(min(value), 1),
    Max = round(max(value), 1)
  ) %>%
  gather(summary_stat, value, Mean, SD, Min, Max) %>%
  ungroup() %>%
  mutate(
    gear = paste(gear, "Gears"),
    cyl = paste(cyl, "Cylinders")
  ) %>%
  addHtmlTableStyle(align = "r") %>%
  tidyHtmlTable(
    header = gear,
    cgroup = cyl,
    rnames = summary_stat,
    rgroup = per_metric,
    skipRemovalWraning = TRUE
  )
