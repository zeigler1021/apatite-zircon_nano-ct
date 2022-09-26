# Test regressions results for statistical significance 

anova_comps <- apatite_regressions |> 
  select(estimate, parameter, gc, size_cat, ri) |> 
  filter(parameter %in% c("v", "ft238", "rft")) |> 
  pivot_longer(3:5, names_to = "grouping") |> 
  group_by(parameter, grouping) |> 
  group_map(~ tidy(TukeyHSD(aov(.x$estimate ~ .x$value)))) 
  
# Calculate an ANOVA and Tukey post-hoc for every grouping of interest

anova_comps <- do.call(rbind.data.frame, anova_comps) # list to dataframe
parameter <- rep(c("rft", "238ft", "v"), times = 1, each = 5) # name the rows
anova_comps <- cbind(parameter, anova_comps)
write_csv(anova_comps, "output/20220804_apatite-anova-comps.csv")
 
###############################################################

# Print sd of residuals for all relevant groupings to make Table C1

#Apatite

apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ft238", "rft")) |> 
  mutate(size = case_when(geo == "ellipsoid" ~ "Medium & Large", 
                          geo == "hexagonal" ~ as.character(size_cat)), 
         rough = case_when(geo == "ellipsoid" ~ "1 & 2",
                           geo == "hexagonal" ~ as.character(ri))) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))


apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ft238", "rft"), 
         geo == "hexagonal") |>
  mutate(size = case_when(geo == "hexagonal" ~ as.character(size_cat)), 
         rough = case_when(geo == "hexagonal" ~ "1 & 2")) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ft238", "rft"), 
         geo == "hexagonal") |>
  mutate(size = case_when(geo == "hexagonal" ~ "Medium & Large"), 
         rough = case_when(geo == "hexagonal" ~ "1 & 2")) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ft238", "rft"), 
         geo == "hexagonal") |>
  mutate(size = case_when(geo == "hexagonal" ~ "Medium & Large"), 
         rough = case_when(geo == "hexagonal" ~ as.character(ri))) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))


         