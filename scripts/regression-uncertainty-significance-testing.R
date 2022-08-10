# Test regressions results for statistical significance 

Hypothesis test for ANOVA with Tukey post-hoc: 
  
  $H_0: \mu_1 = \mu_2 = ... \mu_n$
    $H_1: \mu_1 \neq \mu_2 \neq ... \mu_n$
    
    ## Apatite 
  anova_comps <- apatite_regressions |> 
    select(estimate, parameter, gc, size_cat, ri) |> 
    filter(parameter %in% c("v", "ftcomb", "rft")) |> 
    pivot_longer(3:5, names_to = "grouping") |> 
    group_by(parameter, grouping) |> 
    group_map(~ tidy(TukeyHSD(aov(.x$estimate ~ .x$value)))) 
  
  # Calculate an ANOVA and Tukey post-hoc for every grouping of interest

    
anova_comps <- do.call(rbind.data.frame, anova_comps) # list to dataframe
parameter <- rep(c("rft", "ft", "v"), times = 1, each = 5) # name the rows
anova_comps <- cbind(parameter, anova_comps)
# write_csv(anova_comps, "/Users/spencerzeigler/Desktop/20220804_apatite-anova-comps.csv")
 
 # Volume
 
TukeyHSD(aov(estimate ~ ri, data = filter(apatite_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ gem, data = filter(apatite_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ size_cat, data = filter(apatite_regressions, parameter == "v")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(apatite_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ gc, data = filter(apatite_regressions, parameter == "v")))
  
# Only gc is significant
  
# Mean Ft
TukeyHSD(aov(estimate ~ ri, data = filter(apatite_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ gem, data = filter(apatite_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ size_cat, data = filter(apatite_regressions, parameter == "ftcomb")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(apatite_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ gc, data = filter(apatite_regressions, parameter == "ftcomb")))

# Only gc is significant

# R_Ft 
TukeyHSD(aov(estimate ~ ri, data = filter(apatite_regressions, parameter == "rft")))
TukeyHSD(aov(estimate ~ gem, data = filter(apatite_regressions, parameter == "rft" )))
TukeyHSD(aov(estimate ~ size_cat, data = filter(apatite_regressions, parameter == "rft")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(apatite_regressions, parameter == "rft")))
TukeyHSD(aov(estimate ~ gc, data = filter(apatite_regressions, parameter == "rft")))

# Only gc is significant


## Zircon

anova_comps <- zircon_regressions |> 
  select(estimate, parameter, gc, size_cat, ci) |> 
  filter(parameter %in% c("v", "ftcomb", "rft")) |> 
  pivot_longer(3:5, names_to = "grouping") |> 
  group_by(parameter, grouping) |> 
  group_map(~ tidy(TukeyHSD(aov(.x$estimate ~ .x$value))))

# Calculate an ANOVA and Tukey post-hoc for every grouping of interest
  
anova_comps <- do.call(rbind.data.frame, anova_comps) # list to dataframe
parameter <- rep(c("rft", "ft", "v"), times = 1, each = 7) # name the rows
anova_comps <- cbind(parameter, anova_comps)
# write_csv(anova_comps, "/Users/spencerzeigler/Desktop/20220804_zircon-anova-comps.csv")

# Volume
TukeyHSD(aov(estimate ~ ci, data = filter(zircon_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ gem, data = filter(zircon_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ size_cat, data = filter(zircon_regressions, parameter == "v")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(zircon_regressions, parameter == "v")))
TukeyHSD(aov(estimate ~ gc, data = filter(zircon_regressions, parameter == "v")))

# Only gc is significant

# Mean Ft
TukeyHSD(aov(estimate ~ ci, data = filter(zircon_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ gem, data = filter(zircon_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ size_cat, data = filter(zircon_regressions, parameter == "ftcomb")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(zircon_regressions, parameter == "ftcomb")))
TukeyHSD(aov(estimate ~ gc, data = filter(zircon_regressions, parameter == "ftcomb")))

# Only gc is significant

# R_Ft 
TukeyHSD(aov(estimate ~ ci, data = filter(zircon_regressions, parameter == "rft")))
TukeyHSD(aov(estimate ~ gem, data = filter(zircon_regressions, parameter == "rft" )))
TukeyHSD(aov(estimate ~ size_cat, data = filter(zircon_regressions, parameter == "rft")))
#TukeyHSD(aov(estimate ~ np_obs, data = filter(zircon_regressions, parameter == "rft")))
TukeyHSD(aov(estimate ~ gc, data = filter(zircon_regressions, parameter == "rft")))

# Only gc is significant


###############################################################

# Print sd of residuals for all relevent groupings

#Apatite

apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft")) |> 
  mutate(size = case_when(geo == "ellipsoid" ~ "Medium & Large", 
                          geo == "hexagonal" ~ as.character(size_cat)), 
         rough = case_when(geo == "ellipsoid" ~ "1 & 2",
                           geo == "hexagonal" ~ as.character(ri))) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))


apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft"), 
         geo == "hexagonal") |>
  mutate(size = case_when(geo == "hexagonal" ~ as.character(size_cat)), 
         rough = case_when(geo == "hexagonal" ~ "1 & 2")) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

apatite_regressions |> 
  select(parameter, geo, size_cat, ri, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft"), 
         geo == "hexagonal") |>
  mutate(size = case_when(geo == "hexagonal" ~ "Medium & Large"), 
         rough = case_when(geo == "hexagonal" ~ "1 & 2")) |> 
  group_by(parameter, geo, size, rough) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

#Zircon

zircon_regressions |> 
  select(parameter, geo, size_cat, ci, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft")) |> 
  mutate(size = case_when(geo == "ellipsoid" ~ "Medium & Large", 
                          geo == "tetragonal" ~ as.character(size_cat)), 
         color = case_when(geo == "ellipsoid" ~ "1 & 2 & 3",
                           geo == "tetragonal" ~ as.character(ci))) |> 
  group_by(parameter, geo, size, color) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

zircon_regressions |> 
  select(parameter, geo, size_cat, ci, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft"), 
         geo == "tetragonal") |>
  mutate(size = case_when(geo == "tetragonal" ~ as.character(size_cat)), 
         color = case_when(geo == "tetragonal" ~ "1 & 2 & 3")) |> 
  group_by(parameter, geo, size, color) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))

zircon_regressions |> 
  select(parameter, geo, size_cat, ci, pdiff) |> 
  filter(parameter %in% c("v", "ftcomb", "rft"), 
         geo == "tetragonal") |>
  mutate(size = case_when(geo == "tetragonal" ~ "Medium & Large"), 
         color = case_when(geo == "tetragonal" ~ "1 & 2 & 3")) |> 
  group_by(parameter, geo, size, color) |> 
  summarize(uncert = round(sd(pdiff),0)) |> 
  mutate(uncert = paste0(uncert, "%"))
         
         
         
         
         