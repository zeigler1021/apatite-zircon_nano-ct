
library(tidyverse)

# Glotzbach et al. (2019)

glotzv <- as.data.frame(tabulizer::extract_tables("images-imports/glotzbach2019.pdf", pages = 7))

glotzbach_vol <- glotzv |> 
  slice(1:24) |> 
  separate(X1, into = c("grain", "v_3d"), sep = " ") |> 
  separate(X3, into = c("v_2d", "null"), sep = " ") |> 
  select(grain, v_3d, v_2d)

glotzft <- tabulizer::extract_tables("images-imports/glotzbach2019.pdf", pages = 8)
glotzft <- as.data.frame(glotzft[[1]])

glotzbach_ft <- glotzft |> 
  slice(2:25) |> 
  rename(grain = V1, ft_3d = V2, ft_2d = V3) |> 
  select(grain, ft_3d, ft_2d)

glotzbach <- left_join(glotzbach_vol, glotzbach_ft, by = "grain")

glotzbach <- glotzbach |> 
  mutate_if(is.character, as.numeric) |> 
  mutate(pdiff_v = abs(v_2d - v_3d)/v_2d *100, 
         pdiff_ft = abs(ft_2d - ft_3d)/ft_2d * 100)

mean(abs(glotzbach$pdiff_ft))
sd(abs(glotzbach$pdiff_ft))

mean(abs(glotzbach$pdiff_v))
sd(abs(glotzbach$pdiff_v))

mean(glotzbach$v_3d/glotzbach$v_2d)
sd(glotzbach$v_3d/glotzbach$v_2d)

mean(glotzbach$ft_3d/glotzbach$ft_2d)
sd(glotzbach$ft_3d/glotzbach$ft_2d)

# Evans et al. (2008)
evans_raw <- as.data.frame(tabulizer::extract_tables("images-imports/evans2008.pdf", pages = 6)) |> 
  select(-1) 

evansft <- evans_raw |> 
  slice(4:13) |> 
  separate(X2, into = c("grain", "num", "ft_2d_1", "ft_2d_2"), sep = " ") |> 
  slice(-3) |> 
  rename(ft_2d_3 = X3, ft_2d_4 = X4, ft_3d = X5) |> 
  select(1:7) |> 
  unite("grain", grain, num, sep = "") |> 
  pivot_longer(2:5, values_to = "ft_2d") |> 
  mutate(ft_3d = as.numeric(ft_3d), 
         ft_2d = as.numeric(ft_2d)) |> 
  mutate(pdiff_avg = abs(ft_2d-ft_3d)/((ft_2d + ft_3d)/2)*100, 
         pdiff = abs(ft_2d - ft_3d)/ft_2d *100) |> 
  group_by(grain) |>
  summarize(pdiffmean = mean(pdiff), 
            pdiffsd = sd(pdiff)) 

evansv <- evans_raw |> 
  slice(15:24) |> 
  separate(X2, into = c("grain", "num", "v_2d_1", "v_2d_2"), sep = " ") |> 
  #slice(-3) |> 
  rename(v_2d_3 = X3, v_2d_4 = X4, v_3d = X5) |> 
  select(1:7) |> 
  slice(-4) |>  #remove apatite4 
  unite("grain", grain, num, sep = "") |> 
  pivot_longer(2:5, values_to = "v_2d") |> 
  mutate(v_3d = as.numeric(v_3d), 
         v_2d = as.numeric(v_2d)) |> 
  mutate(pdiff_avg = abs(v_2d-v_3d)/((v_3d + v_2d)/2)*100, 
         pdiff = abs(v_2d - v_3d)/v_2d *100) |> 
  group_by(grain) |>
  summarize(pdiffmean = mean(pdiff)) 

evansv |> slice(1:4) |> summarize(mean(pdiffmean), sd(pdiffmean)) #apatite volume
evansv |> slice(5:9) |> summarize(mean(pdiffmean), sd(pdiffmean)) #zircon volume

evansft |> slice(1:4) |> summarize(mean(pdiffmean), sd(pdiffmean)) #apatite ft
evansft |> slice(5:9) |> summarize(mean(pdiffmean), sd(pdiffmean)) #zircon ft

## average 2d/3d for FT apatite and zircon 
evans_raw |> 
  slice(4:13) |> 
  separate(X2, into = c("grain", "num", "ft_2d_1", "ft_2d_2"), sep = " ") |> 
  slice(-3) |> 
  rename(ft_2d_3 = X3, ft_2d_4 = X4, ft_3d = X5) |> 
  select(1:7) |> 
  unite("grain", grain, num, sep = "") |> 
  pivot_longer(2:5, values_to = "ft_2d") |> 
  mutate(ft_3d = as.numeric(ft_3d), 
         ft_2d = as.numeric(ft_2d)) |> 
  group_by(grain) |> 
  summarize(meanft = mean(ft_3d/ft_2d), 
            sdft = sd(ft_3d/ft_2d)) |> 
  separate(grain, into = c("mineral", "num"), sep = "\\d") |> 
  group_by(mineral) |> 
  summarize(meanft = mean(meanft), 
            sdft = mean(sdft))

## avg 3d/2d for volume apatite and zircon
evans_raw |> 
  slice(15:24) |> 
  separate(X2, into = c("grain", "num", "v_2d_1", "v_2d_2"), sep = " ") |> 
  #slice(-3) |> 
  rename(v_2d_3 = X3, v_2d_4 = X4, v_3d = X5) |> 
  select(1:7) |> 
  slice(-4) |>  #remove apatite4 
  unite("grain", grain, num, sep = "") |> 
  pivot_longer(2:5, values_to = "v_2d") |> 
  mutate(v_3d = as.numeric(v_3d), 
         v_2d = as.numeric(v_2d)) |> 
  group_by(grain) |> 
  summarize(meanv = mean(v_3d/v_2d), 
            sdv = sd(v_3d/v_2d)) |> 
  separate(grain, into = c("mineral", "num"), sep = "\\d") |> 
  group_by(mineral) |> 
  summarize(meanv = mean(meanv), 
            sdv = mean(sdv))

# Zeigler et al. 

ap_comp <- apatite |> 
  select(ftcomb_3d, ftcomb_max, v_3d, v_max, rft_max, rft_3d) |>  
  summarize(pdiff_ft = abs(ftcomb_max - ftcomb_3d)/ftcomb_max*100, 
            pdiff_v = abs(v_max - v_3d)/v_max *100,
            pdiff_rft = abs(rft_max - rft_3d)/rft_max*100)

apply(ap_comp, 2, mean)
apply(ap_comp, 2, sd)

zir_comp <- zircon |> 
  select(grain, ftcomb_3d, ftcomb_both, v_3d, v_both, rft_both, rft_3d) |>  
  summarize(pdiff_ft = abs(ftcomb_both - ftcomb_3d)/ftcomb_both*100, 
            pdiff_v = abs(v_both - v_3d)/v_both*100,
            pdiff_rft = abs(rft_both - rft_3d)/rft_both*100)

apply(zir_comp, 2, mean)
apply(zir_comp, 2, sd)


## Hexagonal Only 
ap_comp <- apatite |> 
  select(geo, ft238_3d, ft238_max, v_3d, v_max, rft_max, rft_3d) |>  
  filter(geo == "hexagonal") |> 
  summarize(pdiff_ft = (abs(ft238_max - ft238_3d)/ft238_max)*100, 
            pdiff_v = (abs(v_max - v_3d)/v_max)*100,
            pdiff_rft = (abs(rft_max - rft_3d)/rft_max)*100)

apply(ap_comp, 2, mean)
apply(ap_comp, 2, sd)

## Ellipsoid Only 
ap_comp <- apatite |> 
  select(geo, ft238_3d, ft238_max, v_3d, v_max, rft_max, rft_3d) |>  
  filter(geo == "ellipsoid") |> 
  summarize(pdiff_ft = (abs(ft238_max - ft238_3d)/ft238_max)*100, 
            pdiff_v = (abs(v_max - v_3d)/v_max)*100,
            pdiff_rft = (abs(rft_max - rft_3d)/rft_max)*100)

apply(ap_comp, 2, mean)
apply(ap_comp, 2, sd)

## avg 3d/2d for apatite ft and volume (combined hex and ellip)
apatite |> 
  select(geo, ft238_3d, ft238_max, v_3d, v_max, rft_max, rft_3d) |>  
  summarize(mean(ft238_3d/ft238_max),
            mean(v_3d/v_max),
            mean(rft_3d/rft_max), 
            sd(ft238_3d/ft238_max),
            sd(v_3d/v_max),
            sd(rft_3d/rft_max))

# hexagonal only
apatite |> 
  select(geo, ft238_3d, ft238_max, v_3d, v_max, rft_max, rft_3d) |>  
  filter(geo == "hexagonal") |> 
  summarize(mean(ft238_3d/ft238_max),
            mean(v_3d/v_max),
            mean(rft_3d/rft_max), 
            sd(ft238_3d/ft238_max),
            sd(v_3d/v_max),
            sd(rft_3d/rft_max))

## ellipsoid only
apatite |> 
  select(geo, ft238_3d, ft238_max, v_3d, v_max, rft_max, rft_3d) |>  
  filter(geo == "ellipsoid") |> 
  summarize(mean(ft238_3d/ft238_max),
            mean(v_3d/v_max),
            mean(rft_3d/rft_max), 
            sd(ft238_3d/ft238_max),
            sd(v_3d/v_max),
            sd(rft_3d/rft_max))
















