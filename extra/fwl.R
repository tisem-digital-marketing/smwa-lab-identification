# step 1: Start with the raw data
df <-
    df %>%
    mutate(hair_binary = if_else(hair == "Brown", 1 , 0)) 

df %>%
    ggplot() + 
    geom_jitter(aes(x = hair, y = log_income, color = college), alpha = 0.5) +
    geom_boxplot(aes(x = hair, y = log_income)) +
    scale_color_colorblind()


# step 2: figure out what part of hair color is explained by college
mod_x <- lm(hair_binary ~ college, data = df)
tidy(mod_x)
## brown haired ppl less likely to be in college in the data 

## step 3: get the remaining variation in hair color not due to college
df <-
    df %>%
    mutate(resid_x = mod_x$residuals)

resid_x <- mod_x$residuals

## step 4: figure out what part of log_income is explained by college 
mod_y <- lm(log_income ~ college, data = df)
tidy(mod_y)

## step 5: get the remaining variation in hair color not due to college
df <-
    df %>%
    mutate(resid_y = mod_y$residuals)

df %>%
    ggplot() + 
    geom_jitter(aes(x = resid_x, y = resid_y), alpha = 0.5) +
    scale_color_colorblind()

tidy(lm(resid_y ~ resid_x, data = df), digits = 2) %>%
    mutate_if(is.numeric, round, 4)