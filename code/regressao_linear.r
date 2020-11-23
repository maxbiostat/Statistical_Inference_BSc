boiling.pt <- read.csv(file = "dados_ponto_fervura.csv") ## Table 11.5, DeGroot (4th ed.)

head(boiling.pt)
plot(pressao ~ pt_fervura, boiling.pt)

linmod <- lm(pressao ~ pt_fervura, data = boiling.pt)

summary(linmod)

### plotting code taken  from https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
temp_var <- predict(linmod, interval="prediction")
new_df <- cbind(boiling.pt, temp_var)

library(ggplot2)
p0 <- ggplot(new_df, aes(pt_fervura, pressao))+
  geom_point() +
  scale_x_continuous("Ponto de fervura da água (ºF)") + 
  scale_y_continuous("Pressão atmosférica (inHG)")  +
  theme_bw(base_size = 16)
p0

p1 <- p0 + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se = TRUE) + 
  theme_bw(base_size = 16)

p1

