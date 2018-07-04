# LabStat2 20.11.2016

rm(list = ls()) 


# install.packages("PBImisc")
# install.packages("ggplot2")
library(PBImisc)
library(ggplot2)

data(heights)

# Fajny podcast "Not so strandard deviations" - episode 11

str(heights)
summary(heights)

ggplot(heights, aes(x = Husband, y = Wife)) + geom_point()

# wzrost_¿ony ~ f(wzrost_mê¿a) + X; gdzie X~N(mi, sigma^2)
# wzrost_¿ony ~ beta_0 + beta_1*wzrost_mê¿a + X; X~N(o, sigma^2)
# 
# Pytanie: Czy to w ogóle ma sens, czy model liniowy jest odpowiedni?
#          Czy model dobrze przewiduje dane?


# Na piechotê 
x = cbind(1, heights$Husband)
y = cbind(heights$Wife)
n = length(heights$Husband)
beta_hat = solve(t(x) %*% x) %*% t(x) %*% y

y_hat = x %*% beta_hat

r = y - y_hat
summary(r)

sigma_squered = (t(y - x %*% beta_hat)) %*% (y - x %*% beta_hat)/(n-2)
sgm           = sqrt(sigma_squered)

sigma_multi     = as.numeric(sigma_squered) * solve(t(x) %*% x)
standard_errors = sqrt(diag(sigma_multi))

m       = solve(t(x) %*% x)
t_value = beta_hat[1]/(sqrt(sigma_squered) * sqrt(m[1,1]))

p_value = 2*(1-pt(abs(t_value), df = n-2))

# Na szybko
lmwife = lm(Wife~Husband, data = heights)
summary(lmwife)

# Coœ nowego - porównanie z modelem zerowym

lmwifeone = lm(Wife~1, data = heights)
summary(lmwifeone)

RSS_0 = sum(residuals(lmwifeone)^2)
RSS_1 = sum(residuals(lmwife)^2)

f_value = ((RSS_0-RSS_1)/1)/(RSS_1/(n-2))

1-pf(f_value, df1 = 1, df2 = n-2)

R_squered     = (RSS_0 - RSS_1)/RSS_0
R_squered_adj = 1-(RSS_1/(n-2))/(RSS_0/(n-1))
