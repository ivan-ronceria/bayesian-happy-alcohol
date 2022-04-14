#Bayes Project
library(rethinking)
library(readxl)
set.seed(0)

df<-read_xlsx("C:/Users/Ronceria/Desktop/HappyAlcohol.xlsx")
df<-data.frame(df)
summary(df)


# #Is alcohol consumption of a country a good predictor of a country's happiness score?
mod1 <- map(
  alist(
    HappinessScore ~ dnorm(mu, sigma),
    mu <- a + bb * Beer_PerCapita + bs * Spirit_PerCapita + bw * Wine_PerCapita,
    a ~ dnorm(5,2),
    bb ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bw ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = df
)
# #As we can see here, alcohol consumption are terrible predictors since their beta estimates include 0 in their credible intervals
# precis(mod1)
# plot(precis(mod1))
# 
# #Propose a new model, including GDP and HDI
# mod2 <- map(
#   alist(
#     HappinessScore ~ dnorm(mu, sigma),
#     mu <- a + bb * Beer_PerCapita + bs * Spirit_PerCapita + bw * Wine_PerCapita + bHDI * HDI + bGDP * GDP_PerCapita,
#     a ~ dnorm(5,2),
#     bb ~ dnorm(0, 1),
#     bs ~ dnorm(0, 1),
#     bw ~ dnorm(0, 1),
#     bHDI ~ dnorm(0, 1),
#     bGDP ~ dnorm(0, 1),
#     sigma ~ dunif(0, 10)
#   ),
#   data = df
# )
# 
# #NaN are produced for the sigma estimate, indicated exact convergence. 
# #However, the intervals for the beta estimates still contain 0.
# #Again, this model may not be great predictor either.
# precis(mod2)
# 
# #Is GDP and HDI the best predictors for HappinessScore?
# mod3 <- map(
#   alist(
#     HappinessScore ~ dnorm(mu, sigma),
#     mu <- a + bHDI * HDI + bGDP * GDP_PerCapita,
#     a ~ dnorm(5,2),
#     bHDI ~ dnorm(0, 1),
#     bGDP ~ dnorm(0, 1),
#     sigma ~ dunif(0, 10)
#   ),
#   data = df
# )

#Interestingly, this is also terrible for prediction. It appears standardization of the variables is required
precis(mod3)

#Scale transformations
df$HDI.s = scale(df$HDI)
df$GDP_PerCapita.s = scale(df$GDP_PerCapita)
df$Beer_PerCapita.s = scale(df$Beer_PerCapita)
df$Wine_PerCapita.s = scale(df$Wine_PerCapita)
df$Spirit_PerCapita.s = scale(df$Spirit_PerCapita)

#Now we propose new models
mod4 <- map(
  alist(
    HappinessScore ~ dnorm(mu, sigma),
    mu <- a + bb * Beer_PerCapita.s + bs * Spirit_PerCapita.s + bw * Wine_PerCapita.s,
    a ~ dnorm(5,2),
    bb ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bw ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = df
)

#Better convergence, but it appears that spirit consumption remains a terrible predictor
precis(mod4)
plot(precis(mod4))

#Lets propose the full model with transformed variables.
#Let's regularize as well with our priors
mod5 <- map(
  alist(
    HappinessScore ~ dnorm(mu, sigma),
    mu <- a + bb * Beer_PerCapita.s + bs * Spirit_PerCapita.s + bw * Wine_PerCapita.s + bHDI * HDI.s + bGDP * GDP_PerCapita.s,
    a ~ dnorm(5,2),
    bb ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bw ~ dnorm(0, 1),
    bHDI ~ dnorm(0, 1),
    bGDP ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = df
)

#Sigma improved, the alpha estimate remains the roughtly the same, and spirit consumption continues to be a weak predictor
precis(mod5)
plot(precis(mod5))

#Lets try again the model with just just GDP and HDI
mod6 <- map(
  alist(
    HappinessScore ~ dnorm(mu, sigma),
    mu <- a + bHDI * HDI.s + bGDP * GDP_PerCapita.s,
    a ~ dnorm(5,2),
    bHDI ~ dnorm(0, 1),
    bGDP ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = df
)

#Interestingly, with just HDI and GDP, our sigma is only 0.02 more than the full model
#At this point, one could conclude that overall alcohol consumption is a weak predictor for happiness.
#It is perhaps spurious at best.
plot(precis(mod6))

#check posterior of model
postcheck(modHam, 1e5)

#Now let's us WAIC to average the models
#WAIC even concludes that alcohol consumption by itself as a model is not great as well
#It assigns zero weight to the just alcohol model. 
(models<-compare(mod4, mod5, mod6))

#We can observe how the estimates change accross all the models
plot(coeftab(mod4,mod5,mod6))

#With a pairs plot, one can see that there a strong relationship between Happiness and HDI + GDP
pairs(data= df,HappinessScore ~ Beer_PerCapita.s +  Spirit_PerCapita.s + Wine_PerCapita.s +  HDI.s + GDP_PerCapita.s)

#Prediction
beer.seq <- seq(-3, 3, length.out = 100)
wine.seq <- seq(-3, 3, length.out = 100)
sp.seq <- seq(-3,3, length.out = 100)
HDI.seq <- seq(-3,3)


d.predict<-list(
  HappinessScore = rep(0, 100),
  Beer_PerCapita.s = 
)

pairs(mod0)
#converts glm to bayes
#glimmer()

#hamiltonian monte carlo setup
df_trim <-list(HappinessScore = df$HappinessScore, bsBeer = df$Beer_PerCapita.s
               , bsWine = df$Wine_PerCapita.s, bsSpirit = df$Spirit_PerCapita.s,
               GDP = df$GDP_PerCapita.s, HDI = df$HDI.s)

modHam = map2stan(
  alist(
    HappinessScore ~ dnorm(mu, sigma),
    mu <- a + bb * bsBeer + bs * bsSpirit + bw * bsWine + bHDI * HDI + bGDP * GDP,
    a ~ dnorm(5,2),
    bb ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bw ~ dnorm(0, 1),
    bHDI ~ dnorm(0, 1),
    bGDP ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = df_trim, chains = 6 , cores = 6, iter = 1000
)


pairs(modHam)
precis(modHam)
show(modHam)
traceplot(modHam)
plot(modHam)

samples<-data.frame(extract.samples(modHam))
summary(samples)

mu.samples <- samples$a + samples$bGDP
HDPI(mu.samples)
