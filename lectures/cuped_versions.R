library(tidyverse)
library(estimatr)

n_obs = 1000

set.seed(777)
data = tibble(x = rnorm(n_obs),
              group = sample(c('treatment', 'control'), n_obs, replace = TRUE),
              eps = rnorm(n_obs) * abs(3 * x + 2 * (group == 'treatment')),
              y = 3 + 2 * (group == 'treatment') + 5 * x + 3 * x ^ 2 + eps)

# model is heteroskedastic + includes x ^ 2

data



# way 1a. t-test
mod_1a = t.test(data = data, y ~ group, var.equal = TRUE)
print(mod_1a)

# theory: unbiased point estimator, wrong ci

# way 1b. regression + classical ci
mod_1b = lm(data = data, y ~ group)
mod_1b
summary(mod_1b)
confint(mod_1b)

# theory: unbiased point estimator, wrong ci
# equivalent to way 1a

# way 2. welch test

mod_2 = t.test(data = data, y ~ group, var.equal = FALSE)
print(mod_2)

# theory: unbiased point estimator, asy correct CI

# way 3. robust se

mod_3 = lm_robust(data = data, y ~ group, se_type = 'HC3')
coef(mod_3)
confint(mod_3)

# theory: unbiased point estimator, asy correct CI


# way 4. regression with covariate + classic ci
mod_4 = lm(data = data, y ~ group + x)
coef(mod_4)
confint(mod_4)



# way 5. regression with covariate + hetero ci
mod_5 = lm_robust(data = data, y ~ group + x, se_type = 'HC3')
coef(mod_5)
confint(mod_5)


# way 6. two regressions + classic ci
mod_6_step_1 = lm(data = data, y ~ x)
data$resid_a = resid(mod_6_step_1)

mod_6 = lm(data = data, resid_a ~ group)
coef(mod_6)
confint(mod_6)

# way 7. two regressions + classic ci
mod_7_step_1 = lm(data = data, y ~ x + group)
coef(mod_7_step_1)
data$resid_b = data$y - coef(mod_7_step_1)['x'] * data$x

mod_7 = lm(data = data, resid_b ~ group)
coef(mod_7)
confint(mod_7)


