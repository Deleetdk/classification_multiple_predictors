
# libs --------------------------------------------------------------------

library(pacman)
p_load(psych, ggplot2, plyr, gtools)

# functions ---------------------------------------------------------------

get_error_beta = function(x) {
  return(sqrt(1 - x^2))
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

rotate <- function(x) t(apply(x, 2, rev))

# settings ----------------------------------------------------------------

n = 1e5
X1 = .4
X1_e = get_error_beta(X1)
X2 = .3
X2_e = get_error_beta(X2)
X3 = .2
X3_e = get_error_beta(X3)

#generate
set.seed(45)
{
#target vector
Y = sample(0:1, n, T)
#pred 1
X1 = Y * X1 + rnorm(n) * X1_e
#pred 2
X2 = Y * X2 + rnorm(n) * X2_e
#pred 3
X3 = Y * X3 + rnorm(n) * X3_e
}

d = data.frame(Y, X1, X2, X3)
cor(d)

#fit
#fit = glm(Y ~ X1 + X2 + X3, data = d, family = "binomial")
fit_X1 = glm(Y ~ X1, data = d, family = "binomial")
fit_X2 = glm(Y ~ X2, data = d, family = "binomial")
fit_X3 = glm(Y ~ X3, data = d, family = "binomial")

#probable scores
Y_hat = fit$fitted.values

#most likely scores
Y_hat_rounded = round(fit$fitted.values)

#proportion table
prop_table = prop.table(table(Y_hat_rounded, Y))

#add margin sums
prop_table = addmargins(prop_table)

#as df
prop_table = as.data.frame(matrix(as.vector(prop_table), nrow = 3))

#names
colnames(prop_table) = rownames(prop_table) = c("0", "1", "sum")

d2 = data.frame(permutations(2, 2, v = 0:1, repeats.allowed = T),
                round(as.vector(prop.table(table(Y_hat_rounded, Y))), digits = 3))

colnames(d2) = c("x", "y", "prob")
d2 = mutate(d2,
            x_pos = x - .5,
            y_pos = y - .5,
            prob_p = percent(prob, digits = 1))

#X1
d_X1 = data.frame(X1 = seq(min(d$X1), max(d$X1), sd(d$X1)/100))
d_X1$fit = predict(fit_X1, newdata = d_X1, type = "link")
d_X1$fit = fit_X1$family$linkinv(d_X1$fit) #rescale to 0-1

#X2
d_X2 = data.frame(X2 = seq(min(d$X2), max(d$X2), sd(d$X2)/100))
d_X2$fit = predict(fit_X2, newdata = d_X2, type = "link")
d_X2$fit = fit_X2$family$linkinv(d_X2$fit) #rescale to 0-1

#X3
d_X3 = data.frame(X3 = seq(min(d$X3), max(d$X3), sd(d$X3)/100))
d_X3$fit = predict(fit_X3, newdata = d_X3, type = "link")
d_X3$fit = fit_X3$family$linkinv(d_X3$fit) #rescale to 0-1


ggplot(d, aes(X1, Y)) +
  geom_point() +
  geom_line(data = d_X1, aes(x = X1, y = fit), color = "red", size = 1)

ggplot(d, aes(X2, Y)) +
  geom_point() +
  geom_line(data = d_X2, aes(x = X2, y = fit), color = "green", size = 1)

ggplot(d, aes(X3, Y)) +
  geom_point() +
  geom_line(data = d_X3, aes(x = X3, y = fit), color = "blue", size = 1)
