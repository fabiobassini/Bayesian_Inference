# Genera un valore per Theta dalla distribuzione N(0,1)
Theta <- rnorm(1, mean = 0, sd = 1)

# Genera un valore per W dalla distribuzione N(0,1)
W <- rnorm(1, mean = 0, sd = 1)

# Calcola il dato X = Theta + W
X <- Theta + W

# Simula l'inferenza bayesiana
# Definisci la prior per Theta (distribuzione normale)
prior_theta <- function(theta) {
  dnorm(theta, mean = 0, sd = 1)
}

# Calcola la likelihood
likelihood <- function(theta, x) {
  dnorm(x - theta, mean = 0, sd = 1)
}

# Calcola il posterior
posterior <- function(theta, x) {
  prior_theta(theta) * likelihood(theta, x)
}

# Definisci la funzione del posterior con normalizzazione
posterior_normalizzato <- function(theta, x) {
  unnormalized_posterior <- prior_theta(theta) * likelihood(theta, x)
  normalization_constant <- integrate(function(t) prior_theta(t) * likelihood(t, x), -Inf, Inf)$value
  return(unnormalized_posterior / normalization_constant)
}

# Esegui l'inferenza bayesiana per ottenere il posterior normalizzato
posterior_values_normalizzato <- posterior_normalizzato(theta_values, X)


# Esegui l'inferenza bayesiana per ottenere il posterior
theta_values <- seq(-3, 3, length.out = 100)
posterior_values <- posterior(theta_values, X)


# Plotta il prior, la likelihood e il posterior normalizzato
plot(theta_values, prior_theta(theta_values), type = "l", col = "blue", lty = 2, lwd = 2, ylim = c(0, max(prior_theta(theta_values), likelihood(theta_values, X), posterior_values_normalizzato)))
lines(theta_values, posterior_values_normalizzato, col = "green", lty = 1, lwd = 2)
abline(v = X, col = "orange", lty = 2, lwd = 2)
legend("topright", legend = c("Prior", "Data", "Posterior Normalizzato"), col = c("blue", "red", "green"), lty = c(2, 2, 1), lwd = 2)