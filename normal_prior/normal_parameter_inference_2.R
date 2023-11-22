# Definisci la funzione della pdf della distribuzione normale
pdf_normale <- function(x, mean, sd) {
  exp(-0.5 * ((x - mean) / sd)^2) / (sd * sqrt(2 * pi))
}

# Genera un valore per Theta dalla distribuzione N(0,1) - (sconosciuto)
Theta <- rnorm(1, mean = 0, sd = 1)

# Genera un valore per W dalla distribuzione N(0,1) - (sconosciuto) 
W <- rnorm(1, mean = 0, sd = 1)

# Calcola il dato X = Theta + W
X <- Theta + W

# Simula l'inferenza bayesiana
# Definisci la pdf per Theta (distribuzione normale)
pdf_prior_theta <- function(theta) {
  pdf_normale(theta, mean = 0, sd = 1)
}

# Calcola la likelihood
pdf_likelihood <- function(theta, x) {
  pdf_normale(x - theta, mean = 0, sd = 1)
}

# Calcola il posterior non normalizzato
pdf_posterior_non_normalizzato <- function(theta, x) {
  pdf_prior_theta(theta) * pdf_likelihood(theta, x)
}

# Calcola la costante di normalizzazione
normalization_constant <- integrate(function(t) pdf_posterior_non_normalizzato(t, X), -Inf, Inf)$value

# Definisci il posterior normalizzato
pdf_posterior_normalizzato <- function(theta, x) {
  pdf_posterior_non_normalizzato(theta, x) / normalization_constant
}

# Esegui l'inferenza bayesiana per ottenere la pdf del posterior normalizzato
theta_values <- seq(-3, 3, length.out = 100)
posterior_values_normalizzato <- pdf_posterior_normalizzato(theta_values, X)

# Plotta la pdf del prior, la pdf della likelihood e la pdf del posterior normalizzato
plot(theta_values, pdf_prior_theta(theta_values), type = "l", col = "blue", lty = 2, lwd = 2, ylim = c(0, max(pdf_prior_theta(theta_values), pdf_likelihood(theta_values, X), posterior_values_normalizzato)))
lines(theta_values, pdf_likelihood(theta_values, X), col = "red", lty = 2, lwd = 2)
lines(theta_values, posterior_values_normalizzato, col = "green", lty = 1, lwd = 2)
abline(v = X, col = "orange")
legend("topright", legend = c("Prior", "Likelihood", "Posterior Normalizzato", "Data"), col = c("blue", "red", "green", "orange"), lty = c(2, 2, 1, 1), lwd = 2)
