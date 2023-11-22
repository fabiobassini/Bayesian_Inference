# Impostazioni della simulazione
N <- 100  # Numero totale di lanci della moneta

# Genera un valore per Theta dalla distribuzione U(0,1)
Theta <- runif(1, min = 0, max = 1)

# Genera un dato K dalla distribuzione binomiale B(N, Theta)
K <- rbinom(1, size = N, prob = Theta)

# Simula l'inferenza bayesiana
# Definisci la prior per Theta (uniforme)
prior_theta <- function(theta) {
  ifelse(theta >= 0 & theta <= 1, 1, 0)
}

# Calcola la likelihood
likelihood <- function(theta, k, n) {
  choose(n, k) * theta^k * (1 - theta)^(n - k)
}

# Calcola il posterior non normalizzato
posterior_non_normalizzato <- function(theta, k, n) {
  prior_theta(theta) * likelihood(theta, k, n)
}

# Calcola la costante di normalizzazione
normalization_constant <- integrate(function(t) posterior_non_normalizzato(t, K, N), 0, 1)$value

# Definisci il posterior normalizzato
posterior_normalizzato <- function(theta, k, n) {
  posterior_non_normalizzato(theta, k, n) / normalization_constant
}

# Esegui l'inferenza bayesiana per ottenere il posterior normalizzato
theta_values <- seq(0, 1, length.out = 100)
posterior_values <- posterior_normalizzato(theta_values, K, N)

# Plotta il prior, la likelihood e il posterior normalizzato
plot(theta_values, prior_theta(theta_values), type = "l", col = "blue", lty = 2, lwd = 2, ylim = c(0, max(prior_theta(theta_values), likelihood(theta_values, K, N), posterior_values)))
lines(theta_values, likelihood(theta_values, K, N), col = "red", lty = 2, lwd = 2)
lines(theta_values, posterior_values, col = "green", lty = 1, lwd = 2)
legend("topright", legend = c("Prior", "Likelihood", "Posterior Normalizzato"), col = c("blue", "red", "green"), lty = c(2, 2, 1), lwd = 2)
