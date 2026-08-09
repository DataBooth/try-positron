# Load necessary library
library(stats)

# Function to calculate N(x)
N <- function(x) {
  pnorm(x)
}

# Function to calculate d1 and d2
d1 <- function(S0, X, r, sigma, T) {
  (log(S0 / X) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
}

d2 <- function(S0, X, r, sigma, T) {
  d1(S0, X, r, sigma, T) - sigma * sqrt(T)
}

# Function to calculate Black-Scholes call option price
black_scholes_call <- function(S0, X, r, sigma, T) {
  C <- S0 *
    N(d1(S0, X, r, sigma, T)) -
    X * exp(-r * T) * N(d2(S0, X, r, sigma, T))
  return(C)
}

# Function to calculate Black-Scholes put option price
black_scholes_put <- function(S0, X, r, sigma, T) {
  P <- X *
    exp(-r * T) *
    N(-d2(S0, X, r, sigma, T)) -
    S0 * N(-d1(S0, X, r, sigma, T))
  return(P)
}

# Example usage
S0 <- 100 # Current stock price
X <- 100 # Strike price
r <- 0.05 # Risk-free interest rate
sigma <- 0.2 # Volatility
T <- 1 # Time to maturity (in years)

call_price <- black_scholes_call(S0, X, r, sigma, T)
put_price <- black_scholes_put(S0, X, r, sigma, T)

print(paste("Black-Scholes Call Option Price:", call_price))
print(paste("Black-Scholes Put Option Price:", put_price))
