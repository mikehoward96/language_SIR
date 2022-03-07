#basic SIR model
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

parameters_values <- c(
  beta  = 0.004, # infectious contact rate (/person/day)
  gamma = 0.5    # recovery rate (/day)
)

initial_values <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

time_values <- seq(0, 10) # days

sir_values_1 <- deSolve::ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1 <- as.data.frame(sir_values_1)

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")
