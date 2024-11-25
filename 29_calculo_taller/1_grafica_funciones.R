# Valores de la variable independiente
x <- c(-3, -2, -1, 0, 1, 2, 3)

# Función 1: f(x) = 3x + 5
f1 <- 3 * x + 5
plot(x, f1, type = "b", col = "red", xlab = "x", ylab = "f(x)", main = "f(x) = 3x + 5")

# Función 2: f(x) = 2x + 3
f2 <- 2 * x + 3
plot(x, f2, type = "b", col = "blue", xlab = "x", ylab = "f(x)", main = "f(x) = 2x + 3")

# Función 3: f(x) = (5/4)x + 5
f3 <- (5 * x) / 4 + 5
plot(x, f3, type = "b", col = "green", xlab = "x", ylab = "f(x)", main = "f(x) = (5/4)x + 5")

# Función 4: f(x) = x^2 + 2x + 4
f4 <- x^2 + 2 * x + 4
plot(x, f4, type = "b", col = "purple", xlab = "x", ylab = "f(x)", main = "f(x) = x^2 + 2x + 4")

# Función 5: f(x) = x^2 + 2
f5 <- x^2 + 2
plot(x, f5, type = "b", col = "orange", xlab = "x", ylab = "f(x)", main = "f(x) = x^2 + 2")

# Función 6: f(x) = 3x^2 - 12
f6 <- 3 * x^2 - 12
plot(x, f6, type = "b", col = "brown", xlab = "x", ylab = "f(x)", main = "f(x) = 3x^2 - 12")
