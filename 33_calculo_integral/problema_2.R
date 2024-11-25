library(plot3D)

# Create data manually to avoid function coercion issues
x <- seq(0, pi/4, length.out = 100)
theta <- seq(0, 2*pi, length.out = 100)

# Create meshgrid matrices
X <- matrix(0, nrow=length(x), ncol=length(theta))
Y <- matrix(0, nrow=length(x), ncol=length(theta))
Z1 <- matrix(0, nrow=length(x), ncol=length(theta))
Z2 <- matrix(0, nrow=length(x), ncol=length(theta))

# Manually fill matrices
for(i in 1:length(x)) {
  for(j in 1:length(theta)) {
    X[i,j] <- x[i] * cos(theta[j])
    Y[i,j] <- x[i] * sin(theta[j])
    Z1[i,j] <- x[i] * sin(x[i])
    Z2[i,j] <- x[i] * cos(x[i])
  }
}


# Top surface
persp3D(X, Y, Z1, 
        main = "Top Surface: x*sin(x)", 
        theta = 45, 
        phi = 30, 
        col = "lightblue", 
        border = "blue")

# Bottom surface
persp3D(X, Y, Z2, 
        main = "Bottom Surface: x*cos(x)", 
        theta = 45, 
        phi = 30, 
        col = "lightgreen", 
        border = "green")

# Solid of revolution
persp3D(X, Y, Z1 - Z2, 
        main = "Solid of Revolution", 
        theta = 45, 
        phi = 30, 
        col = "pink", 
        border = "red")
