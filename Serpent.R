library(plotly)

Initial_E <- 10^8 # The units do not matter since they would be in a ratio regardless.

microscopic_cross_section <- 585
micro_constant <- 1

A <- 115 # Mass of target


Min_energy_limit <- ((A-1)/(A+1))**2

number_of_particles <- 10000


x <- vector("numeric", number_of_particles)
y <- vector("numeric", number_of_particles)
z <- vector("numeric", number_of_particles)
E <- vector("numeric", number_of_particles)

for (i in 1:number_of_particles){
  E[i] <- Initial_E
}


#print(E)
# The above three statements say that the neutron is initially at (0,0,0).
for (i in 1:number_of_particles){
  status <- 0  #Says the neutron currently is un-absorbed
  sigma_a <- 0.02
  sigma_s <- 4
  sigma_t <- sigma_a + sigma_s
  while (status==0) {
    #Here I am going to determine variables of a 3-D random walk. The step size is variable, however I am considering
    #that the nucleus is uniform. Hence values must be uniformly spread out.
    s <- (1/sigma_t)*log(runif(1))
    theta <- runif(1, min=-pi, max=pi)
    phi <- runif(1, min=0, max= 2*pi)
    
    dx <- s*sin(theta)*cos(phi)
    dy <- s*sin(theta)*sin(phi)
    dz <- s*cos(theta)
    
    x[i] <- x[i] + dx
    y[i] <- y[i] + dy
    z[i] <- z[i] + dz
    
    New_Energy <- E[i]*runif(1,min = Min_energy_limit,max = 1)
    sigma_a <- sigma_a*sqrt(E[i]/New_Energy)
    sigma_s <- sigma_t - sigma_a
    E[i] <- New_Energy # Reinitialize this energy as the current energy of the neutron
    if (runif(1,min=0,max=1) < sigma_a/sigma_t) {
      status <- 1
    }
  }
}


hist(E, xlab = "Energy of neutrons", main = "Energy distribution of neutrons", pch=18)
plot_ly(x=x,y=y,z=z , type = "scatter3d", mode="markers", color = E)
