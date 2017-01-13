#install.packages("simmer")
library(simmer)

t0 <- trajectory() %>%
    seize("resource", 1) %>%
    timeout(function() rexp(1, 2)) %>%
    release("resource", 2)

t0
## trajectory: anonymous, 3 activities
## { Activity: Seize        | resource: resource | amount: 1 }
## { Activity: Timeout      | delay: 0x7fdfa229cfb8 }
## { Activity: Release      | resource: resource | amount: 2 }
t0[c(3, 1)]
## trajectory: anonymous, 2 activities
## { Activity: Release      | resource: resource | amount: 2 }
## { Activity: Seize        | resource: resource | amount: 1 }

t0 <- trajectory() %>%
    seize("res0", 1) %>%
    branch(function() 1, c(TRUE, FALSE),
           trajectory() %>%
               clone(2,
                     trajectory() %>%
                         seize("res1", 1) %>%
                         timeout(1) %>%
                         release("res1", 1),
                     trajectory() %>%
                         trap("signal",
                              handler=trajectory() %>%
                                  timeout(1)) %>%
                         timeout(1)),
           trajectory() %>%
               set_attribute("dummy", 1) %>%
               seize("res2", function() 1) %>%
               timeout(function() rnorm(1, 20)) %>%
               release("res2", function() 1) %>%
               release("res0", 1) %>%
               rollback(11)) %>%
    synchronize() %>%
    rollback(2) %>%
    release("res0", 1)

#install.packages("simmer.plot")
library(simmer.plot)

plot(t0)


#devtools::install_github("r-simmer/simmer.optim")

library(simmer)
library(dplyr)

t0<-trajectory() %>%
    seize("nurse") %>%
    timeout(function() rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function() rpois(1, 20)) %>%
    release("cardiologist")

envs <- lapply(1:100, function(i){
    simmer() %>%
        add_generator("patient", t0, at(seq(0,60*4, 15))) %>%
        add_resource("nurse", 1) %>%
        add_resource("cardiologist", 1) %>%
        run()
})

plot_evolution_arrival_times(envs, "waiting_time")
