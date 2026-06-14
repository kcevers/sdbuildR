# Translate between deSolve and DifferentialEquations.jl solver names

Translate between deSolve and DifferentialEquations.jl solver names, or
validate that a given solver name is recognized in either language. This
is used internally to allow users to specify familiar R solvers when
using Julia for simulation, and to provide warnings when an exact
equivalent is not available.

## Usage

``` r
sim_methods(method, from = NULL, to = NULL)
```

## Arguments

- method:

  Solver name to validate or translate.

- from:

  Source solver family, either `"R"` or `"Julia"`.

- to:

  Target solver family when translating, either `"R"` or `"Julia"`.

## Value

A character scalar (validated or translated solver name), a character
vector of solver names when `method` is omitted, or a named list of
solver names for both languages when called with no arguments.

## Examples

``` r
# List supported solvers
sim_methods()
#> $R
#>  [1] "euler"      "rk2"        "rk4"        "rk23bs"     "ode23"     
#>  [6] "rk45dp6"    "rk45dp7"    "rk45e"      "rk45f"      "rk45ck"    
#> [11] "rk78dp"     "rk78f"      "ode45"      "irk3r"      "irk5r"     
#> [16] "irk4hh"     "irk4l"      "irk6kb"     "irk6l"      "lsoda"     
#> [21] "lsodar"     "lsode"      "lsodes"     "bdf"        "bdf_d"     
#> [26] "vode"       "daspk"      "adams"      "impAdams"   "impAdams_d"
#> [31] "radau"     
#> 
#> $Julia
#>  [1] "Euler()"        "ForwardEuler()" "Midpoint()"     "Heun()"        
#>  [5] "RK4()"          "BS3()"          "Tsit5()"        "Vern6()"       
#>  [9] "Vern7()"        "Vern8()"        "Vern9()"        "Rosenbrock23()"
#> 

# List supported R solvers
sim_methods(from = "R")
#>  [1] "euler"      "rk2"        "rk4"        "rk23bs"     "ode23"     
#>  [6] "rk45dp6"    "rk45dp7"    "rk45e"      "rk45f"      "rk45ck"    
#> [11] "rk78dp"     "rk78f"      "ode45"      "irk3r"      "irk5r"     
#> [16] "irk4hh"     "irk4l"      "irk6kb"     "irk6l"      "lsoda"     
#> [21] "lsodar"     "lsode"      "lsodes"     "bdf"        "bdf_d"     
#> [26] "vode"       "daspk"      "adams"      "impAdams"   "impAdams_d"
#> [31] "radau"     

# List supported Julia solvers
sim_methods(from = "Julia")
#>  [1] "Euler()"        "ForwardEuler()" "Midpoint()"     "Heun()"        
#>  [5] "RK4()"          "BS3()"          "Tsit5()"        "Vern6()"       
#>  [9] "Vern7()"        "Vern8()"        "Vern9()"        "Rosenbrock23()"

# Validate or translate specific solvers
sim_methods("rk4", from = "R", to = "Julia")
#> [1] "RK4()"
```
