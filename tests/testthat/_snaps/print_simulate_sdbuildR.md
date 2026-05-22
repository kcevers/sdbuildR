# print.simulate_sdbuildR() snapshot: successful simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: Susceptible-Infected-Recovered (SIR) -------------
      
      -- Data (first rows) --
      
    Output
        time Infected  Recovered Susceptible
      1 0.00 1.000000 0.00000000    99999.00
      2 0.01 1.015000 0.00500000    99998.98
      3 0.02 1.030225 0.01007500    99998.96
      4 0.03 1.045678 0.01522612    99998.94
      5 0.04 1.061363 0.02045451    99998.92
    Message
      
      i Access with `as.data.frame()` • Visualise with `plot()`

# print.simulate_sdbuildR() snapshot: named model

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: My SIR Model -------------------------------------
      
      -- Data (first rows) --
      
    Output
        time Infected  Recovered Susceptible
      1 0.00 1.000000 0.00000000    99999.00
      2 0.01 1.015000 0.00500000    99998.98
      3 0.02 1.030225 0.01007500    99998.96
      4 0.03 1.045678 0.01522612    99998.94
      5 0.04 1.061363 0.02045451    99998.92
    Message
      
      i Access with `as.data.frame()` • Visualise with `plot()`

# print.simulate_sdbuildR() snapshot: failed simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation ---------------------------------------------------
      x Simulation failed
      i Inspect the error message with: `x$error_message`

