# print.simulate_sdbuildR() snapshot: successful simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: Susceptible-Infected-Recovered (SIR) -------------
      
      -- Data (wide format, first rows) --
      
    Output
        time Susceptible Infected  Recovered
      1 0.00    99999.00 1.000000 0.00000000
      2 0.01    99998.98 1.015000 0.00500000
      3 0.02    99998.96 1.030225 0.01007500
      4 0.03    99998.94 1.045678 0.01522612
      5 0.04    99998.92 1.061363 0.02045451
    Message
      i Access data with `as.data.frame()`, `head()`, or `tail()` • Visualise with `plot()`

# print.simulate_sdbuildR() snapshot: named model

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: My SIR Model -------------------------------------
      
      -- Data (wide format, first rows) --
      
    Output
        time Susceptible Infected  Recovered
      1 0.00    99999.00 1.000000 0.00000000
      2 0.01    99998.98 1.015000 0.00500000
      3 0.02    99998.96 1.030225 0.01007500
      4 0.03    99998.94 1.045678 0.01522612
      5 0.04    99998.92 1.061363 0.02045451
    Message
      i Access data with `as.data.frame()`, `head()`, or `tail()` • Visualise with `plot()`

# print.simulate_sdbuildR() snapshot: failed simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation ---------------------------------------------------
      x Simulation failed
      i Inspect the error message with: `x$error_message`

