# print.sdbuildR() snapshot: empty model

    Code
      print(sfm)
    Message
      
      -- sdbuildR model --------------------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      
      Time: 0.0 to 100.0 seconds (dt = 0.01) • euler • R

# print.sdbuildR() snapshot: named model

    Code
      print(sfm)
    Message
      
      -- sdbuildR model: My SIR Model ------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      
      Time: 0.0 to 100.0 seconds (dt = 0.01) • euler • R

# print.sdbuildR() snapshot: SIR model

    Code
      print(sfm)
    Message
      
      -- sdbuildR model: Susceptible-Infected-Recovered (SIR) ------------------------
      3 stocks • 2 flows • 4 constants • 1 auxiliary
      
      -- Stock-Flow Structure --
      
      Susceptible: - Infection_Rate
      Infected: + Infection_Rate, - Recovery_Rate
      Recovered: + Recovery_Rate
      
      -- Other Variables --
      
      Constants: `Beta`, `Total_Population`, `Effective_Contact_Rate`, and `Delay`
      Auxiliaries: `Lambda`
      
      -- Simulation Settings --
      
      Time: 0.0 to 20.0 weeks (dt = 0.01) • euler • R

# print.sdbuildR() snapshot: model with constants

    Code
      print(sfm)
    Message
      
      -- sdbuildR model --------------------------------------------------------------
      1 stock • 1 flow • 1 constant
      
      -- Stock-Flow Structure --
      
      S: - Flow1
      
      -- Other Variables --
      
      Constants: `k`
      
      -- Simulation Settings --
      
      Time: 0.0 to 100.0 seconds (dt = 0.01) • euler • R

# print.sdbuildR() snapshot: default name not shown as title

    Code
      print(sfm)
    Message
      
      -- sdbuildR model --------------------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      
      Time: 0.0 to 100.0 seconds (dt = 0.01) • euler • R

