# print(summary(sfm)) shows Dependencies section

    Code
      print(summary(sfm))
    Message
      
      -- Stock-and-Flow Model Summary ------------------------------------------------
      
      -- Dependencies --
      
      `Beta `: Effective_Contact_Rate, Total_Population
      `Lambda `: Beta, Infected
      `Infection_Rate`: Susceptible, Lambda
      `Recovery_Rate `: Infected, Delay
      
      -- Diagnostics --
      
      x 1 warning — run `diagnose()` for details.

# print(summary(sfm)) shows Diagnostics section

    Code
      print(summary(sfm))
    Message
      
      -- Stock-and-Flow Model Summary ------------------------------------------------
      
      -- Dependencies --
      
      `Beta `: Effective_Contact_Rate, Total_Population
      `Lambda `: Beta, Infected
      `Infection_Rate`: Susceptible, Lambda
      `Recovery_Rate `: Infected, Delay
      
      -- Diagnostics --
      
      x 1 warning — run `diagnose()` for details.

# print(summary(sfm)) reports no issues for valid model

    Code
      print(summary(sfm))
    Message
      
      -- Stock-and-Flow Model Summary ------------------------------------------------
      
      -- Dependencies --
      
      `Flow1`: S
      
      -- Diagnostics --
      
      v No issues detected.

# print(summary(sfm)) reports errors for invalid model

    Code
      print(summary(sfm))
    Message
      
      -- Stock-and-Flow Model Summary ------------------------------------------------
      
      -- Dependencies --
      
      i No variables with dependencies.
      
      -- Diagnostics --
      
      x 1 error, 1 warning — run `diagnose()` for details.

