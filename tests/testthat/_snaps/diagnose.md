# diagnose() detects stocks not connected to flows

    Code
      print(result)
    Message
      
      -- Potential problems (3) --
      
      * Model has no flows.
      > Add flows with `flow()` or `update()`.
      * Stocks not connected to any flow: `Stock1` and `Stock2`.
      * `Stock1` and `Stock2` have an equation of 0.

# diagnose() warns about zero equations

    Code
      print(result)
    Message
      
      -- Potential problem (1) --
      
      * `Stock1` and `Flow1` have an equation of 0.

# print.diagnose_sdbuildR() shows 'No problems detected!' for valid model

    Code
      print(result)
    Message
      v No problems detected!

# diagnose() warns when unit test eqn reference undefined variable

    Code
      print(result)
    Message
      
      -- Potential problems (3) --
      
      * Model has no flows.
      > Add flows with `flow()` or `update()`.
      * Stock not connected to any flow: `S`.
      * Unit test reference undefined variable.
        [1] "valid" expr: `drain` is undefined.
      > Update the affected tests or add the missing variables.

