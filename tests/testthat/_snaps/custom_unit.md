# custom_unit() validates unit equations are not empty

    Code
      custom_unit(sfm, "unit1", "")
    Condition
      Warning:
      Empty `eqn` argument.
      > Setting `eqn` to "'0.0'".
      Warning:
      Empty `eqn` argument.
      > Setting `eqn` to "'0.0'".
    Message
      
      -- sdbuildR model --------------------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      
      Time: 0.0 to 100.0 seconds (dt = 0.01) • euler • R

