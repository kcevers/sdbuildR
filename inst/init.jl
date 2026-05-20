# init.jl - Script to initialize Julia environment for sdbuildR

# Load packages
using StockFlowRSupport
using CSV
using DataFrames
using DiffEqCallbacks
using Distributions
using OrdinaryDiffEq
using OrdinaryDiffEqLowOrderRK
using Random
using SciMLBase
using Statistics
using StatsBase

# Extend min/max: when applied to a single vector, use minimum, like in R
Base.min(v::AbstractVector) = minimum(v)
Base.max(v::AbstractVector) = maximum(v)

# Add initialization of sdbuildR
init_sdbuildR = true

