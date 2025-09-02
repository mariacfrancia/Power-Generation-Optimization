# Power Generation Optimization Models in GAMS

## Overview
This repository contains a set of **power system optimization models** implemented in **GAMS (General Algebraic Modeling System)**. The models address short-term **generation scheduling** under multiple operational and economic constraints, including **demand satisfaction, technical limits, ramp rates, hydro constraints, startup/shutdown costs, and stochastic wind generation**.

**Team project executed by 3 members.**

The project demonstrates expertise in:
- **Operations Research**  
- **Mathematical Optimization**  
- **Stochastic Programming**  
- **Power System Scheduling**  
- **Cost Minimization under Uncertainty**

---

## Models Implemented

- **Model 1: Linear Cost Minimization**  
  Minimize production costs using linear unit costs. Outputs: hourly unit commitment, hydro generation, marginal water value.

- **Model 2: Uniform Price Minimization**  
  Minimize total payment assuming a single hourly market price. Outputs: unit schedules, coupled units, hydro generation, hourly price.

- **Model 3: Cost Minimization with Startup/Shutdown**  
  Minimize costs including startup/shutdown of thermal units. Outputs: generation schedule, unit on/off states, hydro generation, total system cost.

- **Model 4: Stochastic Optimization (Wind Uncertainty)**  
  Minimize expected costs under probabilistic wind scenarios. Outputs: stochastic schedule, expected value of stochastic solution (VSS), EV with perfect information (EVPI), risk-adjusted plan.

- **Model 5: Minimax Regret (Optional)**  
  Minimize maximum regret across scenarios. Outputs: robust schedule, maximum and expected regret.

