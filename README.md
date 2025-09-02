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

### Model 1: Linear Production Cost Minimization
- **Objective:** Minimize production costs using only the linear unit cost term (`bi`).  
- **Key Outputs:**
  - Hourly unit commitment and generation
  - Hydro generation per block
  - Marginal water value (substitution value)

### Model 2: Uniform Price Minimization
- **Objective:** Minimize total payment assuming a single hourly market price, equal to the maximum unit cost of all active generators.  
- **Key Outputs:**
  - Hourly schedule of units
  - Coupled units per hour
  - Hydro generation
  - Hourly clearing price

### Model 3: Cost Minimization with Startup/Shutdown Costs
- **Objective:** Minimize production costs including both linear approximation and **startup/shutdown costs** of thermal units.  
- **Key Outputs:**
  - Generation schedule with unit on/off states
  - Units starting and stopping per hour
  - Hydro generation
  - Total system cost

### Model 4: Stochastic Optimization with Wind Uncertainty
- **Objective:** Minimize **expected production costs** considering stochastic wind scenarios.  
- **Scenario Description:**
  - Hours 1–2: deterministic wind production
  - Hour 3: +400 MW (p=0.6) or −600 MW (p=0.4)
  - Hour 4: conditional on hour 3 outcome, with further probabilistic deviations
- **Key Outputs:**
  - Optimal stochastic schedule
  - Expected value of the stochastic solution (VSS)
  - Expected value with perfect information (EVPI)
  - Risk-adjusted generation plan

### Model 5 (Optional): Minimax Regret Optimization
- **Objective:** Minimize the **maximum regret** across all wind generation scenarios, ensuring robustness against uncertainty.  
- **Key Outputs:**
  - Robust generation schedule
  - Maximum and expected regret values

---

## Repository Structure
