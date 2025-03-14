# About the Game Theoretic Model Explorer

This application allows you to explore a game theoretic model of firm organization, specialization, and productivity based on the academic paper "The Inner Beauty of Firms" by Jacob Kohlhepp.

## The Model

The model examines how firms organize their internal labor and assign tasks to workers in a market equilibrium. Key concepts include:

### Task Specialization

The model measures how far a firm's task assignment is from a "generalist benchmark" using the s-index (specialization index). A higher s-index indicates greater specialization of workers to specific tasks.

### Coordination Costs

Firms differ in their coordination cost parameter (γ), which captures the difficulty of implementing specialized task assignments. Firms with lower coordination costs can achieve higher levels of specialization.

### Worker Skill Sets

Workers have multidimensional skills across different tasks. The model allows for horizontal differences in worker skill portfolios, which leads to comparative advantage in specific tasks.

### Equilibrium Types

The model explores two types of equilibrium:

1. **Reallocation Equilibrium**: Firms can adjust prices and the total amount of labor they hire, but task assignments remain fixed.
2. **Reorganization Equilibrium**: Firms can fully adjust their task assignments in addition to prices and labor quantities.

## Counterfactual Scenarios

This app allows you to explore various economic shocks and how they impact productivity, specialization, and wages:

### Sales Tax Increase

Increasing the sales tax generally reduces specialization and productivity when firms can reorganize, as it makes it more difficult for firms to pass on the cost of specialization to consumers.

### Low-Wage Immigration

An increase in the labor supply of the lowest-wage workers has different effects depending on whether firms can reorganize:
- Without reorganization: Specialization falls and productivity decreases
- With reorganization: Specialization increases and productivity rises

### Market Concentration

Removing half of the salons in each market to increase concentration has varying effects depending on the county and whether firms can reorganize.

### Price Sensitivity (ρ)

Adjusting price sensitivity changes how much consumers respond to price differences, which affects firms' incentives to specialize.

## Data Source

The model is calibrated using data from hair salons in three counties:
- Cook County, IL (Chicago)
- New York County, NY (Manhattan)
- Los Angeles County, CA

Each salon's data includes task assignments, prices, and market shares, which allows the model to recover the underlying parameters driving the observed organization patterns.

## Academic Reference

Kohlhepp, J. (2025). "The Inner Beauty of Firms." *American Economic Review*.

---

This app provides a simplified version of the full model. In a complete implementation, the actual model computation would solve for labor market equilibrium using the complex mathematical framework described in the paper.
