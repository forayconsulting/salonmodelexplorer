# Game Theoretic Model Explorer: The Inner Beauty of Firms

This Shiny application allows you to explore a game theoretic model of firm organization, specialization, and productivity based on "The Inner Beauty of Firms" by Jacob Kohlhepp.

## Model Overview

The paper investigates how firms organize their internal labor to assign tasks to workers with different skill sets, and how this affects productivity. The model demonstrates that when firms can internally reorganize in response to economic shocks, the impacts on productivity can be qualitatively different than what would be predicted by models that only consider reallocation of labor across firms.

### Key Concepts

#### Task Specialization (s-index)

The model measures task specialization using the s-index, which calculates the Kullback-Leibler divergence between a firm's observed task assignment and a "generalist benchmark" where tasks are randomly assigned.

#### Firm Heterogeneity

Firms differ in their coordination cost parameter (γ), which affects their ability to implement specialized task assignments. Firms with lower coordination costs can achieve higher levels of specialization and typically higher productivity.

#### Worker Skill Sets

Workers have multidimensional skills across different tasks. The model allows for horizontal differences in worker skill portfolios, which leads to comparative advantage in specific tasks.

#### Equilibrium Types

The model explores two types of equilibrium:

1. **Reallocation Equilibrium**: Firms can adjust prices and the total amount of labor they hire, but task assignments remain fixed.
2. **Reorganization Equilibrium**: Firms can fully adjust their task assignments in addition to prices and labor quantities.

### Counterfactual Scenarios

The model can explore various economic shocks and their impacts:

#### Sales Tax Increase

Increasing the sales tax generally reduces specialization and productivity when firms can reorganize, as it makes it more difficult for firms to pass on the cost of specialization to consumers.

#### Low-Wage Immigration

An increase in the labor supply of the lowest-wage workers has different effects depending on whether firms can reorganize:
- Without reorganization: Specialization falls and productivity decreases
- With reorganization: Specialization increases and productivity rises

#### Market Concentration

Removing half of the salons in each market to increase concentration has varying effects depending on the county and whether firms can reorganize.

## Application Usage

This application provides a simplified interface to explore the key findings and relationships in the model:

1. **Select Parameters**: Choose the county, quarter, and equilibrium type.
2. **Configure Scenarios**: Set values for sales tax rate, immigration, market concentration, and other parameters.
3. **Run Model**: Execute the calculations and view the results.
4. **Examine Results**: Compare productivity, specialization changes, and wage effects.

## Academic Context

The findings from this model contribute to several areas of economic research:

1. **Determinants of Firm Productivity**: Understanding how internal organization affects productivity
2. **Task-Based Perspective of Labor Markets**: How tasks are assigned to workers with different skills
3. **Talent Allocation within the Firm**: The role of management in worker assignment
4. **Organizational Economics**: How coordination costs shape firm structure

## Data

The model is calibrated using data from hair salons in three counties:
- Cook County, IL (Chicago)
- New York County, NY (Manhattan)
- Los Angeles County, CA

## Limitations

This application provides a simplified version of the full model. In a complete implementation, the actual model computation would solve complex labor market equilibrium conditions.

## Reference

Kohlhepp, J. (2025). "The Inner Beauty of Firms." *American Economic Review*.
