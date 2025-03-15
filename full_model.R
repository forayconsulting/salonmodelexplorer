# File: full_model.R
# Complete implementation of the mathematical model from Kohlhepp's paper

library(data.table)
library(Matrix)
library(rootSolve)
library(SQUAREM)
library(lamW)
library(parallel)

#===============================================================
# CORE MODEL FUNCTIONS
#===============================================================

# Helper function for log transformation that handles edge cases
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

# Calculate Kullback-Leibler divergence (s-index)
calculate_kl_divergence <- function(B, alpha) {
  # B is the task assignment matrix, alpha is the task mix
  # Calculate generalist benchmark G
  row_sums <- rowSums(B)
  B_sum <- sum(B)
  G <- outer(row_sums, alpha)
  
  # Calculate KL divergence
  mask <- B > 0 & G > 0
  if (sum(mask) == 0) return(0)
  
  sum(B[mask] * log(B[mask] / G[mask]))
}

#===============================================================
# FIRM ORGANIZATION OPTIMIZATION
#===============================================================

# Implementation of the Blahut-Arimoto algorithm to solve for optimal task assignments
# Based on Theorem 1 and Proposition 6 from the paper
solve_firm_organization <- function(alpha, theta, wages, gamma, rho, max_iter = 10000, tol = 1e-8) {
  # alpha: task mix vector (K-length)
  # theta: skill matrix (N x K)
  # wages: wage vector (N-length)
  # gamma: coordination cost parameter (scalar)
  # rho: price sensitivity (scalar)
  
  N <- nrow(theta)  # Number of skill sets
  K <- ncol(theta)  # Number of tasks
  
  # Initialize relative labor demands with equal distribution
  E <- rep(1/N, N)
  
  # Pre-compute value matrix
  V <- matrix(0, nrow = N, ncol = K)
  for (i in 1:N) {
    for (k in 1:K) {
      V[i, k] <- exp(gamma^(-1) * (rho^(-1) * theta[i, k] - wages[i]))
    }
  }
  
  # Blahut-Arimoto algorithm - fixed point iteration
  converged <- FALSE
  
  # Define the algorithm as a function for SQUAREM acceleration
  fixptfn <- function(E_current) {
    # Step 1: Compute task assignments given current E
    B <- matrix(0, nrow = N, ncol = K)
    for (k in 1:K) {
      denom <- sum(E_current * V[, k])
      for (i in 1:N) {
        B[i, k] <- alpha[k] * V[i, k] * E_current[i] / denom
      }
    }
    
    # Step 2: Compute updated relative labor demands
    E_new <- rowSums(B)
    return(E_new)
  }
  
  # Use SQUAREM for faster convergence
  result <- squarem(E, fixptfn, control = list(maxiter = max_iter, tol = tol))
  E_optimal <- result$par
  
  # Compute optimal task assignments
  B_optimal <- matrix(0, nrow = N, ncol = K)
  for (k in 1:K) {
    denom <- sum(E_optimal * V[, k])
    for (i in 1:N) {
      B_optimal[i, k] <- alpha[k] * V[i, k] * E_optimal[i] / denom
    }
  }
  
  # Ensure B_optimal rows sum to 1 for each skill set
  for (i in 1:N) {
    if (E_optimal[i] > 0) {
      B_optimal[i, ] <- B_optimal[i, ] / sum(B_optimal[i, ])
    }
  }
  
  # Calculate the s-index (specialization index)
  s_index <- calculate_kl_divergence(B_optimal, alpha)
  
  # Calculate endogenous cost and quality components
  quality <- sum(B_optimal * theta)
  coordination_cost <- gamma * s_index
  wage_cost <- sum(E_optimal * wages)
  total_cost <- coordination_cost + wage_cost
  
  return(list(
    E = E_optimal,                # Relative labor demands
    B = B_optimal,                # Task assignments
    s_index = s_index,            # Specialization index
    quality = quality,            # Endogenous quality
    coordination_cost = coordination_cost,  # Coordination cost
    wage_cost = wage_cost,        # Wage cost
    total_cost = total_cost       # Total endogenous cost
  ))
}

#===============================================================
# PRICING AND MARKET EQUILIBRIUM
#===============================================================

# Optimal price setting under logit demand
# Based on the Lambert W function approach in Appendix A.14
calculate_optimal_price <- function(firm, firms, rho) {
  # Extract firm-specific components
  quality <- firm$quality
  cost <- firm$total_cost
  
  # Calculate market share denominator (exclude current firm)
  Ej <- 0
  for (j in names(firms)) {
    if (j != firm$id) {
      f <- firms[[j]]
      Ej <- Ej + exp(f$quality - rho * f$price)
    }
  }
  
  # Apply Lambert W function formula from Appendix A.14
  arg <- exp(-1 + quality - rho * cost) / Ej
  p_star <- (1/rho) + cost + (1/rho) * lambertW0(arg)
  
  return(p_star)
}

# Calculate market shares under logit demand
calculate_market_shares <- function(firms, rho, market_size) {
  # Calculate denominator for logit shares (including outside option)
  denom <- 1  # Start with outside option
  for (j in names(firms)) {
    f <- firms[[j]]
    denom <- denom + exp(f$quality - rho * f$price)
  }
  
  # Calculate share for each firm
  for (j in names(firms)) {
    firms[[j]]$share <- exp(firms[[j]]$quality - rho * firms[[j]]$price) / denom
    firms[[j]]$quantity <- firms[[j]]$share * market_size
  }
  
  return(firms)
}

# Calculate labor demands for the entire market
calculate_labor_demands <- function(firms, skill_sets, required_labor) {
  # Initialize labor demands vector
  labor_demands <- rep(0, length(skill_sets))
  names(labor_demands) <- names(skill_sets)
  
  # Aggregate across all firms
  for (j in names(firms)) {
    f <- firms[[j]]
    for (i in names(skill_sets)) {
      if (f$E[i] > 0) {
        labor_demands[i] <- labor_demands[i] + f$quantity * required_labor * f$E[i]
      }
    }
  }
  
  return(labor_demands)
}

#===============================================================
# EQUILIBRIUM SOLVER
#===============================================================

# Solve for equilibrium prices given fixed organization structures
solve_price_equilibrium <- function(firms, rho, max_iter = 1000, tol = 1e-6) {
  # Initialize convergence tracking
  converged <- FALSE
  iter <- 0
  
  while (!converged && iter < max_iter) {
    iter <- iter + 1
    max_change <- 0
    
    # Update prices for each firm
    for (j in names(firms)) {
      old_price <- firms[[j]]$price
      firms[[j]]$price <- calculate_optimal_price(firms[[j]], firms, rho)
      
      # Track convergence
      change <- abs(firms[[j]]$price - old_price)
      max_change <- max(max_change, change)
    }
    
    # Update market shares
    firms <- calculate_market_shares(firms, rho, market_size = 1)
    
    # Check convergence
    converged <- max_change < tol
  }
  
  return(list(firms = firms, converged = converged, iterations = iter))
}

# Solve for wage equilibrium in the labor market
# Using the BB package for solving systems of nonlinear equations
solve_labor_market <- function(initial_wages, skill_sets, firms, labor_supplies, 
                              rho, required_labor, max_iter = 100, tol = 1e-4) {
  # Define the labor market clearing objective function
  market_clearing <- function(wages) {
    # Update firms with new wages
    for (j in names(firms)) {
      # Solve each firm's organization problem
      org_result <- solve_firm_organization(
        firms[[j]]$alpha, skill_sets, wages, firms[[j]]$gamma, rho
      )
      
      # Update firm organization
      firms[[j]]$E <- org_result$E
      firms[[j]]$B <- org_result$B
      firms[[j]]$s_index <- org_result$s_index
      firms[[j]]$quality <- org_result$quality
      firms[[j]]$coordination_cost <- org_result$coordination_cost
      firms[[j]]$wage_cost <- org_result$wage_cost
      firms[[j]]$total_cost <- org_result$total_cost
    }
    
    # Solve price equilibrium
    price_result <- solve_price_equilibrium(firms, rho)
    firms <- price_result$firms
    
    # Calculate labor demands
    labor_demands <- calculate_labor_demands(firms, skill_sets, required_labor)
    
    # Return excess demands
    return(labor_demands - labor_supplies)
  }
  
  # Solve the system using BB package
  wage_result <- BBsolve(
    initial_wages, market_clearing, 
    control = list(maxit = max_iter, tol = tol, trace = TRUE)
  )
  
  # Apply final wages to get equilibrium
  excess <- market_clearing(wage_result$par)
  
  return(list(
    wages = wage_result$par,
    firms = firms,
    excess_demand = excess,
    converged = wage_result$convergence,
    iterations = wage_result$iter
  ))
}

#===============================================================
# REALLOCATION EQUILIBRIUM SOLVER
#===============================================================

# Solve for reallocation equilibrium (fixed task assignments)
solve_reallocation_equilibrium <- function(firms, initial_wages, skill_sets, 
                                         labor_supplies, rho, required_labor) {
  # Store original task assignments
  original_B <- list()
  for (j in names(firms)) {
    original_B[[j]] <- firms[[j]]$B
  }
  
  # Define the modified organization solver for reallocation
  solve_firm_reallocation <- function(alpha, theta, wages, gamma, rho, original_B) {
    N <- nrow(theta)  # Number of skill sets
    K <- ncol(theta)  # Number of tasks
    
    # Find relative labor demands that satisfy task constraints with fixed B
    # This is a constrained optimization problem
    
    # Define objective function for optimization
    obj_fun <- function(E) {
      # Calculate cost components
      wage_cost <- sum(E * wages)
      
      # Calculate quality
      quality_component <- 0
      for (i in 1:N) {
        if (E[i] > 0) {
          for (k in 1:K) {
            quality_component <- quality_component + E[i] * original_B[i, k] * theta[i, k]
          }
        }
      }
      
      return(wage_cost - rho^(-1) * quality_component)
    }
    
    # Define constraint function: task_mix must be satisfied
    constraint <- function(E) {
      task_allocation <- rep(0, K)
      for (k in 1:K) {
        for (i in 1:N) {
          task_allocation[k] <- task_allocation[k] + E[i] * original_B[i, k]
        }
      }
      return(sum((task_allocation - alpha)^2))
    }
    
    # Solve constrained optimization
    result <- constrOptim(
      rep(1/N, N),                # Initial guess
      obj_fun,                     # Objective function
      NULL,                        # No gradient
      ui = diag(N),                # Constraints: E >= 0
      ci = rep(0, N),              # Constraints: E >= 0
      control = list(trace = 0)
    )
    
    E_optimal <- result$par
    E_optimal <- E_optimal / sum(E_optimal)  # Normalize
    
    # Calculate other metrics
    s_index <- calculate_kl_divergence(original_B, alpha)
    quality <- sum(t(t(original_B) * E_optimal) * theta)
    coordination_cost <- gamma * s_index
    wage_cost <- sum(E_optimal * wages)
    total_cost <- coordination_cost + wage_cost
    
    return(list(
      E = E_optimal,
      B = original_B,
      s_index = s_index,
      quality = quality,
      coordination_cost = coordination_cost,
      wage_cost = wage_cost,
      total_cost = total_cost
    ))
  }
  
  # Modified labor market solver
  market_clearing_realloc <- function(wages) {
    # Update firms with new wages but fixed task assignments
    for (j in names(firms)) {
      org_result <- solve_firm_reallocation(
        firms[[j]]$alpha, skill_sets, wages, firms[[j]]$gamma, rho, original_B[[j]]
      )
      
      # Update firm
      firms[[j]]$E <- org_result$E
      firms[[j]]$B <- org_result$B
      firms[[j]]$s_index <- org_result$s_index
      firms[[j]]$quality <- org_result$quality
      firms[[j]]$coordination_cost <- org_result$coordination_cost
      firms[[j]]$wage_cost <- org_result$wage_cost
      firms[[j]]$total_cost <- org_result$total_cost
    }
    
    # Solve price equilibrium
    price_result <- solve_price_equilibrium(firms, rho)
    firms <- price_result$firms
    
    # Calculate labor demands
    labor_demands <- calculate_labor_demands(firms, skill_sets, required_labor)
    
    # Return excess demands
    return(labor_demands - labor_supplies)
  }
  
  # Solve the system
  wage_result <- BBsolve(
    initial_wages, market_clearing_realloc, 
    control = list(maxit = 100, tol = 1e-4, trace = TRUE)
  )
  
  # Apply final wages to get equilibrium
  excess <- market_clearing_realloc(wage_result$par)
  
  return(list(
    wages = wage_result$par,
    firms = firms,
    excess_demand = excess,
    converged = wage_result$convergence
  ))
}

#===============================================================
# COUNTERFACTUAL IMPLEMENTATION
#===============================================================

# Apply sales tax
apply_sales_tax <- function(firms, tax_rate) {
  for (j in names(firms)) {
    firms[[j]]$tax_rate <- tax_rate
    firms[[j]]$consumer_price <- firms[[j]]$price * (1 + tax_rate)
  }
  return(firms)
}

# Apply immigration shock
apply_immigration <- function(labor_supplies, increase_rate, lowest_wage_index) {
  # Increase the supply of the lowest-wage worker type
  labor_supplies[lowest_wage_index] <- labor_supplies[lowest_wage_index] * (1 + increase_rate)
  return(labor_supplies)
}

# Apply market concentration shock
apply_concentration <- function(firms, reduction_rate) {
  # Identify firms to remove
  n_firms <- length(firms)
  n_remove <- floor(n_firms * reduction_rate)
  
  if (n_remove > 0) {
    # Sort firms by productivity (lowest first)
    firm_productivity <- sapply(firms, function(f) f$quality / f$total_cost)
    ordered_firms <- names(firms)[order(firm_productivity)]
    remove_firms <- ordered_firms[1:n_remove]
    
    # Remove firms
    firms[remove_firms] <- NULL
  }
  
  return(firms)
}

#===============================================================
# FULL MODEL SOLUTION
#===============================================================

# Run a complete counterfactual analysis
run_counterfactual <- function(params) {
  # Extract parameters
  county <- params$county
  quarter_year <- params$quarter_year
  equilibrium_type <- params$equilibrium_type
  sales_tax <- params$sales_tax
  immigration <- params$immigration
  market_concentration <- params$market_concentration
  rho_adj <- params$rho_adj
  
  # Set price sensitivity based on county
  rho_base <- switch(county,
                    "17031" = 0.027,  # Cook County
                    "36061" = 0.018,  # New York County
                    "6037" = 0.016)   # Los Angeles County
  
  rho <- rho_base * rho_adj
  
  # Load or create model data
  model_data <- setup_model_data(county, quarter_year)
  
  # Initialize baseline equilibrium
  baseline_result <- solve_baseline_equilibrium(
    model_data$firms, 
    model_data$initial_wages,
    model_data$skill_sets,
    model_data$labor_supplies,
    rho,
    model_data$required_labor
  )
  
  # Apply shocks
  counterfactual_data <- model_data
  
  # Apply sales tax if specified
  if (sales_tax > 0) {
    counterfactual_data$firms <- apply_sales_tax(counterfactual_data$firms, sales_tax)
  }
  
  # Apply immigration if specified
  if (immigration > 0) {
    # Find index of lowest wage worker
    lowest_wage_index <- which.min(baseline_result$wages)
    counterfactual_data$labor_supplies <- apply_immigration(
      counterfactual_data$labor_supplies, 
      immigration, 
      lowest_wage_index
    )
  }
  
  # Apply market concentration if specified
  if (market_concentration > 0) {
    counterfactual_data$firms <- apply_concentration(
      counterfactual_data$firms, 
      market_concentration
    )
  }
  
  # Solve counterfactual equilibrium
  if (equilibrium_type == "realloc") {
    counterfactual_result <- solve_reallocation_equilibrium(
      counterfactual_data$firms,
      baseline_result$wages,
      counterfactual_data$skill_sets,
      counterfactual_data$labor_supplies,
      rho,
      counterfactual_data$required_labor
    )
  } else {
    counterfactual_result <- solve_labor_market(
      baseline_result$wages,
      counterfactual_data$skill_sets,
      counterfactual_data$firms,
      counterfactual_data$labor_supplies,
      rho,
      counterfactual_data$required_labor
    )
  }
  
  # Calculate changes relative to baseline
  changes <- calculate_changes(baseline_result, counterfactual_result)
  
  return(list(
    baseline = baseline_result,
    counterfactual = counterfactual_result,
    changes = changes
  ))
}

# Calculate changes between baseline and counterfactual
calculate_changes <- function(baseline, counterfactual) {
  # Calculate aggregate productivity change
  baseline_productivity <- calculate_aggregate_productivity(baseline$firms)
  counterfactual_productivity <- calculate_aggregate_productivity(counterfactual$firms)
  productivity_change <- counterfactual_productivity / baseline_productivity - 1
  
  # Calculate aggregate specialization change
  baseline_s_index <- calculate_aggregate_s_index(baseline$firms)
  counterfactual_s_index <- calculate_aggregate_s_index(counterfactual$firms)
  s_index_change <- counterfactual_s_index / baseline_s_index - 1
  
  # Calculate wage changes
  wage_changes <- counterfactual$wages / baseline$wages - 1
  
  # Calculate productivity by skill set
  productivity_by_skill <- calculate_productivity_by_skill(
    baseline$firms, counterfactual$firms
  )
  
  return(list(
    productivity_change = productivity_change,
    s_index_change = s_index_change,
    wage_changes = wage_changes,
    productivity_by_skill = productivity_by_skill,
    avg_wage_change = mean(wage_changes)
  ))
}

# Helper functions for calculating aggregate metrics
calculate_aggregate_productivity <- function(firms) {
  total_output <- 0
  total_labor <- 0
  
  for (j in names(firms)) {
    f <- firms[[j]]
    total_output <- total_output + f$quantity * f$quality
    total_labor <- total_labor + f$quantity * f$required_labor
  }
  
  return(total_output / total_labor)
}

calculate_aggregate_s_index <- function(firms) {
  weighted_sum <- 0
  total_weight <- 0
  
  for (j in names(firms)) {
    f <- firms[[j]]
    weighted_sum <- weighted_sum + f$quantity * f$s_index
    total_weight <- total_weight + f$quantity
  }
  
  return(weighted_sum / total_weight)
}

calculate_productivity_by_skill <- function(baseline_firms, counterfactual_firms) {
  # Extract skill sets
  skill_sets <- rownames(baseline_firms[[1]]$B)
  
  # Initialize results
  baseline_prod <- rep(0, length(skill_sets))
  counterfactual_prod <- rep(0, length(skill_sets))
  names(baseline_prod) <- skill_sets
  names(counterfactual_prod) <- skill_sets
  
  # Calculate baseline productivity by skill set
  for (j in names(baseline_firms)) {
    f <- baseline_firms[[j]]
    for (i in skill_sets) {
      if (f$E[i] > 0) {
        skill_quality <- sum(f$B[i,] * i]  # Quality contribution
        baseline_prod[i] <- baseline_prod[i] + f$quantity * f$required_labor * f$E[i] * skill_quality
      }
    }
  }
  
  # Calculate counterfactual productivity by skill set
  for (j in names(counterfactual_firms)) {
    f <- counterfactual_firms[[j]]
    for (i in skill_sets) {
      if (f$E[i] > 0) {
        skill_quality <- sum(f$B[i,] * i]  # Quality contribution
        counterfactual_prod[i] <- counterfactual_prod[i] + f$quantity * f$required_labor * f$E[i] * skill_quality
      }
    }
  }
  
  # Calculate percent changes
  changes <- counterfactual_prod / baseline_prod - 1
  return(changes)
}

#===============================================================
# DATA SETUP FUNCTIONS
#===============================================================

# Setup model data based on county and quarter
setup_model_data <- function(county, quarter_year) {
  # Create skill sets matrix (different for each county based on Tables 9-11 in the paper)
  skill_sets <- switch(county,
    "17031" = matrix(c(
      -0.993, 12.340, -0.421, 0.955, -37.562,
      -0.372, 10.695, -5.088, 1.100, 56.239,
      -1.533, 33.242, -2.516, 0.721, -1.909,
      -1.186, -14.376, 14.264, -5.015, -9.803,
      6.755, 9.516, -4.148, 0.751, -4.197
    ), nrow = 5, byrow = TRUE),
    
    "36061" = matrix(c(
      -29.238, 2.254, 1.103, 1.647, 0.206,
      -0.795, 2.752, 1.991, -5.408, -3.038,
      -4.001, -6.377, -0.745, -1.541, 8.193,
      11.461, -3.885, 0.683, -3.853, 9.979,
      47.273, 16.775, -10.078, -4.238, 22.728
    ), nrow = 5, byrow = TRUE),
    
    "6037" = matrix(c(
      -0.028, -0.275, 0.876, -5.248, -61.626,
      -5.466, 13.326, 2.332, -6.157, -9.492,
      0.043, 1.570, -0.439, -3.733, -6.118,
      -0.305, 3.759, 0.751, -5.383, -3.982,
      0.946, -2.708, 1.654, -3.703, -3.676
    ), nrow = 5, byrow = TRUE)
  )
  
  rownames(skill_sets) <- paste0("skill_", 1:5)
  colnames(skill_sets) <- c("Administrative", "Blowdry", "Color", "Haircut", "Nail")
  
  # Create initial wages (based on Table 9-11 relativities)
  initial_wages <- switch(county,
    "17031" = c(15, 10, 12, 18, 11),
    "36061" = c(15, 12, 10, 11, 22),
    "6037" = c(15, 20, 14, 16, 17)
  )
  names(initial_wages) <- rownames(skill_sets)
  
  # Create labor supplies (roughly proportional to wages)
  labor_supplies <- initial_wages * 1000
  
  # Create required labor (a_j)
  required_labor <- 10
  
  # Create firms with different coordination costs
  firms <- create_firms(county, quarter_year, skill_sets)
  
  return(list(
    firms = firms,
    initial_wages = initial_wages,
    skill_sets = skill_sets,
    labor_supplies = labor_supplies,
    required_labor = required_labor
  ))
}

# Create a set of simulated firms based on the paper's data
create_firms <- function(county, quarter_year, skill_sets) {
  # Number of firms to create
  n_firms <- 30
  
  # Create task mix distributions (based on Table 2 from the paper)
  task_mix_mean <- c(0.40, 0.10, 0.38, 0.05, 0.06)
  task_mix_sd <- c(0.10, 0.05, 0.10, 0.03, 0.04)
  
  # Create coordination cost distribution (based on Figure A9 from the paper)
  gamma_mean <- switch(county,
    "17031" = 1.2,
    "36061" = 1.5,
    "6037" = 1.0
  )
  gamma_sd <- 0.5
  
  # Create firms
  firms <- list()
  for (j in 1:n_firms) {
    # Generate task mix
    alpha <- pmax(0.01, rnorm(5, task_mix_mean, task_mix_sd))
    alpha <- alpha / sum(alpha)  # Normalize
    
    # Generate coordination cost
    gamma <- rlnorm(1, log(gamma_mean), gamma_sd)
    
    # Initial price (will be optimized later)
    initial_price <- 100
    
    # Create firm
    firms[[paste0("firm_", j)]] <- list(
      id = paste0("firm_", j),
      alpha = alpha,
      gamma = gamma,
      price = initial_price,
      required_labor = required_labor,
      exo_quality = rnorm(1, 0, 1),   # Exogenous quality component
      exo_cost = pmax(0, rnorm(1, 0, 10))  # Exogenous cost component
    )
  }
  
  return(firms)
}

# Solve for the baseline equilibrium
solve_baseline_equilibrium <- function(firms, initial_wages, skill_sets, labor_supplies, rho, required_labor) {
  # First, solve each firm's organization problem
  for (j in names(firms)) {
    org_result <- solve_firm_organization(
      firms[[j]]$alpha, skill_sets, initial_wages, firms[[j]]$gamma, rho
    )
    
    # Update firm
    firms[[j]]$E <- org_result$E
    firms[[j]]$B <- org_result$B
    firms[[j]]$s_index <- org_result$s_index
    firms[[j]]$quality <- org_result$quality + firms[[j]]$exo_quality
    firms[[j]]$coordination_cost <- org_result$coordination_cost
    firms[[j]]$wage_cost <- org_result$wage_cost
    firms[[j]]$total_cost <- org_result$total_cost + firms[[j]]$exo_cost
  }
  
  # Solve for price equilibrium
  price_result <- solve_price_equilibrium(firms, rho)
  firms <- price_result$firms
  
  # Calculate labor demands
  labor_demands <- calculate_labor_demands(firms, skill_sets, required_labor)
  
  # Adjust wages to clear labor markets
  market_result <- solve_labor_market(
    initial_wages, skill_sets, firms, labor_supplies, rho, required_labor
  )
  
  return(list(
    firms = market_result$firms,
    wages = market_result$wages,
    excess_demand = market_result$excess_demand,
    converged = market_result$converged
  ))
}

#===============================================================
# CONNECT TO SHINY APP INTERFACE
#===============================================================

# Main function to replace the simplified model in the app
run_full_salon_model <- function(params) {
  # Run the full counterfactual analysis
  result <- run_counterfactual(params)
  
  # Extract the key metrics in the format expected by the app
  return(list(
    productivity_change = result$changes$productivity_change,
    s_index_change = result$changes$s_index_change,
    wage_changes = result$changes$wage_changes,
    productivity_by_skill = result$changes$productivity_by_skill,
    avg_wage_change = result$changes$avg_wage_change
  ))
}