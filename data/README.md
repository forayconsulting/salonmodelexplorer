# Data Directory

This directory should contain the input data files required by the Salon Model Explorer application.

## Expected Data Files

The application is designed to work with the following data files:

### County-Level Data

- `cook_county.rds`: Data for Cook County, IL (Chicago)
- `new_york_county.rds`: Data for New York County, NY (Manhattan)
- `los_angeles_county.rds`: Data for Los Angeles County, CA

### Data Format

Each county data file should be an R data frame (saved as an RDS file) with the following structure:

```
salon_id | quarter | task_assignments | worker_skills | prices | wages | productivity | s_index
---------|---------|-----------------|---------------|--------|-------|--------------|--------
numeric  | date    | matrix          | matrix        | vector | vector| numeric      | numeric
```

Where:
- `salon_id`: Unique identifier for each salon
- `quarter`: Time period (date in format YYYY-QN where N is the quarter number)
- `task_assignments`: Matrix B where B[i,j] is the share of worker i's time spent on task j
- `worker_skills`: Matrix Z where Z[i,j] is worker i's skill level at task j
- `prices`: Vector of service prices at the salon
- `wages`: Vector of worker wage rates
- `productivity`: Revenue per worker-minute
- `s_index`: Task specialization index (KL divergence from generalist benchmark)

## Sample Data

Until you have the real data files, the application will run with simulated data generated at runtime. When you have the actual data files, place them in this directory, and the application will automatically use them instead of the simulated data.

## Data Sources

The data for this application is based on the research paper "The Inner Beauty of Firms" by Jacob Kohlhepp. In a full implementation, this data would come from calibrated model outputs using the methodology described in the paper.
