# margot.core

Core S3 classes and infrastructure for the margotsphere ecosystem.

## Overview

The `margot.core` package provides the fundamental object system and infrastructure used across all margotsphere packages. It defines:

- S3 classes for shadows and scenarios
- The `margot_panel` class for standardised panel data
- Shadow dependency management with cycle detection
- Core utilities shared across the ecosystem

## Installation

```r
# install from github
devtools::install_github("go-bayes/margot.core")
```

## Key Components

### Shadow Classes

Shadows represent observational distortions in data:

```r
library(margot.core)

# create a measurement error shadow
shadow <- new_shadow(
  type = "measurement_error",
  params = list(
    variables = "blood_pressure",
    error_type = "classical",
    sigma = 0.5
  )
)

print(shadow)
#> <margot_shadow>
#> Type: measurement_error
#> Parameters:
#>   variables: blood_pressure
#>   error_type: classical
#>   sigma: 0.5
```

### Scenario Classes

Scenarios bundle shadows with documentation:

```r
scenario <- new_scenario(
  name = "Validation Study",
  description = "Based on validation against clinical measurements",
  shadows = list(shadow),
  justification = "Measurement error estimated from duplicate measures"
)

print(scenario)
#> <margot_scenario>
#> Name: Validation Study
#> Description: Based on validation against clinical measurements
#> Justification: Measurement error estimated from duplicate measures
#> Number of shadows: 1
#> Shadow types: measurement_error
```

### Panel Data Class

The `margot_panel` class provides standardised handling of longitudinal data:

```r
# create panel data
df <- data.frame(
  id = rep(1:3, each = 3),
  wave = rep(1:3, 3),
  outcome = rnorm(9),
  treatment = rbinom(9, 1, 0.5)
)

panel <- margot_panel(df, id = "id", time = "wave")
print(panel)
#> <margot_panel>
#>   Rows: 9
#>   Units: 3
#>   Time periods: 3
#>   ID column: id
#>   Time column: wave
#>   Panel: balanced

# convert to wide format
wide <- as_wide.margot_panel(panel)
```

### Shadow Dependencies

The package includes a dependency system to handle shadow interactions:

```r
# shadows are automatically ordered based on dependencies
shadows <- list(
  new_shadow("measurement_error", list(
    variables = "y",
    error_type = "classical",
    sigma = 0.5
  )),
  new_shadow("truncation", list(
    variables = "y",
    lower = 0
  ))
)

# check for circular dependencies
cycle_check <- detect_cycles(shadows)
print(cycle_check$message)
#> No cycles detected in shadow dependencies

# reorder based on dependencies
ordered_shadows <- reorder_shadows(shadows)
```

## Design Principles

1. **Minimal Dependencies**: Only base R, stats, and utils
2. **No Circular Imports**: margot.core imports from no other margotsphere packages
3. **Extensible S3 System**: Easy to add new shadow types
4. **Comprehensive Validation**: All objects are validated on creation

## For Package Developers

If you're building a margotsphere package:

```r
# in your DESCRIPTION file
Imports:
    margot.core (>= 0.0.1)

# in your code
shadow <- margot.core::new_shadow(...)
```

## License

MIT © Joseph A. Bulbulia