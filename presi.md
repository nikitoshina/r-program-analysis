---
title: "Final Report CS 686"
author:
  - name: "Nikita Tkachenko"
    email: "natkachenko@dons.usfca.edu"
patat:
  wrap: true
  margins:
    left: 10
    right: 10
    top: 5
  transition:
    type: slideLeft
  theme:
    header: [bold, rgb#ffb86c]
    strong: [rgb#bd93f9]
---

# Introduction

- **Objective:** Implement a points-to-definition approach that highlights variables used in a script but not defined at specific program points.

# Background

- **Existing Solutions:** Limited availability of program analyzers for R; only a partially functioning language server and an imperfect R linter found.
- **Resources:** The most helpful resource was the "Expressions" chapter in "Advanced R" by Hadley Wickham, providing a foundation with examples and necessary functions for the analysis.

# Methodology

## Challenges Encountered

- **Functions:** Must account for variables undefined at the point of function definition but used when the function is called.
- **External Packages:** Using `library()` to attach packages requires all functions from the package to be considered defined.
- **Data Frames and Built-in Datasets:** Requires stubs for common functions to manage quoted and unquoted arguments effectively due to complications with data frames and built-in datasets.

## Analysis Implementation

- **Parsing and AST Conversion:** Starts by parsing an `file.r` into expressions and converting them into an AST.
- **Variable Store:** Maintains a single store for all available variables and logs issues related to undefined variables in an error store.
- **Handling Loops:** Temporarily adds loop-only variables to the store and removes them after loop execution.
- **Function Calls:** Checks globals used within functions against the store to ensure they are defined.
- **Sourcing Files:** Evaluates sourced files fully, including their definitions in the analysis.

# Results

- **Outcome:** Successfully identifies variables that are defined and undefined, marking undefined variables with `%` for clarity.

# Example

## R Script

```r
foo <- function(a, b) {
    return(a + b * bop())
}

bar <- function(a, b) {
    sum <- foo(a, b)
    return(sum * constant)
}

baz <- function(a, b) {
    bar(a, b) * bar(a, b)
}

var <- baz(a = 2, b = b)

constant <- 5

bop <- function() {
    return(5)
}

baz(1, var)
```

## Output

```
3 : constant
------------------------------------------------------------
%baz <- function(a, b) {%

3 : bop
------------------------------------------------------------
%baz <- function(a, b) {%

4,3,3 : b
------------------------------------------------------------
var <- baz(a = 2, b = `%b%`)
```

# Conclusion

- **Skill Improvement:** Enhanced proficiency with R, especially in list manipulations, debugging, and shell scripting.
- **Project Impact:** Demonstrates R's capabilities for program analysis and contributes to refining development practices for R programming.
