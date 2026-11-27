# Parametric decay functions for intervention effects

Evaluates one of several decay functions at a vector of time points,
following the OpenMalaria decay conventions. With \\x = t / L\\:

|  |  |  |
|----|----|----|
| **shape** | **value** | **meaning of L** |
| `"weibull"` | \\\exp(-x^\kappa \log 2)\\ | half-life |
| `"exponential"` | \\\exp(-x \log 2)\\ | half-life |
| `"hill"` | \\1 / (1 + x^\kappa)\\ | half-life |
| `"linear"` | \\\max(1 - x, 0)\\ | time to zero |
| `"step"` | 1 if \\t \< L\\, else 0 | time to zero |
| `"smooth-compact"` | \\\exp(\kappa - \kappa / (1 - x^2))\\ if \\t \< L\\, else 0 | time to zero |
| `"none"` / `"constant"` | 1 | unused |

Shape names are case-insensitive; `"smoothcompact"` and `"no decay"` are
accepted as aliases.

## Usage

``` r
decay_function(L, kappa = NULL, datej, decay)
```

## Arguments

- L:

  Half-life or time to zero (days), depending on the shape (see table).
  Ignored for `"none"` / `"constant"`.

- kappa:

  Shape exponent (required for Weibull, Hill, smooth-compact; ignored
  otherwise).

- datej:

  Numeric vector of time points (days) at which to evaluate the decay.

- decay:

  String specifying the decay shape (see table).

## Value

Numeric vector of decay values in `[0, 1]`.
