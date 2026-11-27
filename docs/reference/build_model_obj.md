# Incorporate vector and host parameters in the entomological model

`build_model_obj` This function incorporates defined vector and host
parameters in the entomological model of mosquito oviposition cycle and
builds a comprehensive object containing all model parameters.
Additional parameters are calculated and included in the object: the
mosquito death rate while host seeking and the availability rate of each
host type (humans and animals) in the absence of interventions. The
function should be applied only on list objects created by the package
functions `def_vector_params` and `def_host_params`. Functionality using
other customized objects is not guaranteed.

Activity patterns are not stored in the model object; pass them directly
to
[`calculate_exposure()`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)
where they are used.

## Usage

``` r
build_model_obj(vec_p, hosts_p)
```

## Arguments

- vec_p:

  object containing vector-specific entomological parameters created
  with the function `def_vector_params`

- hosts_p:

  object containing host-specific entomological parameters created with
  the function `def_host_params`

## Value

list object containing the following components:

- `vec_params`: object with mosquito-specific entomological parameters
  (use function `def_vector_params` to generate object)

- `host_params`: object with host-specific entomological parameters (use
  function `def_host_params` to generate object). In addition to
  previously-defined parameters, it contains:

  - `muvA`: mosquito death rate while host seeking (per day)

  - `alphai`: per-individual-host availability to mosquitoes (per day),
    for humans and animals

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

## Examples

``` r
# Retrieve the species-specific entomological parameters for An. gambiae
gambiae_vec_p = def_vector_params("Anopheles gambiae ss/coluzzii")
# Retrieve the host-specific entomological parameters
gambiae_hosts_p = def_host_params()
# Build model object
model_p = build_model_obj(gambiae_vec_p, gambiae_hosts_p)
print(model_p)
#> $vec_params
#> $vec_params$species_name
#> [1] "Anopheles gambiae ss/coluzzii"
#> 
#> $vec_params$level
#> [1] "species"
#> 
#> $vec_params$complex
#> [1] "Gambiae"
#> 
#> $vec_params$imputed_traits
#> [1] "endophily"
#> 
#> $vec_params$M
#> [1] 0.6132517
#> 
#> $vec_params$M.sd
#> [1] 0.003408961
#> 
#> $vec_params$Chi
#> [1] 0.8642215
#> 
#> $vec_params$Chi.sd
#> [1] 0.00805288
#> 
#> $vec_params$HBI_indoor
#> [1] 0.8642215
#> 
#> $vec_params$HBI_indoor.sd
#> [1] 0.00805288
#> 
#> $vec_params$HBI_outdoor
#> [1] 0.7008052
#> 
#> $vec_params$HBI_outdoor.sd
#> [1] 0.03420376
#> 
#> $vec_params$A0
#> [1] 0.6205667
#> 
#> $vec_params$A0.sd
#> [1] 0.06231277
#> 
#> $vec_params$zeta.3
#> [1] 1
#> 
#> $vec_params$td
#> [1] 0.33
#> 
#> $vec_params$tau
#> [1] 1.074949
#> 
#> $vec_params$tau.sd
#> [1] 0.02383553
#> 
#> $vec_params$ts
#> [1] 10
#> 
#> $vec_params$to
#> [1] 5
#> 
#> $vec_params$endophily
#> [1] 0.7258244
#> 
#> $vec_params$endophily.sd
#> [1] 0.1163184
#> 
#> $vec_params$endophagy
#> [1] 0.5605694
#> 
#> $vec_params$endophagy.sd
#> [1] 0.004730246
#> 
#> $vec_params$hbi_source
#> [1] "indoor"
#> 
#> attr(,"source")
#> [1] "package: AnophelesBionomics" "version: 0.1.0"             
#> [3] "built: 2026-10-07"          
#> 
#> $host_params
#> $host_params$species_name
#> [1] "Anopheles gambiae" "Anopheles gambiae"
#> 
#> $host_params$host
#> [1] "human"  "animal"
#> 
#> $host_params$PBi
#> [1] 0.95 0.95
#> 
#> $host_params$PCi
#> [1] 0.95 0.95
#> 
#> $host_params$PDi
#> [1] 0.99 0.99
#> 
#> $host_params$PEi
#> [1] 0.88 0.88
#> 
#> $host_params$Kvi
#> [1] 0.03 0.00
#> 
#> $host_params$muvA
#> [1] 0.6461614
#> 
#> $host_params$alphai
#> [1] 1.9794421 0.3109918
#> 
#> 
```
