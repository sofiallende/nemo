# Configuration eORCA1_ELIC_5

## Status

Functional.

## Summary

This configuration is the evolution of `eORCA1_ELIC_3`.

## Notable features

### Namelists

- The maximum tolerated sea ice concentration in the Southern Hemisphere (`rn_amax_s`) is reduced
to 0.98.
- The snow thermal conductivity (`rn_cnd_s`) is increased to 0.5 W m<sup>-1</sup> K<sup>-1</sup>.
- No iceberg melting flux is used.

### Code

* A code change is included to prevent more efficiently the formation of ice in open water for
landfast ice.
* A possibly important bug in the computation of the ice-ocean drag is fixed.
