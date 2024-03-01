# Configuration eORCA1_ELIC_3

## Status

Obsolete.

## Summary

This configuration is the evolution of `eORCA1_ELIC_2` in NEMO 4.2.0. It should be replaced with
`eORCA1_ELIC_5`, which fixes the bug in the computation of the ice-ocean drag.

## Notable features

### Namelists

- The maximum tolerated sea ice concentration in the Southern Hemisphere (`rn_amax_s`) is reduced
to 0.98.
- The snow thermal conductivity (`rn_cnd_s`) is increased to 0.5 W m<sup>-1</sup> K<sup>-1</sup>.
- The iceberg melting flux from Jourdain (2019) is used.

### Code

* A code change is included to prevent more efficiently the formation of ice in open water for
landfast ice.
* Two code changes are included to:
  * distribute the iceberg melting flux along the vertical;
  * block the iceberg melting flux when the surface of the ocean is close to its freezing point.

## Known issues

- A possibly important bug is present in the computation of the ice-ocean drag.
