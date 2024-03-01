# Configuration eORCA025_ELIC_2

## Status

Obsolete.

## Summary

This configuration is the evolution of `eORCA025_ELIC` in NEMO 4.2.0, with additional changes to
make it comparable to an equivalent eORCA1 configuration, in order to conduct a
resolution-sensitivity study. The configuration `eORCA025_ELIC_3` has been prepared in NEMO 4.2.2
to improve the present configuration.

## Notable features

### Namelists

- The maximum tolerated sea ice concentration in the Southern Hemisphere (`rn_amax_s`) is reduced
to 0.98.
- The scheme used to compute the solar flux transmitted through the surface layer is changed
(`nn_qtrice`).
- The snow thermal conductivity (`rn_cnd_s`) is increased to 0.5 W m<sup>-1</sup> K<sup>-1</sup>.
- Since NEMO 4.2.1 fixes a bug with the freshwater budget control, this configuration uses
`nn_fwb = 1`, while `eORCA025_ELIC` could have only `nn_fwb = 0`.
- No iceberg melting flux is used.
- The snowfall from ERA5 is ajdusted to avoid thick ice accumulations in small embayments along the
Antarctic Peninsula.

### Code

- The same DRAKKAR code as in configuration `eORCA025_ELIC` is included.
- A code change is included to prevent more efficiently the formation of ice in open water for
landfast ice.
- A possibly important bug in the computation of the ice-ocean drag is fixed.

## Known issues

* In an experiment starting on 1960-01-01, the model starts to be unstable around 2008, crashing
  more and more frequently with the following error in the file `ocean.output`:

  ```
  kt 1188035 |ssh| max    Infinity at i j     21  333     found in    2 MPI tasks, spread out among ranks  194 to  341
  kt 1188035 |U|   max   2.361     at i j k  586  692   1 MPI rank  722
  kt 1188035 Sal   min   7.394     at i j k 1200 1050   1 MPI rank 1107
  kt 1188035 Sal   max   46.06     at i j k 1351  790   1 MPI rank  871
  ```

  Inexplicably, the experiment can sometimes be resumed by simply re-launching the computing jobs.
  An unpassable point is finally reached during December 2013.

* Despite using `nn_fwb = 1` to control the global freshwater budget, the SSH is dropping by
  around 5 cm per year.

* Since the instability described above arises at a point situated under a small and shallow ice
  shelf along the Antarctic coast, Pierre Mathiot suggests that it could be linked to the drop in
  SSH. In turn, the failed control of the global freshwater budget may be caused by an
  incompatibility between the DRAKKAR code and the source code of this version of NEMO.
