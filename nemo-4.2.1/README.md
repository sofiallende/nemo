# NEMO 4.2.1

This repository is a fork of the release 4.2.1 of NEMO, and it also contains the compilation files
and the configurations used in ELIC.

## Installation instructions

The code can be retrieved using:

```
mkdir -p ~/models
cd ~/models
git clone git@forge.uclouvain.be:abarthelemy/nemo-4.2.1.git
```

In order to prepare the compilation, two steps must be completed. First, we must load the modules
corresponding to the machine used:

- `lemaitre3`: https://forge.uclouvain.be/abarthelemy/climatetoolbox/-/blob/main/models/nemo/modules/lemaitre3.txt
- `lemaitre4`: https://forge.uclouvain.be/abarthelemy/climatetoolbox/-/blob/main/models/nemo/modules/lemaitre4.txt
- `manneback`: https://forge.uclouvain.be/abarthelemy/climatetoolbox/-/blob/main/models/nemo/modules/manneback.txt
- `lucia`: https://forge.uclouvain.be/abarthelemy/climatetoolbox/-/blob/main/models/nemo/modules/lucia.txt
- `lumi`: https://forge.uclouvain.be/abarthelemy/climatetoolbox/-/blob/main/models/nemo/modules/lumi.txt

Second, we must check that XIOS (the software used by NEMO to write its outputs) is correctly set
up. The required version of XIOS depends on the machine used. This information can be found in the
machine architecture file, which is located in `~/models/nemo-4.2.1/arch/` and is named
`arch-machine_used.fcm` (with `machine_used` replaced by the actual name of the machine,
obviously). For instance, the first line of this file looking like:

```
%XIOS_HOME           $HOME/tools/xios-elic-2
```

means that, for the present version of NEMO and on the machine used, `xios-elic-2` has to be
installed in `$HOME/tools/`. This path should be adjusted in the architecture file if XIOS was
installed in another directory.

THE `REBUILD_NEMO` tool should now be compiled. We can use the following lines:

```
cd ~/models/nemo-4.2.1/tools
./maketools -m machine_used -n REBUILD_NEMO
```

The `WEIGHTS` tool allows to create weight files used by NEMO to perform on-the-fly interpolation
of input files. New weight files will need to be created only if non-standard input files
are to be used. In this case, the tool can be compiled with:

```
cd ~/models/nemo-4.2.1/tools
./maketools -m machine_used -n WEIGHTS
```

Finally, we can compile NEMO and create a new configuration on the basis of a reference
configuration. For instance, the following command creates the compiled configuration
`MY_CONFIGURATION`, based on `REFERENCE_CONFIGURATION`, compiling in parallel on 4 processes:

```
cd ~/models/nemo-4.2.1/
./makenemo -m machine_used -r REFERENCE_CONFIGURATION -n MY_CONFIGURATION -j 4
```

**Warning** The option enabling parallel compilation `-j 4` does not work on `lumi`. The solution
is simply to omit it.

Compiled configurations are created alongside reference configurations in
`~/models/nemo-4.2.1/cfgs/`. To verify that NEMO compiled properly, the existence of the NEMO
executable can be checked in:

```
~/models/nemo-4.2.1/cfgs/MY_CONFIGURATION/BLD/bin/nemo.exe
```

While reference configurations should not be removed, a compiled configuration can be deleted (if
needed) via:

```
cd ~/models/nemo-4.2.1/
./makenemo -n MY_CONFIGURATION clean_config
```

## Useful links

- https://www.nemo-ocean.eu/
- https://sites.nemo-ocean.io/user-guide/

## Details about the fork (not important for users)

The code was obtained with:

```
git clone --branch 4.2.1 https://forge.nemo-ocean.eu/nemo/nemo.git nemo_4.2.1
```

This was done on 2023-08-01, but the date is not important since it is a tagged version that will
not evolve.

The Git control of the code was then removed by deleting the `.git` folder, and the files were
finally committed in the present, new Git repository.
