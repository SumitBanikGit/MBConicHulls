# MBConicHulls
MBConicHulls: a Mathematica package to evaluate N-fold MB integrals.

Version 1.3 can evaluate MB integrals with polygamma functions, both using the conic hull and triangulation approaches. More details can be found in [2512.XXXXX](https://arxiv.org/abs/2512.XXXXX).

Version 1.2 can evaluate MB integrals using the triangulation approach, which is much faster than the conic hull method for higher-fold MB integrals. More details can be found in [2309.00409](https://arxiv.org/abs/2309.00409) (published version [here](https://journals.aps.org/prd/abstract/10.1103/PhysRevD.110.036002)).

Version 1.1 can evaluate MB integrals with straight-contours. It can also evaluate one-fold MB integrals. More details can be found in [2212.11839](https://arxiv.org/abs/2212.11839) (published version [here](https://journals.aps.org/prd/abstract/10.1103/PhysRevD.107.016007)).

Version 1.0 can evaluate two or higher-fold MB integrals with non-straight contours. It is identical to the one published in [2012.15108](https://arxiv.org/abs/2012.15108) (published version [here](https://journals.aps.org/prl/abstract/10.1103/PhysRevLett.127.151601)).

```
███╗   ███╗██████╗  ██████╗ ██████╗ ███╗   ██╗██╗ ██████╗██╗  ██╗██╗   ██╗██╗     ██╗     ███████╗
████╗ ████║██╔══██╗██╔════╝██╔═══██╗████╗  ██║██║██╔════╝██║  ██║██║   ██║██║     ██║     ██╔════╝
██╔████╔██║██████╔╝██║     ██║   ██║██╔██╗ ██║██║██║     ███████║██║   ██║██║     ██║     ███████╗
██║╚██╔╝██║██╔══██╗██║     ██║   ██║██║╚██╗██║██║██║     ██╔══██║██║   ██║██║     ██║     ╚════██║
██║ ╚═╝ ██║██████╔╝╚██████╗╚██████╔╝██║ ╚████║██║╚██████╗██║  ██║╚██████╔╝███████╗███████╗███████║
╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝ ╚═════╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝                                                                                                                                   


```

MBConicHulls.wl needs the Mathematica package [MultivariateResidues.m](https://arxiv.org/abs/1701.01040) and software [TOPCOM](https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/).

Pentagon.nb and Examples.nb need the packages [FIESTA5](https://bitbucket.org/feynmanIntegrals/fiesta/src), [MB.m](https://mbtools.hepforge.org/) and [MBresolve.m](https://mbtools.hepforge.org/). 

For more packages related to MB integrals, [see](https://bitbucket.org/feynmanIntegrals/mb/src/master/).

