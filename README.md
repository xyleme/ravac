# r.avac: computing snow avalanches in GRASS GIS using AVAC

AVAC is a numerical code for simulating flowing avalanches using the ClawPack and GeoClaw libraries (www.clawpack.org). See https://github.com/cancey/avac.

This repository provides the GRASS GIS AddOn r.avac, which prepares the input data, runs the AVAC code, and post-processes the output files.
See the pdf files in /doc directory for further information.

We provide additional files to test the code or serve as examples.

The AVAC code has been extensively tested on Linux machines. See the ClawPack information for other operating systems.


*** CONTENTS ***


ClawPack files:
- AddSetrun.py
- b4step2.f90
- Makefile
- module_voellmy.f90
- qinit_module.f90
- setprob.f90
- setrun.py
- src2.f90
- voellmy.data

Documentation:
- readme (this page)
- r.avac_manual_fr.pdf: documentation on the r.avac addon in French
- r.avac_manual_en.pdf: documentation on the r.avac addon in English
- tex source files

GRASS addon:
- r.avac

Application data:
- boussolenc.gpkg: a GeoPackage containing a set of geographic data
- p_toraval.file: Toraval color chart for pressure maps display

***************

Christophe Ancey, EPFL, Switzerland

Vincent Bain, Toraval, France

version 3.1.4 December 2020
