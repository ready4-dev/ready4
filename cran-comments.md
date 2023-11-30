## Test environments

* Local Windows (mingw32), R 4.3.1 [1 NOTE generated]
* Fedora Linux (on R-hub) R-devel [2 NOTEs generated]
* Windows Server 2022, R-devel, 64 bit (on R-hub) [3 NOTEs generated]
* Windows Server 2022, R-release, 32/64 bit (on R-hub) [0 NOTEs generated]
* Windows Server 2022 x64 (build 20348) (on winbuilder) [1 NOTE generated]

## R CMD check results

There were no ERRORs or WARNINGs. 

There are 4 NOTEs.

1. One NOTE related to this being a new submission and having a potentially misspelled word (found in Local Windows, Windows Server development, Windows Server (on winbuilder) and Fedora Linux):

```
checking CRAN incoming feasibility ... [186s] NOTE
  Maintainer: 'Matthew Hamilton <matthew.hamilton1@monash.edu>'
  
  New submission
  ....
  ....
    Possibly misspelled words in DESCRIPTION:
    updatable (15:42)

```

2. One NOTE only found on Windows Server (development) is: 

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```

As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored. 


3. One NOTE only found on Windows Server (development) is: 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


4. A fourth NOTE is found with Fedora Linux:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This also seems to be a recurring issue on Rhub [R-hub issue #560](https://github.com/r-hub/rhub/issues/548) and so can likely be ignored.

---

This initial version provides generics that collectively define a syntax for modular health economic model development in R, a template model module and functions to partially automate the generation of HTML for inclusion on modelling project web-pages.
