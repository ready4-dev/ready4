## Test environments

* Local Windows (mingw32), R 4.3.1 [1 NOTE generated]
* Fedora Linux (on R-hub) R-devel GCC [0 NOTEs generated]
* Fedora Linux (on R-hub) R-devel clang gfortran [2 NOTEs generated]
* Windows Server 2022, R-devel, 64 bit (on R-hub) [3 NOTEs generated]
* Windows Server 2022, R-release, 32/64 bit (on R-hub) [0 NOTEs generated]
* Windows Server 2022 x64 (build 20348) (on winbuilder) [1 NOTE generated] 

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE (on Local Windows) relating to this being a new submission.

1. One NOTE related to this being a new submission.

```
  New submission

```

---

This version addresses the following issues flagged by a previous CRAN reviewer:


- two functions have been edited so that "T" has been replaced with "TRUE" and "F" has been replaced with "FALSE"; and

- four functions that were imported from other packages and which were inadequately documented (e.g. not /value tag), have now been documented with explicit links to the source package's documentation.




