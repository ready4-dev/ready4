## Test environments

* Local Windows (mingw32), R 4.3.1 [1 NOTE generated]
* Mac OS macos-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8658118159/job/23741393976)
* Windows windows-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8658118159/job/23741394263)
* Fedora Linux 38 R Under development (unstable) gcc13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8658118159/job/23741394147) 
* Windows Server 2022 x64 (build 20348) (on winbuilder) [1 NOTE generated] 

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE (on Local Windows and winbuilder) relating to this being a new submission.

1. One NOTE related to this being a new submission.

```
    New submission

```

---

This version addresses the following issues flagged by a previous CRAN reviewer:

- a call to the cat function has been replaced to a call to the message function to enable users to suppress messages to console; 

- the reference to an arXiv pre-print in the DESCRIPTION file has been updated to adopt the format <doi:10.48550/arXiv.YYMM.NNNNN>.; and

- four functions that were imported from other packages have value tags added to their .Rd files.




