## Test environments

* Local Windows (mingw32), R version 4.4.1 (2024-06-14 ucrt) [1 WARNING generated]
* Mac OS macos-13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585744)
* Windows windows-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585842)
* Fedora Linux 38 R Under development (unstable) gcc13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585644) 
* Windows Server 2022 x64 (build 20348) (on winbuilder at https://win-builder.r-project.org/9PdZ3j1vR23X/00check.log) 

## R CMD check results

There was 1 WARNING (on Local Windows).

  Maintainer: 'Matthew Hamilton <matthew.hamilton1@monash.edu>'
  
  New submission
  
  Package was archived on CRAN
  
  Version contains large components (0.1.17.9000)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2024-08-20 for policy violation
  
    On Internet access

---

This version addresses the following CRAN policy violation:

- 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)'.

To address this issue, this version introduces the get_gracefully() function and deploys it within functions that seek to retrieve internet resources. Functions within which get_gracefully() has been deployed have also be written to exit without failing if get_gracefully() returns NULL (ie when internet resources have not been retrieved).




