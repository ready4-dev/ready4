## Test environments

* Local Windows (mingw32), R version 4.4.1 (2024-06-14 ucrt) [1 NOTE generated]
* Mac OS macos-13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/11098113463/job/30830326617)
* Windows windows-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/11098113463/job/30830326737)
* Fedora Linux 38 R Under development (unstable) gcc13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/11098113463/job/30830326459) 
* Windows Server 2022 x64 (build 20348) (on winbuilder at https://win-builder.r-project.org/sH3ODhP8kLOl/00check.log) [1 NOTE generated]

## R CMD check results

There was 1 NOTE (on Local Windows and on winbuilder):

  Maintainer: 'Matthew Hamilton <matthew.hamilton1@monash.edu>'

  New submission
  
  Package was archived on CRAN
  
  Version contains large components (0.1.17.9001)
  
  Possibly misspelled words in DESCRIPTION:
  al (21:21)
  et (21:18)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2024-08-20 for policy violation
  
    On Internet access

---

This version addresses the following CRAN policy violation:

- 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)'.

In particular, this version addresses bugs in how the get_gracefully() function is used in functions such as make_datasets_tb(). The affected functions have been updated so that get_gracefully() is now called with the not_chr_1L_lgl argument set to TRUE, which will ensure a NULL return when internet resources have not been retrieved.




