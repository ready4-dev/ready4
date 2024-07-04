## Test environments

* Local Windows (mingw32), R 4.3.1 [1 WARNING generated]
* Mac OS macos-13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585744)
* Windows windows-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585842)
* Fedora Linux 38 R Under development (unstable) gcc13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/9800145754/job/27061585644) 
* Windows Server 2022 x64 (build 20348) (on winbuilder at https://win-builder.r-project.org/9PdZ3j1vR23X/00check.log) 

## R CMD check results

There was 1 WARNING (on Local Windows).

- 'qpdf' is needed for checks on size reduction of PDFs

---

This version addresses the following CRAN policy violation:

- 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)'.

To address this issue, this version introduces the get_gracefully() function and deploys it within functions that seek to retrieve internet resources. Functions within which get_gracefully() has been deployed have also be written to exit without failing if get_gracefully() returns NULL (ie when internet resources have not been retrieved).




