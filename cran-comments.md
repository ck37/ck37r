## Test environments

* Local
    * Windows 10: R release
* Travis CI
    * Ubuntu precise (12.04.5): R devel, release
    * macOS 10.11.6: R devel, release
* AppVeyor CI
    * Windows Server 2012 R2: R devel, release
* win-builder: R devel and release

## R CMD check results

There were no ERRORs or WARNINGs.

NOTEs (1):

1. devel: "Imports includes 21 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally."

   - Because this is my personal toolkit across all projects it needs to span
   a large number of packages. I have reviewed the imported packages and do not
   believe that the user experience will be improved by moving any to Suggests.
