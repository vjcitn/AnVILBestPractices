# AnVILBestPractices for workspaces involved with interactive analysis in AnVIL

- The workspace code and documentation is managed in a github repository
    - A github action (that will typically be independent of AnVIL's cloud
resources) can be used to check or enhance the package on
change events
        - The action will "emulate" an AnVIL cloud environment by using
an endorsed container image
        - The action will also produce and register a Docker container
to ensure that the software employed in the workspace is runnable
in the future

- It is recommended that the repository be organized as an R package
    - R packages can be evaluated with automated quality control procedures
    - Package metadata formally defines requirements for associated software tools
    - Documentation of workspace resources can be standardized

- The README.md can be transformed into the workspace "Dashboard"
component using [AnVILPublish](https://bioconductor.org/packages/AnVILPublish).
    - The README.md should include "badges" to clearly indicate package status
to individuals visiting the workspace

- Vignette code will be inserted into the "Analyses" component of
the workspace by AnVILPublish

Images for the Dashboard can be included

![](https://github.com/vjcitn/AnVILBestPractices/blob/main/hpbox.png?raw=true)
