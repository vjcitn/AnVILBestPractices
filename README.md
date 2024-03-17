# AnVILBestPractices for workspaces involved with interactive analysis in AnVIL

- The workspace code and documentation is managed in a github repository
    - A github action (that will typically be independent of AnVIL's cloud
resources) can be used to check or enhance the package on
change events

- It is recommended that the repository be organized as an R package
    - R packages can be evaluated with automated quality control procedures
    - Package metadata formally defines requirements for associated software tools
    - Documentation of workspace resources can be standardized

- The README.md can be transformed into the workspace Description
component

- The README.md should include "badges" to rapidly indicate package status
to individuals visiting the workspace
