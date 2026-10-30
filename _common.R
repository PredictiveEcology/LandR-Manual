## Runs before every chapter, in a fresh session. Do not set .libPaths() here:
## chapters use whatever library the build is running in.
##
## The settings themselves live in SpaDES.docs, so the three manuals cannot
## drift apart again -- this file's previous contents had already diverged from
## the other two in two places.
SpaDES.docs::manualChunkOptions()
