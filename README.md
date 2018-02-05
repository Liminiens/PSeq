# PSeq

Port of [FSharp.Collections.ParallelSeq](https://github.com/fsprojects/FSharp.Collections.ParallelSeq) to .NET Standard 2.0

Library itself isn't changed.

Changed NUnit tests to Xunit and build infrastructure to [NUKE Build](https://github.com/nuke-build/nuke)

## Build
To build, run:
```
.\build.ps1 -target Compile -configuration Release
```
Library will be in a `output/lib` folder
