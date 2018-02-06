# PSeq [![Build status](https://ci.appveyor.com/api/projects/status/x6dm425cdhvr176r/branch/master?svg=true)](https://ci.appveyor.com/project/Liminiens/pseq/branch/master) [![NuGet](https://img.shields.io/badge/NuGet--red.svg)](https://www.nuget.org/packages/FSharp.Collections.ParallelSeq.Standard/)

Port of [FSharp.Collections.ParallelSeq](https://github.com/fsprojects/FSharp.Collections.ParallelSeq) to .NET Standard 2.0

Library itself isn't changed.

Changed NUnit tests to Xunit and build infrastructure to [NUKE Build](https://github.com/nuke-build/nuke)

## Build
To build, run:
```
.\build.ps1 -target Compile -configuration Release
```
Library will be in a `output/lib` folder

## Test
To test, run:
```
.\build.ps1 -target Test -configuration Release
```
## Pack
```
.\build.ps1 -target Pack -configuration Release
```
Packages will be in a `output/packages` folder
## Publish
To publish to nuget.org, run
```
.\build.ps1 -target Publish -configuration Release -ApiKey your_key
```
