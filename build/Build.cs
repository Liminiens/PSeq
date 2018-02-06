using System;
using System.Linq;
using Nuke.Common;
using Nuke.Common.Git;
using Nuke.Common.Tools.DotNet;
using Nuke.Common.Tools.GitVersion;
using Nuke.Core;
using Nuke.Common.Tools.Xunit;
using Nuke.Core.Tooling;
using static Nuke.Common.Tools.DotNet.DotNetTasks;
using static Nuke.Core.IO.FileSystemTasks;
using static Nuke.Core.IO.PathConstruction;
using static Nuke.Core.EnvironmentInfo;

class Build : NukeBuild
{
    // Console application entry. Also defines the default target.
    public static int Main() => Execute<Build>(x => x.Compile);

    [GitVersion] readonly GitVersion GitVersion;

    [GitRepository] readonly GitRepository GitRepository;

    [Parameter("API key for nuget.org")] readonly string ApiKey;

    Target Clean => _ => _
        .Executes(() =>
        {
            DeleteDirectories(GlobDirectories(SourceDirectory, "**/bin", "**/obj"));
            EnsureCleanDirectory(OutputDirectory);
        });

    Target Restore => _ => _
        .DependsOn(Clean)
        .Executes(() =>
        {
            DotNetRestore(s => DefaultDotNetRestore);
        });

    Target Compile => _ => _
        .DependsOn(Restore)
        .Executes(() =>
        {
            DotNetBuild(s =>
                DefaultDotNetBuild.SetAssemblyVersion(GitVersion.AssemblySemVer)
                .SetOutputDirectory(OutputDirectory / "lib"));
        });

    Target Test => _ => _
        .DependsOn(Compile)
        .Executes(() =>
        {
            DotNetTest(s => s.SetProjectFile(SolutionDirectory / "tests" / "FSharp.Collections.ParallelSeq.Standard.Tests" / "FSharp.Collections.ParallelSeq.Standard.Tests.fsproj"));
        });

    Target Pack => _ => _
        .DependsOn(Compile)
        .Executes(() =>
        {
            DotNetPack(s =>
                s.SetOutputDirectory(OutputDirectory / "packages")
                    .SetAuthors("Liminiens")
                    .SetPackageLicenseUrl("https://github.com/Liminiens/PSeq/blob/master/LICENSE")
                    .SetPackageProjectUrl("https://github.com/Liminiens/PSeq")
                    .SetDescription("Unofficial port of PSeq library to .NET Standard. Parallel sequence operators for F#. See the original documentation: http://fsprojects.github.io/FSharp.Collections.ParallelSeq/")
                    .SetPackageTags("Parallel FSharp")
                    .SetCopyright("Copyright 2018")
                    .SetVersion(GitVersion.SemVer)
                    .SetIncludeSymbols(true));
        });

    Target Publish => _ => _
        .Requires(() => ApiKey)
        .DependsOn(Pack)
        .Executes(() =>
        {
            var packages = GlobFiles(OutputDirectory / "packages", "*.nupkg").Where(x => !x.Contains(".symbols."));
            foreach (var package in packages)
            {
                DotNetNuGetPush(s => s
                    .SetTargetPath(package)
                    .SetApiKey(ApiKey)
                    .SetSource("https://api.nuget.org/v3/index.json"));
            }
        });
}
