﻿using System;
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

    [Parameter] readonly string ApiKey;

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
            DotNetTest();
        });

    Target Pack => _ => _
        .DependsOn(Compile)
        .Executes(() =>
        {
            DotNetPack(s =>
                s.SetOutputDirectory(OutputDirectory / "packages")
                 .SetVersion(GitVersion.SemVer)
                 .SetIncludeSymbols(true));
        });

    Target Publish => _ => _
        .Requires(() => ApiKey)
        .DependsOn(Pack)
        .Executes(() =>
        {
            var packages = GlobFiles(OutputDirectory / "packages", "*.nupkg");
            foreach (var package in packages)
            {
                DotNetNuGetPush(s => s
                    .SetTargetPath(package)
                    .SetApiKey(ApiKey)
                    .SetSource("http://192.168.0.66:8081/repository/nuget-hosted/"));
            }
        });
}