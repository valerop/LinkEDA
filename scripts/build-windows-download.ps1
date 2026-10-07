param(
    [Parameter(Mandatory = $true)][string]$RHome,
    [Parameter(Mandatory = $true)][string]$MSBuildPath,
    [string]$RSourceArchive,
    [switch]$ReuseWinUI
)

$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$rExe = Join-Path $RHome 'bin\R.exe'
$rscriptExe = Join-Path $RHome 'bin\Rscript.exe'
$project = Join-Path $repo 'src\platform\windows\winui\LinkEDA\LinkEDA.vcxproj'
foreach ($required in @($rExe, $rscriptExe, $MSBuildPath, $project)) {
    if (-not (Test-Path -LiteralPath $required -PathType Leaf)) {
        throw "Missing build input: $required"
    }
}

$runtimeVersion = (& $rscriptExe --vanilla --slave -e `
    "cat(as.character(getRversion()))" 2>$null | Select-Object -Last 1).Trim()
if (-not $runtimeVersion) { throw 'Could not determine the bundled R version.' }
if (-not $RSourceArchive) {
    $RSourceArchive = Join-Path $repo "build\third-party-source\R-$runtimeVersion.tar.gz"
}
if (-not (Test-Path -LiteralPath $RSourceArchive -PathType Leaf)) {
    throw ("Missing corresponding R source archive: $RSourceArchive`n" +
           "Download the exact R $runtimeVersion source archive from CRAN and pass -RSourceArchive.")
}

$nonBasePackages = & $rscriptExe --vanilla --slave -e `
    "ip <- installed.packages(lib.loc = R.home('library')); extra <- ip[is.na(ip[, 'Priority']), 'Package']; cat(setdiff(extra, 'translations'), sep='\n')" `
    2>$null
if ($nonBasePackages) {
    throw ('RHome contains non-base packages that cannot be redistributed automatically: ' +
           ($nonBasePackages -join ', '))
}

$build = Join-Path $repo 'build'
$portable = Join-Path $build 'winui-portable'
$intermediate = Join-Path $build 'winui-portable-obj'
$stage = Join-Path $build 'windows-release-stage'
$sourceStage = Join-Path $build 'windows-source-stage'
$launcherBuild = Join-Path $build 'windows-launcher'
$release = Join-Path $build 'windows-download'
$releaseComplete = Join-Path $build 'windows-download-complete'
$projectDirectory = Split-Path -Parent $project
$projectGenerated = Join-Path $projectDirectory 'Generated Files'
$projectObject = Join-Path $projectDirectory 'obj'
New-Item -ItemType Directory -Path $build -Force | Out-Null

$resetTargets = @($stage, $sourceStage, $launcherBuild, $release, $releaseComplete)
if (-not $ReuseWinUI) { $resetTargets = @($portable, $intermediate) + $resetTargets }
foreach ($target in $resetTargets) {
    $parent = [IO.Path]::GetFullPath((Split-Path -Parent $target))
    if ($parent -ne [IO.Path]::GetFullPath($build)) {
        throw "Unsafe build reset path: $target"
    }
    if (Test-Path -LiteralPath $target) {
        Remove-Item -LiteralPath $target -Recurse -Force
    }
    New-Item -ItemType Directory -Path $target | Out-Null
}
foreach ($target in $(if ($ReuseWinUI) { @() } else { @($projectGenerated, $projectObject) })) {
    $parent = [IO.Path]::GetFullPath((Split-Path -Parent $target))
    if ($parent -ne [IO.Path]::GetFullPath($projectDirectory)) {
        throw "Unsafe generated-file reset path: $target"
    }
    if (Test-Path -LiteralPath $target) {
        Remove-Item -LiteralPath $target -Recurse -Force
    }
}

if (-not $ReuseWinUI) {
    & $MSBuildPath $project /t:Build /p:Configuration=Release /p:Platform=x64 `
        /p:AppxPackage=false /p:WindowsPackageType=None `
        /p:WindowsAppSDKSelfContained=true /p:SelfContained=true `
        "/p:OutDir=$portable\" "/p:IntDir=$intermediate\" /m:4 /v:minimal
    if ($LASTEXITCODE -ne 0) { throw 'Portable WinUI build failed.' }
}
$app = Join-Path $portable 'LinkEDA.exe'
if (-not (Test-Path -LiteralPath $app -PathType Leaf)) {
    throw 'Portable WinUI executable missing.'
}

$probeSocket = [Net.Sockets.TcpListener]::new([Net.IPAddress]::Loopback, 0)
try {
    $probeSocket.Start()
    $probePort = ([Net.IPEndPoint]$probeSocket.LocalEndpoint).Port
} finally {
    $probeSocket.Stop()
}
$previousPort = $env:LINKEDA_WINUI_PORT
$env:LINKEDA_WINUI_PORT = [string]$probePort
$probeProcess = Start-Process -FilePath $app -WindowStyle Hidden -PassThru
$env:LINKEDA_WINUI_PORT = $previousPort
$probeReply = $null
try {
    for ($attempt = 0; $attempt -lt 80; $attempt++) {
        if ($probeProcess.HasExited) { break }
        Start-Sleep -Milliseconds 250
        try {
            $client = [Net.Sockets.TcpClient]::new('127.0.0.1', $probePort)
            try {
                $stream = $client.GetStream()
                $stream.ReadTimeout = 1000
                $writer = [IO.StreamWriter]::new($stream, [Text.Encoding]::ASCII)
                $writer.Write("PING`n__LINKEDA_TCP_MESSAGE_END__`n")
                $writer.Flush()
                $reader = [IO.StreamReader]::new($stream)
                $probeReply = $reader.ReadLine()
            } finally {
                $client.Close()
            }
            break
        } catch {}
    }
} finally {
    if (-not $probeProcess.HasExited) { Stop-Process -Id $probeProcess.Id -Force }
}
if ($probeReply -ne 'OK') { throw 'Portable WinUI failed its startup PING probe.' }

$package = Join-Path $stage 'LinkEDA'
New-Item -ItemType Directory -Path $package | Out-Null
foreach ($item in @('DESCRIPTION', 'NAMESPACE', 'LICENSE', 'NEWS.md', 'README.md',
                    'LICENSE-DOCUMENTATION.md', '.Rbuildignore')) {
    $source = Join-Path $repo $item
    if (Test-Path -LiteralPath $source -PathType Leaf) {
        Copy-Item -LiteralPath $source -Destination $package
    }
}
foreach ($item in @('R', 'man', 'inst')) {
    $source = Join-Path $repo $item
    if (Test-Path -LiteralPath $source -PathType Container) {
        Copy-Item -LiteralPath $source -Destination $package -Recurse
    }
}

# The Windows distribution has no R DLL or configure script. Native analysis
# commands run in the bundled WinUI process and calculations run in R.
$namespace = Join-Path $package 'NAMESPACE'
$namespaceLines = @(Get-Content -LiteralPath $namespace |
    Where-Object { $_ -notmatch '^useDynLib\(LinkEDA,' })
Set-Content -LiteralPath $namespace -Value $namespaceLines -Encoding utf8
$descriptionPath = Join-Path $package 'DESCRIPTION'
$description = Get-Content -LiteralPath $descriptionPath -Raw
$description = $description -replace '(?ms)^SystemRequirements:.*?(?=^[A-Za-z][A-Za-z0-9]*:)',
    "SystemRequirements: Windows 11 (x64); no developer tools required.`r`n"
$description = $description.TrimEnd() + "`r`nOS_type: windows`r`n"
Set-Content -LiteralPath $descriptionPath -Value $description -Encoding utf8
$version = ([regex]::Match($description, '(?m)^Version: ([^\r\n]+)')).Groups[1].Value

$appDestination = Join-Path $package 'inst\bin\winui'
New-Item -ItemType Directory -Path $appDestination -Force | Out-Null
Copy-Item -Path (Join-Path $portable '*') -Destination $appDestination -Recurse -Force
Get-ChildItem -LiteralPath $appDestination -Recurse -File |
    Where-Object { $_.Extension -in @('.pdb', '.ilk', '.exp', '.lib', '.obj') } |
    Remove-Item -Force

Push-Location $release
try {
    & $rExe CMD build $package --no-build-vignettes --no-manual
    if ($LASTEXITCODE -ne 0) { throw 'Windows distribution archive build failed.' }
    $archive = Get-ChildItem -File -Filter 'LinkEDA_*.tar.gz' | Select-Object -First 1
    if (-not $archive) { throw 'Windows distribution archive missing.' }
    $windowsName = $archive.Name -replace '[.]tar[.]gz$', '_Windows.tar.gz'
    Rename-Item -LiteralPath $archive.FullName -NewName $windowsName
    Copy-Item -LiteralPath $descriptionPath -Destination (Join-Path $release 'LinkEDA-DESCRIPTION')
} finally {
    Pop-Location
}

# Include the exact preferred sources that correspond to the native executable.
# Stage them explicitly so generated WinUI/NuGet directories are never scanned
# or copied into the GPL source bundle.
$sourcePackage = Join-Path $sourceStage "LinkEDA-$version"
New-Item -ItemType Directory -Path $sourcePackage -Force | Out-Null
foreach ($item in @('DESCRIPTION', 'NAMESPACE', 'LICENSE',
                    'LICENSE-DOCUMENTATION.md', 'NEWS.md', 'README.md',
                    'configure', 'cleanup')) {
    $source = Join-Path $repo $item
    if (Test-Path -LiteralPath $source -PathType Leaf) {
        Copy-Item -LiteralPath $source -Destination $sourcePackage
    }
}
foreach ($item in @('R', 'man', 'inst')) {
    Copy-Item -LiteralPath (Join-Path $repo $item) -Destination $sourcePackage -Recurse
}
$sourceScripts = Join-Path $sourcePackage 'scripts'
New-Item -ItemType Directory -Path $sourceScripts -Force | Out-Null
foreach ($item in @('build-windows-download.ps1', 'Instalar-LinkEDA-Windows.ps1',
                    'Instalar-LinkEDA-Windows.R', 'Instalar LinkEDA.cmd')) {
    Copy-Item -LiteralPath (Join-Path $repo "scripts\$item") -Destination $sourceScripts
}
Copy-Item -LiteralPath (Join-Path $repo 'scripts\windows-launcher') `
    -Destination $sourceScripts -Recurse
Copy-Item -LiteralPath (Join-Path $repo 'scripts\windows-self-extractor') `
    -Destination $sourceScripts -Recurse
function Copy-PreferredSourceTree([string]$Source, [string]$Destination) {
    New-Item -ItemType Directory -Path $Destination -Force | Out-Null
    $skippedDirectories = @('bin', 'obj', 'packages', 'Generated Files',
                            'AppPackages', '.vs')
    $skippedExtensions = @('.o', '.obj', '.dll', '.so', '.dylib', '.rds',
                           '.pdb', '.ilk', '.exp', '.lib')
    foreach ($entry in Get-ChildItem -LiteralPath $Source -Force) {
        if ($entry.PSIsContainer) {
            if ($entry.Name -in $skippedDirectories) { continue }
            Copy-PreferredSourceTree $entry.FullName (Join-Path $Destination $entry.Name)
        } elseif ($entry.Extension -notin $skippedExtensions) {
            Copy-Item -LiteralPath $entry.FullName -Destination (Join-Path $Destination $entry.Name)
        }
    }
}
Copy-PreferredSourceTree (Join-Path $repo 'src') (Join-Path $sourcePackage 'src')
$sourceArchive = Join-Path $sourceStage "LinkEDA_$($version)_Source.zip"
Compress-Archive -Path (Join-Path $sourcePackage '*') -DestinationPath $sourceArchive `
    -CompressionLevel Optimal
if (-not (Test-Path -LiteralPath $sourceArchive -PathType Leaf)) {
    throw 'LinkEDA source archive missing.'
}
$distributionSource = Join-Path $release 'source'
New-Item -ItemType Directory -Path $distributionSource -Force | Out-Null
Copy-Item -LiteralPath $sourceArchive -Destination `
    (Join-Path $distributionSource "LinkEDA_$($version)_Source.zip")

$compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework64\v4.0.30319\csc.exe'
if (-not (Test-Path -LiteralPath $compiler -PathType Leaf)) {
    $compiler = Join-Path $env:WINDIR 'Microsoft.NET\Framework\v4.0.30319\csc.exe'
}
$bootstrap = Join-Path $PSScriptRoot 'windows-self-extractor\InstallerBootstrap.cs'
$launcherSource = Join-Path $PSScriptRoot 'windows-launcher\LinkEDALauncher.cs'
$launcherR = Join-Path $PSScriptRoot 'windows-launcher\LinkEDA-Launcher.R'
$icon = Join-Path $repo 'src\platform\windows\winui\LinkEDA\Assets\LinkEDA.ico'
foreach ($required in @($compiler, $bootstrap, $launcherSource, $launcherR, $icon)) {
    if (-not (Test-Path -LiteralPath $required -PathType Leaf)) {
        throw "Missing installer input: $required"
    }
}
$launcherExe = Join-Path $launcherBuild 'LinkEDA.exe'
& $compiler /nologo /target:winexe /platform:anycpu /optimize+ `
    "/out:$launcherExe" "/win32icon:$icon" /reference:System.dll `
    /reference:System.Windows.Forms.dll $launcherSource
if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $launcherExe -PathType Leaf)) {
    throw 'Windows desktop launcher build failed.'
}

foreach ($copy in @(
    @{ Source = (Join-Path $PSScriptRoot 'Instalar-LinkEDA-Windows.R'); Name = 'Instalar-LinkEDA-Windows.R' },
    @{ Source = (Join-Path $PSScriptRoot 'Instalar-LinkEDA-Windows.ps1'); Name = 'Instalar-LinkEDA-Windows.ps1' },
    @{ Source = (Join-Path $PSScriptRoot 'Instalar LinkEDA.cmd'); Name = 'Instalar LinkEDA.cmd' },
    @{ Source = $launcherExe; Name = 'LinkEDA.exe' },
    @{ Source = $launcherR; Name = 'LinkEDA-Launcher.R' },
    @{ Source = (Join-Path $repo 'docs\windows-download.md'); Name = 'LEEME.md' },
    @{ Source = (Join-Path $repo 'docs\windows-third-party.md'); Name = 'TERCEROS-Y-LICENCIAS.md' }
)) {
    Copy-Item -LiteralPath $copy.Source -Destination (Join-Path $release $copy.Name)
}

function New-Zip([string]$SourceDirectory, [string]$Destination) {
    if (Test-Path -LiteralPath $Destination) { Remove-Item -LiteralPath $Destination -Force }
    Compress-Archive -Path (Join-Path $SourceDirectory '*') -DestinationPath $Destination `
        -CompressionLevel Optimal
}

function New-SelfExtractor([string]$Payload, [string]$Destination) {
    if (Test-Path -LiteralPath $Destination) { Remove-Item -LiteralPath $Destination -Force }
    & $compiler /nologo /target:exe /platform:anycpu /optimize+ `
        "/out:$Destination" "/resource:$Payload,LinkEDA.Payload" `
        "/win32icon:$icon" /reference:System.IO.Compression.dll $bootstrap
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path -LiteralPath $Destination -PathType Leaf)) {
        throw "Windows self-extracting installer build failed: $Destination"
    }
}

$bundle = Join-Path $build "LinkEDA-$version-Windows.zip"
$selfExtractor = Join-Path $build "Instalar-LinkEDA-$version-Windows.exe"
New-Zip $release $bundle
New-SelfExtractor $bundle $selfExtractor

# The complete installer carries an isolated, relocatable copy of R. Its exact
# corresponding source archive is retained beside the installed sources.
Copy-Item -Path (Join-Path $release '*') -Destination $releaseComplete -Recurse -Force
$runtimeDestination = Join-Path $releaseComplete "runtime\R-$runtimeVersion"
New-Item -ItemType Directory -Path $runtimeDestination -Force | Out-Null
Get-ChildItem -LiteralPath $RHome -Force | ForEach-Object {
    if ($_.Name -notlike 'unins000.*') {
        Copy-Item -LiteralPath $_.FullName -Destination $runtimeDestination -Recurse -Force
    }
}
$completeSource = Join-Path $releaseComplete 'source'
Copy-Item -LiteralPath $RSourceArchive -Destination `
    (Join-Path $completeSource "R-$runtimeVersion.tar.gz") -Force
$rSourceHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $RSourceArchive).Hash
@(
    "Private R runtime: R $runtimeVersion for Windows x64"
    "Official project: https://www.r-project.org/"
    "Licence: GPL-2 | GPL-3; see runtime\R-$runtimeVersion\COPYING"
    "Copyright notices: runtime\R-$runtimeVersion\doc\COPYRIGHTS"
    "Corresponding source: source\R-$runtimeVersion.tar.gz"
    "Source SHA256: $rSourceHash"
) | Set-Content -LiteralPath (Join-Path $releaseComplete 'R-RUNTIME.txt') -Encoding utf8

$privateRscript = Join-Path $runtimeDestination 'bin\Rscript.exe'
$relocatedVersion = (& $privateRscript --vanilla --slave -e `
    "stopifnot(normalizePath(R.home(), winslash='/') == normalizePath(commandArgs(TRUE)[1], winslash='/')); cat(as.character(getRversion()))" `
    $runtimeDestination 2>$null | Select-Object -Last 1).Trim()
if ($relocatedVersion -ne $runtimeVersion) {
    throw 'The copied private R runtime failed its relocation check.'
}

$completeBundle = Join-Path $build "LinkEDA-$version-Windows-Completo.zip"
$completeInstaller = Join-Path $build "Instalar-LinkEDA-$version-Windows-Completo.exe"
New-Zip $releaseComplete $completeBundle
New-SelfExtractor $completeBundle $completeInstaller

Write-Host "Windows standard ZIP ready: $bundle"
Write-Host "Windows standard installer ready: $selfExtractor"
Write-Host "Windows complete ZIP ready: $completeBundle"
Write-Host "Windows complete installer ready: $completeInstaller"
