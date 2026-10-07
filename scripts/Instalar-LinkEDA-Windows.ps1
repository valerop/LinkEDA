param([switch]$NoLaunch, [switch]$NoShortcut)

$ErrorActionPreference = 'Stop'
$installer = Join-Path $PSScriptRoot 'Instalar-LinkEDA-Windows.R'
$launcherSource = Join-Path $PSScriptRoot 'LinkEDA.exe'
$launcherScriptSource = Join-Path $PSScriptRoot 'LinkEDA-Launcher.R'
$metadata = Join-Path $PSScriptRoot 'LinkEDA-DESCRIPTION'
foreach ($required in @($installer, $launcherSource, $launcherScriptSource, $metadata)) {
    if (-not (Test-Path -LiteralPath $required -PathType Leaf)) {
        throw "Falta un archivo del instalador: $required"
    }
}

$description = Get-Content -LiteralPath $metadata -Raw
$linkedaVersion = ([regex]::Match($description, '(?m)^Version: ([^\r\n]+)')).Groups[1].Value
if (-not $linkedaVersion) { throw 'No se pudo leer la versión de LinkEDA.' }

$installRoot = Join-Path $env:LOCALAPPDATA 'Programs\LinkEDA'
$libraryRoot = Join-Path $installRoot 'library'
$sourceRoot = Join-Path $installRoot 'source'
New-Item -ItemType Directory -Path $installRoot, $libraryRoot, $sourceRoot -Force | Out-Null

function Get-RVersion([string]$Rscript) {
    try {
        $reported = & $Rscript --vanilla --slave -e "cat(as.character(getRversion()))" 2>$null
        if ($LASTEXITCODE -ne 0) { return $null }
        return [version](($reported | Select-Object -Last 1).Trim())
    } catch { return $null }
}

function Get-RHome([string]$Rscript) {
    $bin = Split-Path -Parent $Rscript
    if ((Split-Path -Leaf $bin) -ieq 'x64') { $bin = Split-Path -Parent $bin }
    return (Split-Path -Parent $bin)
}

$bundledRuntimeRoot = Join-Path $PSScriptRoot 'runtime'
$bundledRuntime = Get-ChildItem -LiteralPath $bundledRuntimeRoot -Directory -ErrorAction SilentlyContinue |
    Where-Object { Test-Path -LiteralPath (Join-Path $_.FullName 'bin\Rscript.exe') -PathType Leaf } |
    Select-Object -First 1

if ($bundledRuntime) {
    $runtimeParent = Join-Path $installRoot 'runtime'
    New-Item -ItemType Directory -Path $runtimeParent -Force | Out-Null
    $runtimeHome = Join-Path $runtimeParent $bundledRuntime.Name
    Write-Host "Instalando la copia privada de R $($bundledRuntime.Name -replace '^R-', '')..."
    if (Test-Path -LiteralPath $runtimeHome -PathType Container) {
        Copy-Item -Path (Join-Path $bundledRuntime.FullName '*') -Destination $runtimeHome `
            -Recurse -Force
    } else {
        Copy-Item -LiteralPath $bundledRuntime.FullName -Destination $runtimeParent -Recurse
    }
    $rscript = Join-Path $runtimeHome 'bin\Rscript.exe'
    $runtimeKind = 'privada'
} else {
    $candidates = [System.Collections.Generic.List[string]]::new()
    foreach ($key in @('HKCU:\SOFTWARE\R-core\R', 'HKLM:\SOFTWARE\R-core\R',
                       'HKCU:\SOFTWARE\WOW6432Node\R-core\R',
                       'HKLM:\SOFTWARE\WOW6432Node\R-core\R')) {
        $entry = Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue
        if ($entry -and $entry.InstallPath) {
            $candidate = Join-Path $entry.InstallPath 'bin\Rscript.exe'
            if (Test-Path -LiteralPath $candidate -PathType Leaf) { $candidates.Add($candidate) }
        }
    }
    foreach ($root in @((Join-Path $env:ProgramFiles 'R'),
                        (Join-Path ${env:ProgramFiles(x86)} 'R'),
                        (Join-Path $env:LOCALAPPDATA 'Programs\R'))) {
        if (Test-Path -LiteralPath $root -PathType Container) {
            Get-ChildItem -LiteralPath $root -Directory -Filter 'R-*' | ForEach-Object {
                $candidate = Join-Path $_.FullName 'bin\Rscript.exe'
                if (Test-Path -LiteralPath $candidate -PathType Leaf) { $candidates.Add($candidate) }
            }
        }
    }
    $command = Get-Command 'Rscript.exe' -ErrorAction SilentlyContinue
    if ($command) { $candidates.Add($command.Source) }

    $compatible = foreach ($candidate in ($candidates | Select-Object -Unique)) {
        $candidateVersion = Get-RVersion $candidate
        if ($candidateVersion -and $candidateVersion -ge [version]'4.1.0') {
            [pscustomobject]@{ Path = $candidate; Version = $candidateVersion }
        }
    }
    $selected = $compatible | Sort-Object Version -Descending | Select-Object -First 1
    if (-not $selected) {
        throw ('No se encontró R 4.1 o posterior. Use el instalador completo de LinkEDA, ' +
               'que ya incluye una copia privada de R.')
    }
    $rscript = $selected.Path
    $runtimeHome = Get-RHome $rscript
    $runtimeKind = 'del sistema'
}

$rVersion = Get-RVersion $rscript
if (-not $rVersion -or $rVersion -lt [version]'4.1.0') {
    throw "La copia de R seleccionada no es compatible: $rscript"
}
$series = "$($rVersion.Major).$($rVersion.Minor)"
if ($runtimeKind -eq 'privada') {
    $privateLibrary = Join-Path $libraryRoot $series
} else {
    # Keep the standard edition visible from the user's ordinary R sessions.
    $reportedLibrary = (& $rscript --vanilla --slave -e `
        "cat(path.expand(Sys.getenv('R_LIBS_USER')))" 2>$null | Select-Object -Last 1).Trim()
    if ($reportedLibrary) {
        $privateLibrary = $reportedLibrary
    } else {
        $privateLibrary = Join-Path $env:LOCALAPPDATA "R\win-library\$series"
    }
}
New-Item -ItemType Directory -Path $privateLibrary -Force | Out-Null

$env:R_HOME = $runtimeHome
$env:R_LIBS_USER = $privateLibrary
Write-Host "Usando R $rVersion ($runtimeKind): $rscript"
Write-Host 'Instalando los paquetes necesarios y LinkEDA...'
& $rscript --vanilla $installer
if ($LASTEXITCODE -ne 0) { throw 'La instalación de LinkEDA ha fallado.' }

$launcher = Join-Path $installRoot 'LinkEDA.exe'
$launcherScript = Join-Path $installRoot 'LinkEDA-Launcher.R'
Copy-Item -LiteralPath $launcherSource -Destination $launcher -Force
Copy-Item -LiteralPath $launcherScriptSource -Destination $launcherScript -Force

$configuration = @(
    "R_HOME=$runtimeHome"
    "RSCRIPT=$rscript"
    "R_LIBS_USER=$privateLibrary"
    "LINKEDA_VERSION=$linkedaVersion"
)
Set-Content -LiteralPath (Join-Path $installRoot 'LinkEDA-runtime.conf') `
    -Value $configuration -Encoding utf8

$bundledSources = Join-Path $PSScriptRoot 'source'
if (Test-Path -LiteralPath $bundledSources -PathType Container) {
    Copy-Item -Path (Join-Path $bundledSources '*') -Destination $sourceRoot -Recurse -Force
}
$notice = Join-Path $PSScriptRoot 'TERCEROS-Y-LICENCIAS.md'
if (Test-Path -LiteralPath $notice -PathType Leaf) {
    Copy-Item -LiteralPath $notice -Destination $installRoot -Force
}

Write-Host "LinkEDA $linkedaVersion está instalado." -ForegroundColor Green
if (-not $NoShortcut) {
    $desktop = [Environment]::GetFolderPath('Desktop')
    $shortcutPath = Join-Path $desktop 'LinkEDA.lnk'
    $shell = New-Object -ComObject WScript.Shell
    $shortcut = $shell.CreateShortcut($shortcutPath)
    $shortcut.TargetPath = $launcher
    $shortcut.WorkingDirectory = $installRoot
    $shortcut.IconLocation = "$launcher,0"
    $shortcut.Description = 'LinkEDA'
    $shortcut.Save()
    Write-Host "Acceso directo creado: $shortcutPath"
}
if (-not $NoLaunch) { Start-Process -FilePath $launcher }
