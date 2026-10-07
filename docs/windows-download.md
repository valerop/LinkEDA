# Descargar e instalar LinkEDA en Windows

## Elegir el instalador

- `Instalar-LinkEDA-<versión>-Windows.exe` es la descarga **estándar**. Usa la
  versión compatible más reciente de R que ya esté instalada en el equipo.
- `Instalar-LinkEDA-<versión>-Windows-Completo.exe` es la descarga
  **completa**. Incluye una copia privada de R y funciona aunque el equipo no
  tenga R. Es la opción más sencilla para quien solo quiere usar LinkEDA.

Ambos instaladores se ejecutan con doble clic, descargan desde CRAN los
paquetes de R obligatorios y crean el acceso directo **LinkEDA** en el
escritorio. El acceso directo abre directamente la aplicación; no hace falta
abrir R ni escribir ningún comando.

La edición estándar instala LinkEDA en la biblioteca personal del R elegido,
por lo que también se puede usar `LinkEDA::LinkEDA(datos)` desde una sesión de
R. No escribe en la biblioteca del sistema.

La distribución completa guarda R dentro de
`%LOCALAPPDATA%\Programs\LinkEDA\runtime`. No cambia la instalación de R del
usuario, no modifica `PATH` ni registra esa copia como el R general del equipo.
La biblioteca de paquetes de la edición completa también queda dentro de ese
directorio y no aparece en las sesiones normales de R del usuario.

No hacen falta Rtools, Visual Studio, MSIX ni el modo desarrollador. Se
necesita Windows 11 de 64 bits y acceso a internet durante la instalación de
los paquetes de R. El instalador estándar necesita R 4.1 o posterior.

El ZIP correspondiente contiene los mismos archivos y permite iniciar la
instalación haciendo doble clic en `Instalar LinkEDA.cmd` después de
descomprimirlo.

Algunas funciones estadísticas usan paquetes opcionales. LinkEDA los instala
en su propia biblioteca cuando el usuario acepta la instalación solicitada
por la aplicación.

Las descargas actuales no están firmadas con un certificado de publicación.
Según la configuración de seguridad de Windows, la primera ejecución puede
requerir una confirmación de SmartScreen. No es necesario activar el modo
desarrollador.

## Licencias y fuentes

Los dos instaladores incluyen el archivo de fuentes correspondiente a su
versión de LinkEDA. El instalador completo conserva además los avisos de R e
incluye el archivo de fuentes exacto de la versión privada de R. Los detalles
están en `TERCEROS-Y-LICENCIAS.md`.

## Preparar las dos descargas

En una máquina de compilación con Visual Studio, Windows SDK y R, guarde el
archivo de fuentes de la misma versión de R en
`build/third-party-source/R-<versión>.tar.gz` y ejecute:

```powershell
& .\scripts\build-windows-download.ps1 `
  -RHome 'C:\Program Files\R\R-4.4.2' `
  -MSBuildPath 'C:\Program Files\Microsoft Visual Studio\18\Insiders\MSBuild\Current\Bin\MSBuild.exe'
```

También se puede indicar otra ubicación mediante `-RSourceArchive`. Se crean:

- `build/Instalar-LinkEDA-<versión>-Windows.exe`
- `build/Instalar-LinkEDA-<versión>-Windows-Completo.exe`
- los dos ZIP equivalentes.

La máquina de compilación necesita Visual Studio. El equipo que instala
LinkEDA no lo necesita. El proceso comprueba que la copia de R es trasladable,
que su biblioteca contiene solo paquetes base o recomendados y que se incluye
el archivo de fuentes correspondiente.
