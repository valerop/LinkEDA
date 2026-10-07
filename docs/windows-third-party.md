# Componentes y licencias de la distribución de Windows

## LinkEDA

Copyright (c) 2026 Pedro Valero Mora.

El software LinkEDA se distribuye bajo la GNU General Public License, versión
3 o cualquier versión posterior. El texto completo está en `LICENSE`. Cada
instalador de Windows incluye, en la carpeta `source`, el archivo de fuentes de
la misma versión de LinkEDA.

La documentación original de LinkEDA se distribuye bajo CC BY-NC-ND 4.0 con
el alcance y las excepciones indicados en `LICENSE-DOCUMENTATION.md` y
`inst/COPYRIGHTS` dentro del archivo de fuentes.

## R

El instalador completo contiene una copia sin modificar del entorno R para
Windows. R se distribuye bajo GPL-2 o GPL-3; algunos archivos usan LGPL. Los
avisos originales se conservan dentro de la carpeta `runtime`, incluidos
`COPYING` y `doc/COPYRIGHTS`. El archivo de fuentes correspondiente a la
versión incluida se instala en la carpeta `source`.

Información oficial: <https://www.r-project.org/Licenses/>.

## Paquetes de R

Los paquetes adicionales necesarios se descargan como binarios desde CRAN
durante la instalación y conservan sus propias licencias. La edición completa
los instala en la biblioteca privada de LinkEDA. La edición estándar los
instala en la biblioteca personal de la versión de R elegida.

## Componentes de Windows

La aplicación nativa incorpora componentes redistribuibles de Microsoft
Windows App SDK y sus dependencias conforme a sus condiciones de
redistribución. Los componentes mantienen sus avisos y metadatos originales.
