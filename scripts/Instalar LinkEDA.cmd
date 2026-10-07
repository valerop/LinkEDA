@echo off
setlocal
title Instalar LinkEDA
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Instalar-LinkEDA-Windows.ps1"
if errorlevel 1 (
    echo.
    echo La instalacion no se ha completado. Revise el mensaje anterior.
    pause
    exit /b 1
)
echo.
echo LinkEDA esta instalado y tiene un acceso directo en el escritorio.
exit /b 0
