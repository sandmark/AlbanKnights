@echo off
cd /d %~dp0

if "%~1" == "" goto help
AlbanKnights.exe %1 %2
goto end

:help
start cmd /K AlbanKnights.exe

:end
