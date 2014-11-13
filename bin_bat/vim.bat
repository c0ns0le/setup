@echo off
SET DIR=%~dp0%
for /f "tokens=2 delims=:." %%x in ('chcp') do set cp=%%x
rem chcp 65001 >nul
SET LANG=en_US.UTF-8
"C:\Program Files (x86)\vim\vim74\vim.exe" %*
rem chcp %cp%  >nul
