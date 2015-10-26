@echo off
SET DIR=%~dp0%
for /f "tokens=2 delims=:." %%x in ('chcp') do set cp=%%x
chcp 65001 >nul
SET LANG=en_US.UTF-8
"c:\usr\vim\vim74\vim.exe" %*
chcp %cp%  >nul
