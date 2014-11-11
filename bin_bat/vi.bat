@echo off
SET DIR=%~dp0%
for /f "tokens=2 delims=:." %%x in ('chcp') do set cp=%%x
chcp 65001 >nul
"C:\Program Files (x86)\vim\vim74\vim.exe" %*
chcp %cp%  >nul
