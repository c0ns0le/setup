Import-Module "PowerTab" -ArgumentList "C:\Users\Brian\Documents\WindowsPowerShell\PowerTabConfig.xml"
Import-Module Pscx

#Invoke-BatchFile "${env:VS100COMNTOOLS}..\..\VC\vcvarsall.bat"

# Load Jump-Location profile
Import-Module 'C:\Users\Brian\Documents\WindowsPowerShell\Modules\Jump.Location\Jump.Location.psd1'

# Load posh-hg example profile
. 'C:\Users\Brian\Documents\WindowsPowerShell\Modules\posh-hg\profile.example.ps1'

. (Resolve-Path "$env:LOCALAPPDATA\GitHub\shell.ps1")

# Load posh-git example profile
. 'C:\Users\Brian\Documents\WindowsPowerShell\Modules\posh-git\profile.example.ps1'

# Load posh-github example profile
. 'C:\Users\Brian\Documents\WindowsPowerShell\Modules\Posh-GitHub\Posh-GitHub-Profile.ps1'

$Env:WORKON_HOME= $env:USERPROFILE+"\.virtualenvs"
$Env:PROJECT_HOME= "c:\dev\python"
Import-Module virtualenvwrapper


New-Alias which where.exe
