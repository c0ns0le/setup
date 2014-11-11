Push-Location $PSScriptRoot

$Global:Admin = (whoami /all | select-string S-1-16-12288) -ne $null
$env:ProfileRoot = $PSScriptRoot

Import-Module "PowerTab" -ArgumentList .\PowerTabConfig.xml
Import-Module Pscx

$env:GOPATH = "c:\dev\go"
$env:GOBIN = "c:\usr\local\go\bin"

$env:path += ";c:\dev\go\bin"
$env:path += ";${Env:ProgramFiles(x86)}\Git\bin"

#Invoke-BatchFile "${env:VS100COMNTOOLS}..\..\VC\vcvarsall.bat"
#Invoke-BatchFile "${env:VS90COMNTOOLS}..\..\VC\vcvarsall.bat" amd64

# Load PSReadLine
. .\Modules\PSReadline\PSReadLineProfile.ps1

# Load Jump-Location profile
#Import-Module $PSScriptRoot\Modules\Jump.Location\Jump.Location.psd1
Import-Module Jump.Location

# posh-git/hg profiles
Import-Module posh-hg
Import-Module posh-git
Enable-GitColors
$global:GitPromptSettings.EnableFileStatus = $false

$Env:WORKON_HOME= "$env:USERPROFILE\.virtualenvs"
$Env:PROJECT_HOME= "c:\dev\python"
Import-Module virtualenvwrapper

# Set up prompt, adding the git prompt parts inside git repos
function global:prompt {
    $realLASTEXITCODE = $LASTEXITCODE

    # Reset color, which can be messed up by Enable-GitColors
    $Host.UI.RawUI.ForegroundColor = $GitPromptSettings.DefaultForegroundColor

    Write-Host "`r`n"
    if ($env:VIRTUAL_ENV) {write-host "[" -foregroundcolor yellow -nonewline; write-host $(split-path $env:VIRTUAL_ENV -leaf) -foregroundcolor magenta -nonewline; write-host "] " -foregroundcolor yellow -nonewline}
    Write-Host "<" -ForeGroundColor red -nonewline; Write-Host($pwd.ProviderPath) -nonewline; Write-Host ">" -ForeGroundColor red -nonewline
    Write-VcsStatus

    $global:LASTEXITCODE = $realLASTEXITCODE
    
    $symbol = "`r`n"
    if ($global:Admin) {
        $symbol = $symbol + [char]955
        Write-Host $symbol -foregroundcolor DarkGreen -nonewline}
    else {
        $symbol = $symbol + [char]946
        Write-Host $symbol -foregroundcolor White -nonewline}
    return " "
}

# Print out env:path
function Print-Path {
	($env:path).split(";") | %{write-host $_}
}

# Add custom aliases
New-Alias sn New-Symlink
New-Alias which where.exe
New-Alias git hub
New-Alias pinst Install-Module
New-Alias pup Update-Module
New-Alias ppath Print-Path

Pop-Location