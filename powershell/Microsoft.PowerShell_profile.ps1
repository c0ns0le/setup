Push-Location $PSScriptRoot

$Global:Admin = (whoami /all | select-string S-1-16-12288) -ne $null
$env:ProfileRoot = $PSScriptRoot

New-PSDrive -Name HOME -PSProvider FileSystem -Root $env:USERPROFILE >$null

$GitPath = "d:\opt\git"

$env:path += ";$GitPath\cmd;$GitPath\bin;$GitPath\mingw\bin"
$env:path += ";c:\local\bde-tools\bin"

$env:GOPATH = "d:\home\brian\go"
$env:GOBIN = "$env:GOPATH\bin"

$env:path += ";$env:GOBIN"
$env:path += ";$env:ChocolateyInstall\bin"

# Load Modules
Import-Module Pscx
. .\Modules\PSReadline\PSReadLineProfile.ps1
#Import-Module Jump.Location

Import-Module posh-git
$global:GitPromptSettings.EnableFileStatus = $false

$Env:WORKON_HOME= "$Env:USERPROFILE\.virtualenvs"
$Env:PROJECT_HOME= "c:\dev\python"
if (-Not $VIRTUALENVWRAPPER_PYTHON) {$global:VIRTUALENVWRAPPER_PYTHON= "c:\tools\python27\python.exe"}
Import-Module virtualenvwrapper
New-PSDrive -Name WORKON_HOME -PSProvider FileSystem -Root $Env:WORKON_HOME >$null

# Start the devpi server
#if (-Not (tasklist /fi "imagename eq devpi-server.exe" 2>$null | select-string "devpi-server.exe" -Quiet)) {
    #rm $HOME/.devpi\server\.xproc\devpi-server\xprocess.pid 2>$null
    #c:\users\brian\.virtualenvs\devpi\scripts\devpi-server.exe --start}
#else {Write-Host "devpi-server already running..."}
devpi login brian --password=

#Set up prompt, adding the git prompt parts inside git repos
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
        Write-Host $symbol -foregroundcolor cyan -nonewline}
    return " "
}

# Print out env:path
function Print-Path {
	($env:path).split(";") | %{write-host $_}
}

# PostgreSQL
$Env:PostgresPath = "c:\postgresql\9.4"
$Env:PostgresData = "c:\data\postgresql"
function Postgres-Start {
    cmd /c "$Env:PostgresPath\bin\pg_ctl.exe" start -D $Env:PostgresData -w
}
function Postgres-Stop {
    cmd /c "$Env:PostgresPath\bin\pg_ctl.exe" stop -D $Env:PostgresData -w
}
function Postgres-Command ([switch]$start, [switch]$stop) {
    If ($stop) {Postgres-Stop}
    elseif ($start) {Postgres-Start}
}

# MongoDB
$Env:MongoPath = "c:\mongodb"
$Env:MongoData = "c:\data\mongodb\db"
$Env:MongoLog = "c:\data\mongodb\log"
function Mongodb-Start {
    net start MongoDB    
}
function Mongodb-Stop {
    net stop MongoDB
}
function Mongdb-Command ([switch]$start, [switch]$stop) {
    If ($stop) {Mongodb-Stop}
    elseif ($start) {Mongodb-Start}
}

# Compiler Functions
function Get-Platform ($version = 'amd64') {
    $amd64 = 'x64'
    $x86 = 'x86'
    If ($version -eq $amd64) {$rtn = $amd64}
    ElseIf ($version -eq 'amd64') {$rtn = $amd64}
    ElseIf ($version -eq '64') {$rtn = $amd64}
    ElseIf ($version -eq 'x86') {$rtn = $x86}
    ElseIf ($version -eq 'win32') {$rtn = $x86}
    ElseIf ($version -eq '32') {$rtn = $x86}
    Else {$rtn = $amd64}
    return $rtn
}
#function Set-VsPy ($version = 'x64', $config = 'release') {
    #$version = Get-Platform $version
    #Invoke-BatchFile C:\Users\Brian\AppData\Local\Programs\Common\Microsoft\"Visual C++ for Python"\9.0\vcvarsall.bat  $version $config
#}
function Set-Vs2008 ($version = 'x64', $config = 'release') {
    $version = Get-Platform $version
    Invoke-BatchFile "${env:VS90COMNTOOLS}..\..\VC\vcvarsall.bat" $version $config
}
function Set-Vs2010 ($version = 'x64', $config = 'release') {
    $version = Get-Platform $version
    & "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /$version /$config
}
function Set-Vs2013 ($version = 'x64', $config = 'release') {
    $version = Get-Platform $version
    Invoke-BatchFile "${env:VS120COMNTOOLS}..\..\VC\vcvarsall.bat" $version $config
}

# Add custom aliases
New-Alias sn New-Symlink
#New-Alias which where.exe
New-Alias git hub
New-Alias pinst Install-Module
New-Alias pup Update-Module
New-Alias ppath Print-Path

New-Alias psql psql.bat
New-Alias pg Postgres-Command
New-Alias mdb Mongodb-Command

#New-Alias vspy Set-VsPy
New-Alias vs2008 Set-Vs2008
New-Alias vs2010 Set-Vs2010
New-Alias vs2013 Set-Vs2013

rm alias:/curl
rm alias:/wget
rm alias:/r

Pop-Location
#Clear-Host
