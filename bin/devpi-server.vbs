Set WshShell = CreateObject("WScript.Shell")
return = WshShell.Run("cmd.exe /c c:\users\brian\.virtualenvs\devpi\scripts\devpi-server.exe --start", 0, true)
Set WshShell = Nothing
