To create 'winscpxxxsetup.exe' install package, follow these steps:
- Build 'WinSCP.exe', 'Console.com' and 'DragExt.dll' (see ..\readme)
- Install 'Inno Setup'
  http://www.jrsoftware.org/isinfo.php
- Install 'PuTTY' package to directory 'c:\program files\putty\'
  http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html
  (A Windows-style installer for everything except PuTTYtel)
- Run 'iscc winscpsetup.iss'
- File 'winscpxxxsetup.exe' is created
