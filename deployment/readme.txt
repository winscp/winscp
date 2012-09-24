To create 'winscpxxxsetup.exe' install package, follow these steps:
1) build 'WinSCP.exe', 'Console.com' and 'DragExt.dll' (see ..\readme)
2) install 'Inno Setup'
   http://www.jrsoftware.org/isinfo.php
3) install 'PuTTY' package to directory 'c:\program files\putty\'
   http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html
   (A Windows-style installer for everything except PuTTYtel)
4) run 'iscc winscpsetup.iss'
4) file 'winscpxxxsetup.exe' is created
