To create 'WinSCP-x.x.x-Setup.exe' install package, follow these steps:
- Build 'WinSCP.exe', 'Console.com', 'DragExt.dll' and 'DragExt64.dll' (see ..\readme)
- Install 'Inno Setup'
  https://jrsoftware.org/isinfo.php
- Install 'PuTTY' package
  https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html
- Create 'translations' subfolder
- Copy the 'Default.isl' from Inno Setup installation to 'translations\WinSCP.en.islu'
- Append the 'winscp.isl' to the 'translations\WinSCP.en.islu'
- Run 'iscc winscpsetup.iss'
- File 'WinSCP-x.x.x-Setup.exe' is created
