@echo off
set WINSCP_INI=%U3_APP_DATA_PATH%\winscp.ini
set WINSCP3_INI=%U3_APP_DATA_PATH%\winscp3.ini

set VER_403=%U3_APP_DATA_PATH%\ver.403
set VER_406=%U3_APP_DATA_PATH%\ver.406

echo. > "%VER_406%"

if not "%U3_IS_UPGRADE%" == "true" goto NO_UPGRADE

rem 403
if not exist "%WINSCP3_INI%" goto NO_WINSCP3_INI

copy "%WINSCP_INI%" + "%WINSCP3_INI%" "%WINSCP_INI%"
del "%WINSCP3_INI%"

:NO_WINSCP3_INI

rem 406
echo [Override\Configuration\Interface] >> "%WINSCP_INI%"
echo PuttyRegistryStorageKey=Software%%5CPuTTY%%20for%%20U3-%U3_DEVICE_VENDOR_ID%-%U3_DEVICE_SERIAL% >> "%WINSCP_INI%"
echo PuttyPath=%%25U3_HOST_EXEC_PATH%%25%%5C..%%5C..%%5CD7A496BD-33D0-448C-A2A8-6F9FF3130636%%5CExec%%5Cputty.exe >> "%WINSCP_INI%"

:NO_UPGRADE
