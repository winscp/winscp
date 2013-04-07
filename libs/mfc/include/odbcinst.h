/*--------------------------------------------------------------------------
  ODBCInst.h -- Prototypes for ODBCINST.DLL

  (c) Microsoft Corp., 1990-1995
--------------------------------------------------------------------------*/

#ifndef __ODBCINST_H
#define __ODBCINST_H

#ifdef __cplusplus
extern "C" {                               // Assume C declarations for C++
#endif	// __cplusplus

#ifndef ODBCVER
#define ODBCVER 0x0250					   // Assume ODBC 2.50
#endif

#ifndef WINVER
#define  WINVER  0x300                     // Assume Windows 3.0
#endif

#if (WINVER < 0x30a)
// Win 3.1 Types -----------------------------------------------------------
typedef const char FAR*   LPCSTR;
#endif


// Constants ---------------------------------------------------------------
// SQLConfigDataSource request flags
#define  ODBC_ADD_DSN     1               // Add data source
#define  ODBC_CONFIG_DSN  2               // Configure (edit) data source
#define  ODBC_REMOVE_DSN  3               // Remove data source

#if (ODBCVER >= 0x0250)
#define  ODBC_ADD_SYS_DSN 4				  // add a system DSN
#define  ODBC_CONFIG_SYS_DSN	5		  // Configure a system DSN
#define  ODBC_REMOVE_SYS_DSN	6		  // remove a system DSN

// install request flags
#define	 ODBC_INSTALL_INQUIRY	1		
#define  ODBC_INSTALL_COMPLETE	2

// config driver flags
#define  ODBC_INSTALL_DRIVER	1
#define  ODBC_REMOVE_DRIVER		2
#define  ODBC_CONFIG_DRIVER_MAX 100
#endif

#ifndef EXPORT
#define EXPORT _export
#endif

#ifndef RC_INVOKED
// Prototypes --------------------------------------------------------------
#ifdef WIN32
#define INSTAPI __stdcall
#else
#define INSTAPI EXPORT FAR PASCAL
#endif

#ifndef WIN32
#define SQLGetPrivateProfileString   GetPrivateProfileString
#define SQLWritePrivateProfileString WritePrivateProfileString
#endif

// High level APIs
BOOL INSTAPI SQLInstallODBC          (HWND       hwndParent,
                                      LPCSTR     lpszInfFile,
									  LPCSTR     lpszSrcPath,
									  LPCSTR     lpszDrivers);
BOOL INSTAPI SQLManageDataSources    (HWND       hwndParent);
BOOL INSTAPI SQLCreateDataSource     (HWND       hwndParent,
                                      LPCSTR     lpszDSN);
BOOL INSTAPI SQLGetTranslator        (HWND       hwnd,
									   LPSTR      lpszName,
									   WORD       cbNameMax,
									   WORD  FAR *pcbNameOut,
									   LPSTR      lpszPath,
									   WORD       cbPathMax,
									   WORD  FAR *pcbPathOut,
									   DWORD FAR *pvOption);

// Low level APIs
// NOTE: The high-level APIs should always be used. These APIs
//       have been left for compatibility.
BOOL INSTAPI SQLInstallDriver        (LPCSTR     lpszInfFile,
                                      LPCSTR     lpszDriver,
                                      LPSTR      lpszPath,
                                      WORD       cbPathMax,
                                      WORD FAR * pcbPathOut);
BOOL INSTAPI SQLInstallDriverManager (LPSTR      lpszPath,
                                      WORD       cbPathMax,
                                      WORD FAR * pcbPathOut);
BOOL INSTAPI SQLGetInstalledDrivers  (LPSTR      lpszBuf,
                                      WORD       cbBufMax,
                                      WORD FAR * pcbBufOut);
BOOL INSTAPI SQLGetAvailableDrivers  (LPCSTR     lpszInfFile,
                                      LPSTR      lpszBuf,
                                      WORD       cbBufMax,
                                      WORD FAR * pcbBufOut);
BOOL INSTAPI SQLConfigDataSource     (HWND       hwndParent,
                                      WORD       fRequest,
                                      LPCSTR     lpszDriver,
                                      LPCSTR     lpszAttributes);
BOOL INSTAPI SQLRemoveDefaultDataSource(void);
BOOL INSTAPI SQLWriteDSNToIni        (LPCSTR     lpszDSN,
                                      LPCSTR     lpszDriver);
BOOL INSTAPI SQLRemoveDSNFromIni     (LPCSTR     lpszDSN);
BOOL INSTAPI SQLValidDSN             (LPCSTR     lpszDSN);

#ifdef WIN32
BOOL INSTAPI SQLWritePrivateProfileString(LPCSTR lpszSection,
										 LPCSTR lpszEntry,
										 LPCSTR lpszString,
										 LPCSTR lpszFilename);

int  INSTAPI SQLGetPrivateProfileString( LPCSTR lpszSection,
										LPCSTR lpszEntry,
										LPCSTR lpszDefault,
										LPSTR  lpszRetBuffer,
										int    cbRetBuffer,
										LPCSTR lpszFilename);
#endif

#if (ODBCVER >= 0x0250)
BOOL INSTAPI SQLRemoveDriverManager(LPDWORD lpdwUsageCount);
BOOL INSTAPI SQLInstallTranslator(LPCSTR lpszInfFile,
								  LPCSTR lpszTranslator,
								  LPCSTR lpszPathIn,
								  LPSTR  lpszPathOut,
								  WORD   cbPathOutMax,
								  WORD FAR *pcbPathOut,
								  WORD	 fRequest,
								  LPDWORD	lpdwUsageCount);
BOOL INSTAPI SQLRemoveTranslator(LPCSTR lpszTranslator, 
								 LPDWORD lpdwUsageCount);
BOOL INSTAPI SQLRemoveDriver(LPCSTR lpszDriver, 
							 BOOL fRemoveDSN, 
							 LPDWORD lpdwUsageCount);
BOOL INSTAPI SQLConfigDriver(HWND hwndParent, 
							 WORD fRequest, 
							 LPCSTR lpszDriver,
							 LPCSTR lpszArgs,
							 LPSTR  lpszMsg,
							 WORD   cbMsgMax,
                             WORD FAR *pcbMsgOut);
#endif

//	Driver specific Setup APIs called by installer

BOOL INSTAPI ConfigDSN (HWND	hwndParent,
						WORD	fRequest,
						LPCSTR	lpszDriver,
						LPCSTR	lpszAttributes);

BOOL INSTAPI ConfigTranslator (	HWND		hwndParent,
								DWORD FAR  *pvOption);

#if (ODBCVER >= 0x0250)
BOOL INSTAPI ConfigDriver(HWND hwndParent, 
						  WORD fRequest, 
                          LPCSTR lpszDriver,
				          LPCSTR lpszArgs, 
                          LPSTR  lpszMsg, 
                          WORD   cbMsgMax, 
                          WORD FAR *pcbMsgOut);
#endif

#endif // RC_INVOKED

#ifdef __cplusplus
}                                    // End of extern "C" {
#endif	// __cplusplus

#endif // __ODBCINST_H
