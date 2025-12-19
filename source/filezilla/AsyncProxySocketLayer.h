/*CAsyncProxySocketLayer by Tim Kosse (Tim.Kosse@gmx.de)
                 Version 1.6 (2003-03-26)
--------------------------------------------------------

Introduction:
-------------

This class is layer class for CAsyncSocketEx. With this class you
can connect through SOCKS4/5 and HTTP 1.1 proxies. This class works
as semi-transparent layer between CAsyncSocketEx and the actual socket.
This class is used in FileZilla, a powerful open-source FTP client.
It can be found under https://sourceforge.net/projects/filezilla/
For more information about HTTP 1.1 goto https://datatracker.ietf.org/doc/html/rfc2616

How to use?
-----------

You don't have to change much in you already existing code to use
CAsyncProxySocketLayer.
To use it, create an instance of CAsyncProxySocketLayer, call SetProxy
and attach it to a CAsyncSocketEx instance.
You have to process OnLayerCallback in you CAsyncSocketEx instance as it will
receive all layer nofications.
The following notifications are sent:

//Error codes
PROXYERROR_NOERROR 0
PROXYERROR_NOCONN 1 //Can't connect to proxy server, use GetLastError for more information
PROXYERROR_REQUESTFAILED 2 //Request failed, can't send data
PROXYERROR_AUTHREQUIRED 3 //Authentication required
PROXYERROR_AUTHTYPEUNKNOWN 4 //Authtype unknown or not supported
PROXYERROR_AUTHFAILED 5  //Authentication failed
PROXYERROR_AUTHNOLOGON 6
PROXYERROR_CANTRESOLVEHOST 7

//Status messages
PROXYSTATUS_LISTENSOCKETCREATED 8 //Called when a listen socket was created successfully. Unlike the normal listen function,
                //a socksified socket has to connect to the proxy to negotiate the details with the server
                //on which the listen socket will be created
                //The two parameters will contain the ip and port of the listen socket on the server.

Description of important functions and their parameters:
--------------------------------------------------------

void SetProxy(int nProxyType, const char * ProxyHost, int nProxyPort);
void SetProxy(int nProxyType, const char *, int nProxyPort, const char * ProxyUser, const char * ProxyPass);

Call one of this functions to set the proxy type.
Parametes:
- nProxyType specifies the Proxy Type.
- ProxyHost and nProxyPort specify the address of the proxy
- ProxyUser and ProxyPass are only available for SOCKS5 proxies.

supported proxy types:
PROXYTYPE_NOPROXY
PROXYTYPE_SOCKS4
PROXYTYPE_SOCKS4A
PROXYTYPE_SOCKS5
PROXYTYPE_HTTP11

There are also some other functions:

GetProxyPeerName
Like GetPeerName of CAsyncSocket, but returns the address of the
server connected through the proxy.  If using proxies, GetPeerName
only returns the address of the proxy.

int GetProxyType();
Returns the used proxy

const int GetLastProxyError() const;
Returns the last proxy error

License
-------

Feel free to use this class, as long as you don't claim that you wrote it
and this copyright notice stays intact in the source files.
If you use this class in commercial applications, please send a short message
to tim.kosse@gmx.de
*/
//---------------------------------------------------------------------------
#ifndef AsyncProxySocketLayerH
#define AsyncProxySocketLayerH
//---------------------------------------------------------------------------
#include "AsyncSocketExLayer.h"
//---------------------------------------------------------------------------
class CAsyncProxySocketLayer : public CAsyncSocketExLayer
{
public:

public:
  CAsyncProxySocketLayer();
  virtual ~CAsyncProxySocketLayer();

public:
  virtual void Close();
  virtual BOOL Connect(const wchar_t * lpHostAddress, UINT nHostPort);
  virtual BOOL Connect(const SOCKADDR * lpSockAddr, int nSockAddrLen);
  virtual BOOL Listen(int nConnectionBacklog);

  // Sets the proxy details.
  // nProxyType - Type of the proxy. May be PROXYTYPE_NONE, PROXYTYPE_SOCKS4, PROXYTYPE_SOCKS5 or PROXYTYPE_HTTP11
  // ProxyHost - The address of the proxy. Can be either IP or URL
  // ProxyPort - The port of the proxy
  // ProxyUser - the username for SOCKS5 proxies
  // ProxyPass - the password for SOCKS5 proxies
  void SetProxy(int nProxyType, const char * pProxyHost, int ProxyPort, bool bUseLogon, const char * pProxyUser, const char * pProxyPass);

  // Returns the address of the server behind the SOCKS proxy you are connected to
  virtual BOOL GetPeerName(CString & rPeerAddress, UINT & rPeerPort);
  virtual BOOL GetPeerName(SOCKADDR * lpSockAddr, int * lpSockAddrLen);

protected:
  virtual BOOL Accept(CAsyncSocketEx & rConnectedSocket, SOCKADDR * lpSockAddr = NULL, int * lpSockAddrLen = NULL);
  virtual void OnReceive(int nErrorCode);
  virtual void OnConnect(int nErrorCode);
  virtual int Send(const void * lpBuf, int nBufLen, int nFlags = 0);
  virtual int Receive(void * lpBuf, int nBufLen, int nFlags = 0);

private:
  void Reset();
  void ClearBuffer();    // Clears the receive buffer
  void ConnectionEstablished();
  void ConnectionFailed(int nErrorCode, char * Str = NULL);
  char *m_pRecvBuffer;  // The receive buffer
  int m_nRecvBufferLen;  // Length of the RecvBuffer
  int m_nRecvBufferPos;  // Position within the receive buffer
  char *m_pStrBuffer;    // Recvbuffer needed by HTTP1.1 proxy
  int m_nProxyOpState;  // State of an operation
  int m_nProxyOpID;    // Currently active operation (0 if none)
  unsigned short m_nProxyPeerPort;  // Port of the server you are connected to, retrieve via GetPeerName
  ULONG m_nProxyPeerIp;  // IP of the server you are connected to, retrieve via GetPeerName
  typedef struct
  {
    int nProxyType;
    char * pProxyHost;
    int nProxyPort;
    char * pProxyUser;
    char * pProxyPass;
    BOOL bUseLogon;
  } t_proxydata; // This structure will be used to hold the proxy details

  t_proxydata m_ProxyData; // Structure to hold the data set by SetProxy
  char * m_pProxyPeerHost; // The host connected to
};
//---------------------------------------------------------------------------
// Errorcodes
#define PROXYERROR_NOERROR 0
#define PROXYERROR_NOCONN 1 //Can't connect to proxy server, use GetLastError for more information
#define PROXYERROR_REQUESTFAILED 2 //Request failed, can't send data
#define PROXYERROR_AUTHREQUIRED 3 //Authentication required
#define PROXYERROR_AUTHTYPEUNKNOWN 4 //Authtype unknown or not supported
#define PROXYERROR_AUTHFAILED 5  //Authentication failed
#define PROXYERROR_AUTHNOLOGON 6
#define PROXYERROR_CANTRESOLVEHOST 7
//---------------------------------------------------------------------------
// Status messages
// Called when a listen socket was created successfully. Unlike the normal listen function,
// a socksified socket has to connect to the proxy to negotiate the details with the server
// on which the listen socket will be created
// The two parameters will contain the ip and port of the listen socket on the server.
#define PROXYSTATUS_LISTENSOCKETCREATED 8
struct t_ListenSocketCreatedStruct
{
  unsigned long ip;
  UINT nPort;
};
//---------------------------------------------------------------------------
// Proxytypes
#define PROXYTYPE_NOPROXY 0
#define PROXYTYPE_SOCKS4 1
#define PROXYTYPE_SOCKS4A 2
#define PROXYTYPE_SOCKS5 3
#define PROXYTYPE_HTTP11 4
//---------------------------------------------------------------------------
#define PROXYOP_CONNECT 1
#define PROXYOP_LISTEN 2
//---------------------------------------------------------------------------
#endif // AsyncProxySocketLayerH
