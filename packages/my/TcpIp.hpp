// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TcpIp.pas' rev: 6.00

#ifndef TcpIpHPP
#define TcpIpHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <WinSock.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tcpip
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TSocketState { invalid, valid, connected, state_unknown };
#pragma option pop

#pragma option push -b-
enum TTraceLevel { tt_proto_sent, tt_proto_get, tt_socket };
#pragma option pop

class DELPHICLASS ETcpIpError;
class PASCALIMPLEMENTATION ETcpIpError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall ETcpIpError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ETcpIpError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ETcpIpError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ETcpIpError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ETcpIpError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ETcpIpError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ETcpIpError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ETcpIpError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ETcpIpError(void) { }
	#pragma option pop
	
};


class DELPHICLASS ESocketError;
class PASCALIMPLEMENTATION ESocketError : public ETcpIpError 
{
	typedef ETcpIpError inherited;
	
public:
	Word ErrorNumber;
	__fastcall ESocketError(Word Number);
public:
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall ESocketError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : ETcpIpError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall ESocketError(int Ident)/* overload */ : ETcpIpError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall ESocketError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : ETcpIpError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall ESocketError(const AnsiString Msg, int AHelpContext) : ETcpIpError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall ESocketError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : ETcpIpError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall ESocketError(int Ident, int AHelpContext)/* overload */ : ETcpIpError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall ESocketError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : ETcpIpError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~ESocketError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EProtocolError;
class PASCALIMPLEMENTATION EProtocolError : public ETcpIpError 
{
	typedef ETcpIpError inherited;
	
public:
	Word ErrorNumber;
	AnsiString Protocol;
	__fastcall EProtocolError(const AnsiString Proto, const AnsiString Msg, Word number);
public:
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EProtocolError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : ETcpIpError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EProtocolError(int Ident)/* overload */ : ETcpIpError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EProtocolError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : ETcpIpError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EProtocolError(const AnsiString Msg, int AHelpContext) : ETcpIpError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EProtocolError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : ETcpIpError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EProtocolError(int Ident, int AHelpContext)/* overload */ : ETcpIpError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EProtocolError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : ETcpIpError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EProtocolError(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TTraceProc)(const AnsiString S, TTraceLevel Level);

class DELPHICLASS TTcpIp;
class PASCALIMPLEMENTATION TTcpIp : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	unsigned FHandle;
	int FSocket;
	AnsiString FHostname;
	TTraceProc FTracer;
	short FSocketNumber;
	int IpAddress;
	bool FEof;
	bool FNewData;
	Classes::TStream* FStream;
	void *FBuffer;
	bool FAsync;
	bool FLoggedIn;
	virtual void __fastcall WndProc(Messages::TMessage &Msg);
	int __fastcall CreateSocket(void);
	void __fastcall ConnectSocket(int &Socket, short SocketNumber, int IpAddress);
	void __fastcall BindSocket(int &Socket, Word OutPortMin, Word OutPortMax);
	virtual void __fastcall OpenSocketOut(int &Socket, short SocketNumber, int IpAddress);
	void __fastcall OpenSocketIn(int &Socket, short &SocketNumber, int IpAddress);
	void __fastcall CloseSocket(int &Socket);
	int __fastcall AcceptSocketIn(int Socket, sockaddr_in &SockInfo);
	TSocketState __fastcall SocketState(int Socket);
	short __fastcall SocketByName(const AnsiString Service);
	AnsiString __fastcall ReadLine(int Socket);
	void __fastcall ReadVar(int Socket, void *Buf, int Size, int &Ok);
	void __fastcall WriteBuf(int Socket, const void *Buf, int &Size);
	void __fastcall WriteStr(int Socket, const AnsiString S);
	void __fastcall SetStream(Classes::TStream* Value);
	virtual void __fastcall Action(void);
	virtual void __fastcall SendCommand(const AnsiString S);
	
public:
	virtual void __fastcall Login(void);
	virtual void __fastcall Logout(void);
	__property TTraceProc OnTrace = {read=FTracer, write=FTracer};
	__fastcall virtual TTcpIp(Classes::TComponent* AOwner);
	__fastcall virtual ~TTcpIp(void);
	virtual bool __fastcall Eof(int Socket);
};


class DELPHICLASS THttp;
class PASCALIMPLEMENTATION THttp : public TTcpIp 
{
	typedef TTcpIp inherited;
	
protected:
	AnsiString FUrl;
	AnsiString FPath;
	AnsiString FProxy;
	AnsiString FSender;
	AnsiString FReference;
	AnsiString FAgent;
	bool FNoCache;
	int FStatusNr;
	AnsiString FStatusTxt;
	int FSize;
	AnsiString FType;
	Classes::TStringList* FDoAuthor;
	AnsiString FContentPost;
	void __fastcall GetHead(void);
	void __fastcall GetBody(void);
	void __fastcall SendRequest(const AnsiString Method, const AnsiString Version);
	void __fastcall GetAnswer(void);
	
public:
	__property Classes::TStream* Stream = {read=FStream, write=SetStream};
	__property int ContentSize = {read=FSize, nodefault};
	__property AnsiString ContentType = {read=FType};
	__property int StatusNumber = {read=FStatusNr, nodefault};
	__property AnsiString StatusText = {read=FStatusTxt};
	virtual void __fastcall Action(void);
	void __fastcall Post(void);
	__fastcall virtual THttp(Classes::TComponent* AOwner);
	__fastcall virtual ~THttp(void);
	__property Classes::TStringList* AuthorizationRequest = {read=FDoAuthor};
	
__published:
	__property AnsiString URL = {read=FUrl, write=FUrl};
	__property AnsiString Proxy = {read=FProxy, write=FProxy};
	__property AnsiString Sender = {read=FSender, write=FSender};
	__property AnsiString Agent = {read=FAgent, write=FAgent};
	__property AnsiString Reference = {read=FReference, write=FReference};
	__property bool NoCache = {read=FNoCache, write=FNoCache, nodefault};
	__property AnsiString ContentTypePost = {read=FContentPost, write=FContentPost};
	__property OnTrace ;
};


//-- var, const, procedure ---------------------------------------------------
static const Word WM_SocketEvent = 0x500;
extern PACKAGE System::ResourceString _SSocketError;
#define Tcpip_SSocketError System::LoadResourceString(&Tcpip::_SSocketError)
extern PACKAGE System::ResourceString _SUnknownSockError;
#define Tcpip_SUnknownSockError System::LoadResourceString(&Tcpip::_SUnknownSockError)
extern PACKAGE void __fastcall Register(void);

}	/* namespace Tcpip */
using namespace Tcpip;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TcpIp
