unit TcpIp;

interface

{$WARN SYMBOL_DEPRECATED OFF}

{ Based on TCP/IP component V0.2                              }
{ Copyright 1997 Andreas Hörstemeier Version 0.22  2001-07-09 }
{ http://www.hoerstemeier.com/tcpip.htm                       }

{$x+}

uses
  Sysutils,
  Classes,
  Winsock,
{$ifdef ver80}
  Winprocs,
  Wintypes;
{$else}
  Windows;
{$endif}

type
  TSocketState = (invalid, valid, connected, state_unknown);
  TTraceLevel = (tt_proto_sent, tt_proto_get, tt_socket);

  ETcpIpError = class(Exception);

  ESocketError = class(ETcpIpError)
    ErrorNumber: Cardinal;
    constructor Create(Number: Cardinal);
  end;

  EProtocolError = class(ETcpIpError)
    ErrorNumber: Word;
    Protocol: string;
    constructor Create(const Proto, Msg: string; number: Word);
  end;

  TTraceProc = procedure(const S: string; Level: TTraceLevel) of object;

  TTcpIp = class(TComponent)
  protected
    FSocket: TSocket;
    FHostname: string;
    FTracer: TTraceProc;
    FSocketNumber: SmallInt;
    IpAddress: LongInt; // Network order!
    FEof: Boolean;
    FStream: TStream;
    FBuffer: Pointer;
    FLoggedIn: Boolean;
    function CreateSocket: TSocket;
    procedure ConnectSocket(var Socket: TSocket; SocketNumber: SmallInt;
      IpAddress: LongInt);
    procedure BindSocket(var Socket: TSocket; OutPortMin, OutPortMax: Word);
    procedure OpenSocketOut(var Socket: TSocket; SocketNumber: SmallInt;
      IpAddress: LongInt); virtual;
    procedure OpenSocketIn(var Socket: TSocket; var SocketNumber: SmallInt;
      IpAddress: LongInt);
    procedure CloseSocket(var Socket: TSocket);
    function AcceptSocketIn(Socket: TSocket; var SockInfo: TSockAddr): TSocket;
    function SocketState(Socket: TSocket): TSocketState;
    function SocketByName(const Service: string): SmallInt;
    function ReadLine(Socket: TSocket): string;
    procedure ReadVar(Socket: TSocket; var Buf; Size: Integer; var Ok: Integer);
    procedure WriteBuf(Socket: TSocket; const Buf; var Size: Integer);
    procedure WriteStr(Socket: TSocket; const S: string);
    procedure SetStream(Value: TStream);
    procedure Action; virtual;
    procedure SendCommand(const S: string); virtual;

  public
    procedure Login; virtual;
    procedure Logout; virtual;
    property OnTrace: TTraceProc read FTracer write FTracer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Eof(Socket: TSocket): Boolean; virtual;
  end;

  THttp = class(TTcpIp)
  protected
    FUrl: string;
    FPath: string;
    FProxy: string;
    FSender: string;
    FReference: string;
    FAgent: string;
    FNoCache: boolean;
    FStatusNr: Integer;
    FStatusTxt: string;
    FSize: Integer;
    FType: string;
    FDoAuthor: TStringList;
    FContentPost: string;
    FRedirect: Boolean;

    procedure GetHead;
    procedure GetBody;
    procedure SendRequest(const Method, Version: string);
    procedure GetAnswer;
    procedure ReportStatusError;

  public
    property Stream: TStream read FStream write SetStream;
    property ContentSize: Integer read FSize;
    property ContentType: string read FType;
    property StatusNumber: Integer read FStatusNr;
    property StatusText: string read FStatusTxt;
    procedure Action; override;
    procedure Post;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AuthorizationRequest: TStringList read FDoAuthor;

  published
    property URL: string read FUrl write FUrl;
    property Proxy: string read FProxy write FProxy;
    property Sender: string read FSender write FSender;
    property Agent: string read FAgent write FAgent;
    property Reference: string read FReference write FReference;
    property NoCache: Boolean read FNoCache write FNoCache;
    property ContentTypePost: string read FContentPost write FContentPost;
    property OnTrace;
  end;

procedure Register;

resourcestring
  SSocketError2 = 'Socket error (%s)';
  STimeout = 'Timeout';
  SUnknownSockError = 'Unknown error';
  SHttpError = 'Received response %d %s from %s';
  SRedirectLimitError = 'Exceeded maximal redirect limie %d';

implementation

const
  BackLog = 2; // possible values 1..5
  BufSize = $7F00; // size of the internal standard buffer
  MaxRedirects = 4;
  INVALID_IP_ADDRESS= -1; // only invalid as a host ip, maybe OK for broadcast ($FFFFFFFF as longint)

function LookupHostname(const Hostname: string): LongInt;
var
  RemoteHost: PHostEnt; // no, don't free it!
  IpAddress: LongInt;
{$ifdef ver80 }
  S: string;
{$else}
{$ifopt h-}
  S: string;
{$endif}
{$endif}
begin
  IpAddress:=INVALID_IP_ADDRESS;
  try
    if Hostname='' then
    begin
      Result := IpAddress;
      Exit;
    end
      else
    begin
{$ifdef ver80}
      S := Hostname + #0;
      // try a xxx.xxx.xxx.xx first
      IpAddress := Winsock.Inet_Addr(PChar(@s[1]));
{$else}
 {$ifopt h-}
      S := Hostname + #0;
      // try a xxx.xxx.xxx.xx first
      IpAddress := Winsock.Inet_Addr(PChar(@s[1]));
 {$else}
      // try a xxx.xxx.xxx.xx first
      IpAddress := Winsock.Inet_Addr(PChar(Hostname));
 {$endif}
{$endif}
      if IpAddress = SOCKET_ERROR then
      begin
{$ifdef ver80}
        RemoteHost := Winsock.GetHostByName(PChar(@s[1]));
{$else}
 {$ifopt h-}
        RemoteHost := Winsock.GetHostByName(PChar(@s[1]));
 {$else}
        RemoteHost := Winsock.GetHostByName(PChar(Hostname));
 {$endif}
{$endif}
        if (RemoteHost = nil) or (RemoteHost^.h_length <= 0) then
        begin
          Result := IpAddress;
          Exit; // host not found
        end
          else
        begin
          IpAddress := LongInt(Pointer(RemoteHost^.h_addr_list^)^);
        end;
        // use the first address given
      end;
    end;
  except
    IpAddress := INVALID_IP_ADDRESS;
  end;
  Result := IpAddress;
end;

function Ip2String(IpAddress: LongInt): string;
begin
  IpAddress := winsock.ntohl(IpAddress);
  Result :=
    IntToStr(IpAddress shr 24)+'.'+
    IntToStr((IpAddress shr 16) and $FF)+'.'+
    IntToStr((IpAddress shr 8) and $FF)+'.'+
    IntToStr(IpAddress and $FF);
end;

// find the count'th occurence of the substring,
// if count<0 then look from the back
function PosN(const s, t: string; Count: Integer): Integer;
var
  i, h, last: Integer;
  u: string;
begin
  u := t;
  if Count > 0 then
  begin
    Result := length(t);
    for i := 1 to Count do
    begin
      h := Pos(s, u);
      if h > 0 then
        u := Copy(u, Pos(s, u) + 1, length(u))
        else
      begin
        u := '';
        Inc(Result);
      end;
    end;
    Result := Result - Length(u);
  end
    else
  if Count < 0 then
  begin
    last := 0;
    for i := Length(t) downto 1 do
    begin
      u := Copy(t, i, Length(t));
      h := Pos(s, u);
      if (h <> 0) and (h + i <> last) then
      begin
        last := h + i - 1;
        Inc(Count);
        if Count = 0 then Break;
      end;
    end;

    if Count = 0 then Result := last
      else Result := 0;
    end
  else
    Result := 0;
end;

constructor EProtocolError.Create(const Proto, Msg: string; Number: Word);
begin
  inherited Create(Msg);
  Protocol := Proto;
  ErrorNumber := Number;
end;

constructor ESocketError.Create(Number: Cardinal);
const
  UnknownSuccessError = $1BD0000;
var
  SysError: string;
begin
  if Number = UnknownSuccessError then SysError := SUnknownSockError
    else SysError := SysErrorMessage(Number);
  inherited Create(Format(SSocketError2, [SysError]));
  ErrorNumber := Number;
end;

// standard syntax of an URL:
// protocol://[user[:password]@]server[:port]/path
procedure ParseUrl(const Url: string; var Proto, User, Pass, Host, Port, Path: string);
var
  p, q: Integer;
  s: string;
begin
  Proto := '';
  User := '';
  Pass := '';
  Host := '';
  Port := '';
  Path := '';

  p := Pos('://', Url);
  if p = 0 then
  begin
    if LowerCase(Copy(Url, 1, 7)) = 'mailto:' then
    begin
      // mailto:// not common
      Proto := 'mailto';
      p := Pos(':', Url);
    end;
  end
    else
  begin
    Proto := Copy(Url, 1, p-1);
    Inc(p,2);
  end;
  s := Copy(Url, p + 1, Length(Url));

  p := Pos('/',s);
  if p = 0 then p := Length(s) + 1;
  Path := Copy(s, p, Length(s));
  s := Copy(s, 1, p-1);

  p := PosN(':', s, -1);
  if p > Length(s) then p:=0;
  q := PosN('@', s, -1);
  if q > Length(s) then q := 0;
  if (p = 0) and (q = 0) then
  begin
    // no user, password or port
    Host := s;
    Exit;
  end
    else
  if q < p then
  begin
    // a port given
    Port := Copy(s, p + 1, Length(s));
    Host := Copy(s, q + 1, p - q - 1);
    if q = 0 then exit; // no user, password
    s := Copy(s, 1, q - 1);
  end
    else
  begin
    Host := Copy(s, q + 1, Length(s));
    s := Copy(s, 1, q - 1);
  end;
  p := Pos(':', s);
  if p = 0 then User := s
    else
  begin
    User := Copy(s, 1, p - 1);
    Pass := Copy(s, p + 1, Length(s));
  end;
end;

  { TTcpIp }

constructor TTcpIp.Create(AOwner: TComponent);
begin
  inherited;

  GetMem(FBuffer, BufSize);
  FStream := TMemorystream.Create;
  FSocket := INVALID_SOCKET;
  IpAddress := INVALID_IP_ADDRESS;
  FLoggedIn := False;
end;

destructor TTcpIp.Destroy;
begin
  FTracer := nil;
  if FBuffer <> nil then
    FreeMem(FBuffer, BufSize);
  FStream.Free;
  if FSocket <> INVALID_SOCKET then
    Logout;
  inherited;
end;

function TTcpIp.CreateSocket: TSocket;
begin
  Result := Winsock.Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
end;

procedure TTcpIp.BindSocket(var Socket: TSocket; OutPortMin, OutPortMax: Word);
var
  LocalAddress: TSockAddr;
  i: Word;
begin
  with LocalAddress do
  begin
    Sin_Family := PF_INET;
    Sin_addr.S_addr := INADDR_ANY;
  end;
  i := OutPortMin;
  while i <= OutPortMax do
  begin
    LocalAddress.Sin_Port := Winsock.htons(i);
    if Winsock.bind(Socket, LocalAddress, SizeOf(LocalAddress)) <>
        SOCKET_ERROR then Break;
    Inc(i);
  end;
end;

procedure TTcpIp.ConnectSocket(var Socket: TSocket; SocketNumber: SmallInt;
  IpAddress: LongInt);
var
  RemoteAddress: TSockAddr;
begin
  with RemoteAddress do
  begin
    Sin_Family := PF_INET;
    Sin_Port := Winsock.htons(SocketNumber);
    Sin_addr := TInAddr(IpAddress);
  end;

  if Winsock.Connect(Socket,RemoteAddress,
       SizeOf(RemoteAddress)) = SOCKET_ERROR then
  begin
    if Winsock.WSAGetLastError <> WSAEWouldBlock then
    begin
      CloseSocket(Socket);
      if Assigned(FTracer) then
      begin
        FTracer('Failed to open output socket '+IntToStr(SocketNumber)+' to host '+
          Ip2String(IpAddress), tt_socket);
      end;
    end
  end
    else
  if Assigned(FTracer) then
  begin
    FTracer('Opened output socket '+IntToStr(SocketNumber)+' to host '+
      Ip2String(IpAddress)+' successfully; ID '+IntToStr(Socket), tt_socket);
  end;
end;

procedure TTcpIp.OpenSocketOut(var Socket: TSocket; SocketNumber:SmallInt;
  IpAddress: LongInt);
begin
  CloseSocket(Socket);
  Socket := CreateSocket;
  ConnectSocket(Socket, SocketNumber, IpAddress);
end;

procedure TTcpIp.OpenSocketIn(var Socket: TSocket; var SocketNumber: SmallInt;
  IpAddress: LongInt);
var
  LocalAddress: TSockAddr;
  l: Integer;
begin
  CloseSocket(Socket);
  Socket := CreateSocket;
  // open the socket and let it listen
  with LocalAddress do
  begin
    Sin_Family := PF_INET;
    Sin_Port := Winsock.htons(SocketNumber);
    Sin_addr := TInAddr(IpAddress);
  end;

  if Winsock.Bind(Socket, LocalAddress, SizeOf(LocalAddress)) = SOCKET_ERROR then
  begin
    if Assigned(FTracer) then
    begin
      FTracer('Failed to bind socket '+IntToStr(SocketNumber)+' for local ip '+
        Ip2String(IpAddress), tt_socket);
    end;
    CloseSocket(Socket);
    Exit;
  end
    else
  if Assigned(FTracer) then
  begin
    FTracer('Bound to socket '+IntToStr(SocketNumber)+' for local ip '+
      Ip2String(IpAddress), tt_socket);
  end;

  l := SizeOf(LocalAddress);
  if Winsock.GetSockName(Socket, LocalAddress, l) <> SOCKET_ERROR then
    SocketNumber := Winsock.ntohs(LocalAddress.Sin_Port);

  if Winsock.Listen(Socket, BackLog) = SOCKET_ERROR then
  begin
    CloseSocket(Socket);
    if Assigned(FTracer) then
    begin
      FTracer('Failed to set input socket ' + IntToStr(SocketNumber) +
        ' to listening mode', tt_socket);
    end
  end
    else
  if Assigned(FTracer) then
  begin
    FTracer('Set input socket ' + IntToStr(SocketNumber) +
      ' to listening mode sucessfully; ID ' + IntToStr(Socket), tt_socket);
  end;
end;

function TTcpIp.AcceptSocketIn(Socket: TSocket; var SockInfo: TSockAddr): TSocket;
var
  x: u_int;
  LocalAddress: TSockAddr;
  TempSocket: TSocket;
begin
  x := SizeOf(LocalAddress);
{$ifdef ver80}
  TempSocket := Winsock.Accept(Socket, LocalAddress, x);
{$else}
{$ifdef ver90}
  TempSocket := Winsock.Accept(Socket, LocalAddress, x);
{$else}       { Delphi 3 and higher }
  TempSocket := Winsock.Accept(Socket, @LocalAddress, @x);
{$endif}
{$endif}
  if TempSocket = SOCKET_ERROR then
  begin
    // no incoming call available
    TempSocket := INVALID_SOCKET;
    if Assigned(FTracer) then
    begin
      FTracer('No incoming connection found on socket ID '+IntToStr(Socket),
        tt_socket);
    end;
  end
    else
  if Assigned(FTracer) then
  begin
    FTracer('Incoming connection found on socket ID '+IntToStr(Socket)+
      '; generated socket ID '+IntToStr(TempSocket), tt_socket);
  end;
  AcceptSocketIn := TempSocket;
  SockInfo := LocalAddress;
end;

function TTcpIp.SocketState(Socket: TSocket): TSocketState;
var
  PeerAdr: TSockAddr;
  x: u_int;
begin
  if Socket = INVALID_SOCKET then Result := invalid
    else
  begin
    x := SizeOf(TSockAddr);
    if Winsock.GetPeerName(Socket, PeerAdr, x) = 0 then
        Result := connected
      else
    begin
      if Winsock.WSAGetLastError <> WSAENOTCONN then
        Result := state_unknown
      else
        Result := valid
    end;
  end;
end;

procedure TTcpIp.CloseSocket(var Socket: TSocket);
begin
  if Socket <> INVALID_SOCKET then
  begin
    Winsock.CloseSocket(Socket);
    if Assigned(FTracer) then
      FTracer('Closed socket ID '+IntToStr(socket), tt_socket);
    Socket := INVALID_SOCKET;
  end;
end;

function TTcpIp.SocketByName(const Service: string): SmallInt;
var
  ServiceEntry: PServEnt;
  s: string;
begin
  s := service + #0;
{$ifdef ver80}
  ServiceEntry := Winsock.GetServByName(pchar(@s[1]), 'tcp');
{$else}
 {$ifopt h-}
  ServiceEntry := Winsock.GetServByName(pchar(@s[1]), 'tcp');
 {$else}
  ServiceEntry := Winsock.GetServByName(pchar(s), 'tcp');
 {$endif}
{$endif}
  if ServiceEntry = nil then
    Result := 0
  else
    Result := Winsock.htons(ServiceEntry^.s_port);
end;

procedure TTcpIp.Login;
begin
  if FLoggedIn then Logout;
  IpAddress := LookupHostname(FHostname);
  if IpAddress = INVALID_IP_ADDRESS then
    raise ETcpIpError.Create('Couldn''t resolve hostname ' + FHostname);
  OpenSocketOut(FSocket, FSocketNumber, IpAddress);
  if FSocket = INVALID_SOCKET then
    raise ESocketError.Create(WSAGetLastError);
  FEof := False;
  FLoggedIn := True;
end;

procedure TTcpIp.LogOut;
begin
  CloseSocket(FSocket);
  FSocket := invalid_socket;
  FLoggedIn := False;
end;

procedure TTcpIp.SendCommand(const S: string);
begin
  WriteStr(FSocket, S + #13#10);
  if Assigned(FTracer) then
    FTracer(S, tt_proto_sent);
end;

function TTcpIp.Eof(Socket: TSocket): Boolean;
begin
  Result := FEof or (SocketState(Socket) <> connected);
end;

procedure TTcpIp.ReadVar(Socket: TSocket; var Buf; Size: Integer;
  var Ok: Integer);
var
  TempBuf: Pointer;
  Error: Integer;
  ReadSet: TFDSet;
  Timeout: TTimeVal;
begin
  TempBuf := nil;
  try
    if @Buf = nil then
      GetMem(TempBuf, Size) // alloc for the -> /dev/null
    else
      TempBuf := @Buf;

    repeat
      FD_ZERO(ReadSet);
      FD_SET(Socket, ReadSet);
      Timeout.tv_sec := 5;
      Timeout.tv_usec := 0;
      Ok := Winsock.Select(1, @ReadSet, nil, nil, @Timeout);
      if Ok = 0 then
        raise ETcpIpError.Create(Format(SSocketError2, [STimeout]))
      else if Ok = SOCKET_ERROR then
        raise ESocketError.Create(WSAGetLastError);
      Ok := Winsock.Recv(Socket, TempBuf^, Size, 0);
      if Ok <= 0 then
      begin
        Error := Winsock.WSAGetLastError;
        FEof := (Error <> WSAEWouldBlock);
      end
        else
      begin
        if Assigned(FTracer) then
          FTracer('Received ' + IntToStr(Ok) + ' bytes on socket ID ' +
            IntToStr(FSocket), tt_socket);
      end;
    until FEof or (Ok > 0);
  finally
    if @Buf = nil then
      FreeMem(TempBuf, Size)
  end;
end;

function TTcpIp.ReadLine(Socket: TSocket): string;
var
  x: Char;
  Ok: Integer;
  s: string;
begin
  s := '';
  repeat
    ReadVar(Socket, x, 1, Ok);
    if Ok <> 1 then
    begin
      Break;
    end
      else
    if x = #13 then // at least NCSA 1.3 does send a #10 only
      else
    if x = #10 then
    begin
      Break;
    end
      else
    begin
      s := s + x;
    end;
  until Eof(Socket);
  Result := s;
end;

procedure TTcpIp.WriteBuf(Socket: TSocket; const Buf; var Size: Integer);
begin
  Size := Winsock.Send(Socket, Pointer(@Buf)^, Size, 0);
  if Assigned(FTracer) then
    FTracer('Sent ' + IntToStr(Size) + ' bytes on socket ID ' +
      IntToStr(FSocket), tt_socket);
end;

procedure TTcpIp.WriteStr(Socket: TSocket; const s: string);
var
  l: Integer;
begin
  l := Length(s);
{$ifdef ver80}
  WriteBuf(Socket, PChar(@s[1])^, l);
{$else}
{$ifopt h-}
  WriteBuf(Socket, PChar(@s[1])^, l);
{$else}
  WriteBuf(Socket, PChar(s)^, l);
{$endif}
{$endif}
end;

procedure TTcpIp.SetStream(Value: TStream);
begin
  TMemoryStream(FStream).LoadFromStream(value);
end;

procedure TTcpIp.Action;
var
  p: Pointer;
  ok, ok2: Integer;
begin
  Login;
  TMemoryStream(FStream).Clear;
  while not Eof(FSocket) do
  begin
    ReadVar(FSocket, FBuffer^, BufSize, ok);
    p := FBuffer;
    while ok > 0 do
    begin
      // just to be sure everything goes into the stream
      ok2 := FStream.write(p^, ok);
      Dec(ok,ok2);
      p := Pointer(LongInt(p) + ok2);
    end;
  end;
  FStream.Seek(0, 0);
end;

  { THttp }

constructor THttp.Create(AOwner: TComponent);
begin
  inherited;

  FContentPost := 'application/x-www-form-urlencoded';
  FDoAuthor := TStringlist.Create;
  FRedirect := False;
end;

destructor THttp.Destroy;
begin
  FDoAuthor.Free;
  inherited;
end;

procedure THttp.SendRequest(const Method, Version: string);
begin
  SendCommand(Method + ' ' + FPath + ' HTTP/' + Version);
  SendCommand('Host: ' + FHostname);
  if FSender <> '' then
    SendCommand('From: ' + FSender);
  if FReference <> '' then
    SendCommand('Referer: ' + FReference);
  if FAgent <> '' then
    SendCommand('User-Agent: ' + FAgent);
  if FNoCache then
    SendCommand('Pragma: no-cache');
  if Method = 'POST' then
  begin
    SendCommand('Content-Length: ' + IntToStr(Stream.Size));
    if FContentPost <> '' then
      SendCommand('Content-Type: ' + FContentPost);
  end;
  WriteStr(FSocket, #13#10); // finalize the request
end;

procedure THttp.ReportStatusError;
begin
  raise EProtocolError.Create('HTTP',
    Format(SHttpError, [FStatusNr, FStatusTxt, FHostName]), FStatusNr);
end;

procedure THttp.GetAnswer;
var
  s: string;
  Proto, User, Pass, Port: string;
  Field, Data: string;
begin
  FDoAuthor.Clear;
  FType := '';
  FSize := 0;
  FRedirect := False;

  repeat
    s := ReadLine(FSocket);
    if s <> '' then
      if Assigned(FTracer) then
        FTracer(s, tt_proto_get);

    // many servers (including ours) obviously return 1.1 response to 1.0 request
    if (Copy(s, 1, 8) = 'HTTP/1.0') or
       (Copy(s, 1, 8) = 'HTTP/1.1') then
    begin
      FStatusNr := StrToInt(Copy(s, 10, 3));
      FStatusTxt := Copy(s, 14, Length(s));
      if FStatusNr >= 400 then ReportStatusError;
    end
      else
    if Pos(':', s) > 0 then
    begin
      Field := LowerCase(Copy(s, 1, Pos(':', s) - 1));
      Data := Copy(s, Pos(':', s) + 2, Length(s));
      if Field = 'location' then
      begin
        if Proxy <> '' then
            FPath := Data // it goes via a proxy, so just change the uri
          else
        begin
          ParseUrl(Data, Proto, User, Pass, FHostname, Port, FPath);
          if Port <> '' then FSocketNumber := StrToInt(Port);
        end;
        FRedirect := True;
      end
        else
      if Field = 'content-length' then
          FSize := StrToInt(Data)
        else
      if Field = 'content-type' then
          FType := Data
        else
      if Field = 'www-authenticate' then
        FDoAuthor.Add(Data);
    end
  until s = '';
end;

procedure THttp.Action;
var
  Proto, User, Pass, Host, Port, Path: string;
  Redirects: Integer;
begin
  // parse url and proxy to FHostname, FPath and FSocketNumber
  if FProxy <> '' then
  begin
    ParseUrl(FUrl, Proto, User, Pass, Host, Port, Path);
    FPath := FUrl;
    if Proto = '' then
      FPath := 'http://' + FPath;
    ParseUrl(FProxy, Proto, User, Pass, Host, Port, Path);
    if Port = '' then Port := '8080';
  end
    else
  begin
    ParseUrl(FUrl, Proto, User, Pass, Host, Port, FPath);
    if Port = '' then Port := '80';
  end;
  if Proto = '' then Proto := 'http';
  if FPath = '' then FPath := '/';

  FHostname := Host;
  FSocketNumber := StrToInt(Port);
  Redirects := 0;
  // loop until we get answer without Location header
  repeat
    Inc(Redirects);
    if Redirects > MaxRedirects then
      raise ETcpIpError.Create(Format(SRedirectLimitError, [MaxRedirects]));
    // do directly GetBody, instead of GetHead first.
    // currently we use this for updates only and the potentional overhead
    // of GetBody on redirect answer is smaller then two requests per each check
    // (especially for the server itself)
    GetBody;
  until not FRedirect;
  if FStatusNr <> 200 then ReportStatusError;
end;

procedure THttp.GetHead;
begin
  Login;
  SendRequest('HEAD', '1.0');
  GetAnswer;
  Logout;
end;

procedure THttp.GetBody;
var
  p: Pointer;
  ok, ok2: Integer;
begin
  Login;
  SendRequest('GET', '1.0');
  GetAnswer;
  // read the data
  TMemoryStream(FStream).Clear;
  while not Eof(FSocket) do
  begin
    ReadVar(FSocket, FBuffer^, BufSize, Ok);
    p := FBuffer;
    while ok > 0 do
    begin
      // just to be sure everything goes into the stream
      ok2 := FStream.Write(p^, ok);
      Dec(ok, ok2);
      p := Pointer(LongInt(p) + ok2);
    end;
  end;
  FStream.Seek(0,0); // set the stream back to start
  Logout;
end;

procedure THttp.Post;
var
  p: Pointer;
  ok, ok2: Integer;
  Proto, User, Pass, Host, Port, Path: string;
begin
  // parse url and proxy to FHostname, FPath and FSocketNumber
  if FProxy <> '' then
  begin
    ParseUrl(FProxy, Proto, User, Pass, Host, Port, Path);
    FPath := FUrl;
    if Port = '' then Port := '8080';
  end
    else
  begin
    ParseUrl(FUrl, Proto, User, Pass, Host, Port, FPath);
    if Port = '' then Port := '80';
  end;
  if Proto = '' then Proto := 'http';
  if Path = '' then Path := '/';

  FHostname := Host;
  FSocketNumber := StrToInt(Port);

  Login;
  SendRequest('POST', '1.0');
  // Send the data
  TMemoryStream(FStream).Seek(0, 0);
  ok := 1;
  while ok > 0 do
  begin
    ok := FStream.Read(FBuffer^, BufSize);
    WriteBuf(FSocket, FBuffer^, ok);
  end;
  GetAnswer;
  // read in the response body
  TMemoryStream(FStream).Clear;
  while not Eof(FSocket) do
  begin
    ReadVar(FSocket, FBuffer^, BufSize, ok);
    p := FBuffer;
    while ok > 0 do
    begin
      // just to be sure everything goes into the stream
      ok2 := FStream.Write(p^, ok);
      Dec(ok, ok2);
      p := Pointer(LongInt(p) + ok2);
    end;
  end;
  FStream.Seek(0,0); // set the stream back to start
  Logout;
end;

procedure Register;
begin
  RegisterComponents('Martin', [THttp]);
end;

end.
