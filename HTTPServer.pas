unit HTTPServer;
{******************************************************************************}
{*  HTTP Server Unit                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
{ utils }
    DateUtils, Utils, Strings, VarRecs, Versions, EClasses,
    DllThreads,
{ synapse }
    WinSock, BlckSock, SynaUtil;

{$I 'HTTP.inc'}

type
{ ошибка сокета }
{$M+}
    ESocketError = class (Exception) end;
{$M-}

{ ошибка http-сервера }
{$M+}
    EHTTPServerError = class (EThreadError) end;
{$M-}

{ http-сервер }
{$M+}
    CHTTPServer = class of THTTPServer;
    PHTTPServer = ^THTTPServer;
    THTTPServer = class (TDllThread)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); override;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); override;
    private
        f_Threads: TDllThreads;  { дочерние потоки }
        f_Sock: TTCPBlockSocket; { сокет }
        f_Port: WORD;            { порт }
        f_TimeOut: LongWord;     { время ожидания ms }
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Execute; override;
        procedure Main; override;
        procedure Return; override;
    public
        property Threads: TDllThreads read f_Threads;
        property Sock: TTCPBlockSocket read f_Sock;
        property Port: WORD read f_Port write f_Port;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
    end;
{$M-}

{ дочерний поток http-сервера }
{$M+}
    CHTTPServerThread = class of THTTPServerThread;
    PHTTPServerThread = ^THTTPServerThread;
    THTTPServerThread = class (TDllThread)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); override;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); override;
    private
        f_Sock: TTCPBlockSocket;   { сокет }
        f_HSock: TSocket;          { дискриптор сокета }
        f_TimeOut: LongWord;       { время ожидания ms }
        f_Headers: TStringList;    { заголовок }
        f_InputData: TStream;      { входящий поток }
        f_OutputData: TStream;     { исходящий поток }
        f_Request: String;         { запрос }
        f_RequestSize: Int64;      { размер запроса }
        f_Protocol: String;        { протокол }
        f_ProtocolVersion: String; { версия протокола }
        f_Method: String;          { метод }
        f_URI: String;             { адрес }
        f_Host: String;            { хост в заголовке }
        f_UserAgent: String;       { браузер }
        f_ResultCode: WORD;        { код ответа }
        f_Closed : Boolean;        { признак закрытия сессии }
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
        procedure Return; override;
        function Process : WORD; virtual;
    public
        property Sock: TTCPBlockSocket read f_Sock;
        property HSock: TSocket read f_HSock;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
        property Headers: TStringList read f_Headers write f_Headers;
        property InputData: TStream read f_InputData;
        property OutputData: TStream read f_OutputData;
        property Request: String read f_Request write f_Request;
        property RequestSize: Int64 read f_RequestSize write f_RequestSize;
        property Protocol: String read f_Protocol write f_Protocol;
        property ProtocolVersion: String read f_ProtocolVersion write f_ProtocolVersion;
        property Method: String read f_Method write f_Method;
        property URI: String read f_URI write f_URI;
        property Host: String read f_Host write f_Host;
        property UserAgent: String read f_UserAgent write f_UserAgent;
        property ResultCode: WORD read f_ResultCode write f_ResultCode;
        property Closed : Boolean read f_Closed write f_Closed;
    end;
{$M-}

{ THTTPServer Errors }
resourcestring
    ERR_THTTPSERVER_CREATE          = 'Ошибка создания потока веб-сервера!';
    ERR_THTTPSERVER_DESTROY         = 'Ошибка уничтожения потока веб-сервера!';
    ERR_THTTPSERVER_EXECUTE         = 'Ошибка выполнения потока веб-сервера!';
    ERR_THTTPSERVER_MAIN            = 'Ошибка главной функции потока!';
    ERR_THTTPSERVER_RETURN          = 'Ошибка функции возврата потока!';
    ERR_THTTPSERVER_INVALID_PORT    = 'Некорректный порт!';
    ERR_THTTPSERVER_INVALID_TIMEOUT = 'Некорректное время ожидания!';

{ THTTPServerThread Errors }
resourcestring
    ERR_THTTPSERVERTHREAD_CREATE            = 'Ошибка создания дочернего потока веб-сервера!';
    ERR_THTTPSERVERTHREAD_DESTROY           = 'Ошибка уничтожения дочернего потока веб-сервера!';
    ERR_THTTPSERVERTHREAD_MAIN              = 'Ошибка главной функции потока!';
    ERR_THTTPSERVERTHREAD_RETURN            = 'Ошибка функции возврата потока!';
    ERR_THTTPSERVERTHREAD_PROCESS           = 'Ошибка обработки запроса!';
    ERR_THTTPSERVERTHREAD_INVALID_SOCKET_ID = 'Некорректный дискриптор сокета!';
    ERR_THTTPSERVERTHREAD_INVALID_TIMEOUT   = 'Некорректное время ожидания!';
    ERR_THTTPSERVERTHREAD_SOCKET_ERROR      = 'Ошибка сокета %d : %s';
    ERR_THTTPSERVERTHREAD_IVALID_REQUEST    = 'Некорректный запрос!';
    ERR_THTTPSERVERTHREAD_IVALID_PROTOCOL   = 'Некорректный протокол!';
    ERR_THTTPSERVERTHREAD_IVALID_METHOD     = 'Некорректный метод!';
    ERR_THTTPSERVERTHREAD_IVALID_URI        = 'Некорректный URI!';

implementation

{ THTTPServer }
class procedure THTTPServer._raise (anArgs: array of const;
                                    const anEGUID: String = '');
begin
    raise EHTTPServerError.Create ( _([self],anArgs), anEGUID );
end;

class procedure THTTPServer._raise (anArgs: array of const;
                                    anEGUID: array of const);
begin
    raise EHTTPServerError.Create ( _([self],anArgs), anEGUID );
end;

constructor THTTPServer.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
begin
    try
        { передаем параметры создания базового потока,
          начиная с третьего аргумента }
        if ( High (anArgs) >= 2 ) then
        begin
            SetLength ( Args, High (anArgs)-2 +1 );
            for I := 2 to High (anArgs) do
                args [I-2] := anArgs [I];
        end
        else
            args := _array_of_const ([]);
        inherited Create (args);
        { явно указываем на необходимость уничтожения потока по окончанию работы }
        FreeOnTerminate := TRUE;
        { первый параметр - порт }
        f_Port := 80;
        if notEmpty (0,anArgs) then
        begin
            f_Port := toInteger (anArgs [0]);
        end;
        if ( Port <= 0 ) then
            raise Exception.Create (ERR_THTTPSERVER_INVALID_PORT);
        { второй параметр - время ожидания }
        f_TimeOut := 120000;
        if notEmpty (1,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [1]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_THTTPSERVER_INVALID_TIMEOUT);
        { создаем сокет }
        f_Sock := TTCPBlockSocket.Create;
        { создаем контейнер дочерних потоков }
        f_Threads := TDllThreads.Create ([]);
    except on E: Exception do
        _raise ([ 'Create', ERR_THTTPSERVER_CREATE, E, Exception (FatalException) ],
                ['{5FE107FB-D71C-409E-9A75-22C9A1954E81}']);
    end;
end;

destructor THTTPServer.Destroy;
begin
    try
        try
            if Assigned (Threads) then
                Threads.Terminate;
            FreeAndNil (f_Threads);
            if Assigned (Sock) then
                Sock.CloseSocket;
            FreeAndNil (f_Sock);
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_THTTPSERVER_DESTROY, E, Exception (FatalException) ],
                ['{A628DCFC-4110-4ECE-875B-986E3048F208}']);
    end;
end;

procedure THTTPServer.Execute;
begin
    try
        if Terminated then Exit;
        { слушаем все ip-адреса на выбранном порту }
        with Sock do
        begin
            CreateSocket;
            SetLinger (TRUE,10000);
            Bind ( cAnyHost, IntToStr (Port) );
            Listen;
            inherited Execute;
        end;
    except on E: Exception do
        _raise ([ 'Execute', ERR_THTTPSERVER_EXECUTE, E, Exception (FatalException) ],
                ['{439435B0-C284-47B0-85F9-448A36852261}']);
    end;
end;

procedure THTTPServer.Main;
var
    ClientSock : TSocket;
    Thr        : THTTPServerThread;
begin
    try
        if Terminated then Exit;
        inherited Main;
        with Sock do
        begin
            if CanRead (TimeOut) then
            begin
                ClientSock := Accept;
                if ( LastError = 0 ) and ( ClientSock > 0 ) then
                try
                    Thr := THTTPServerThread.Create ([ClientSock,TimeOut]);
                    if ( Threads.Add (Thr) < 0 ) then
                        FreeAndNil (Thr);
                except
                    FreeAndNil (Thr);
                end;
            end;
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_THTTPSERVER_MAIN, E, Exception (FatalException) ],
                ['{BBAF326A-FDFB-4C53-A177-6F63584BB8C3}']);
    end;
end;

procedure THTTPServer.Return;
begin
    try
        if Terminated then Exit;
        inherited Return;
    except on E: Exception do
        _raise ([ 'Return', ERR_THTTPSERVER_RETURN, E, Exception (FatalException) ],
                ['{DE336E65-E848-4F07-8FC3-6D25BCFBD2AD}']);
    end;
end;

{ THTTPServerThread }
class procedure THTTPServerThread._raise (anArgs: array of const;
                                          const anEGUID: String = '');
begin
    raise EHTTPServerError.Create ( _([self],anArgs), anEGUID );
end;

class procedure THTTPServerThread._raise (anArgs: array of const;
                                          anEGUID: array of const);
begin
    raise EHTTPServerError.Create ( _([self],anArgs), anEGUID );
end;

constructor THTTPServerThread.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
begin
    try
        { передаем параметры создания базового потока,
          начиная с третьего аргумента }
        if ( High (anArgs) >= 2 ) then
        begin
            SetLength ( Args, High (anArgs)-2 +1 );
            for I := 2 to High (anArgs) do
                args [I-2] := anArgs [I];
        end
        else
            args := _array_of_const ([]);
        inherited Create (args);
        { явно указываем на необходимость уничтожения потока по окончанию работы }
        FreeOnTerminate := TRUE;
        { первый параметр - идентификатор сокета }
        f_HSock := 0;
        if notEmpty (0,anArgs) then
        begin
            f_HSock := toInteger (anArgs [0]);
        end;
        if ( HSock <= 0 ) then
            raise Exception.Create (ERR_THTTPSERVERTHREAD_INVALID_SOCKET_ID);
        { второй параметр - время ожидания }
        f_TimeOut := 120000;
        if notEmpty (1,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [1]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_THTTPSERVERTHREAD_INVALID_TIMEOUT);
        { создаем сокет }
        f_Sock := TTCPBlockSocket.Create;
        Sock.Socket := HSock;
        { создаем список заголовков }
        f_Headers := TStringList.Create;
        { создаем потоки данных }
        f_InputData := TMemoryStream.Create;
        f_OutputData := TMemoryStream.Create;
    except on E: Exception do
        _raise ([ 'Create', ERR_THTTPSERVERTHREAD_CREATE, E, Exception (FatalException) ],
                ['{D064B326-0F5A-43F6-BAB2-F08198F63459}']);
    end;
end;

destructor THTTPServerThread.Destroy;
begin
    try
        _FillChar ( f_Request, Length (f_Request), $00 );
        _FillChar ( f_Protocol, Length (f_Protocol), $00 );
        _FillChar ( f_ProtocolVersion, Length (f_ProtocolVersion), $00 );
        _FillChar ( f_Method, Length (f_Method), $00 );
        _FillChar ( f_URI, Length (f_URI), $00 );
        _FillChar ( f_Host, Length (f_Host), $00 );
        _FillChar ( f_UserAgent, Length (f_UserAgent), $00 );
        if Assigned (Sock) then
            Sock.CloseSocket;
        FreeAndNil (f_Sock);
        FreeAndNil (f_Headers);
        FreeAndNil (f_InputData);
        FreeAndNil (f_OutputData);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_THTTPSERVERTHREAD_DESTROY, E, Exception (FatalException) ],
                ['{0AF99A00-B2A4-4713-9954-B94E7D4AC1D9}']);
    end;
end;

procedure THTTPServerThread.Main;
var
    sz : LongWord;
    S  : String;
begin
    try
        if Terminated then Exit;
        inherited Main;
        with Sock do
        begin
            { читаем протокол HTTP-запроса }
            Request := RecvString (TimeOut);
            if ( LastError <> 0 ) then
                raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
            if isEmpty (Request) then
                raise Exception.Create (ERR_THTTPSERVERTHREAD_IVALID_REQUEST);
            S := Request;
            try
                Method := Fetch (S,' ');
                if isEmpty (Method) then
                    raise Exception.Create (ERR_THTTPSERVERTHREAD_IVALID_METHOD);
                URI := Fetch (S,' ');
                if isEmpty (URI) then
                    raise Exception.Create (ERR_THTTPSERVERTHREAD_IVALID_URI);
                Protocol := Fetch (S,' ');
                if isEmpty (Protocol) or ( Pos ('HTTP/',Protocol) <> 1 ) then
                    raise Exception.Create (ERR_THTTPSERVERTHREAD_IVALID_PROTOCOL);
                ProtocolVersion := Copy ( Protocol, Length ('HTTP/')+1, Length (Protocol) );
            finally
                _FillChar ( S, Length (S), $00 );
            end;
            Headers.Clear;
            Host := '';
            UserAgent := '';
            RequestSize := -1;
            Closed := FALSE;
            { читаем заголовок HTTP-запроса }
            repeat
                ProcessMessages;
                Request := RecvString (TimeOut);
                if ( LastError <> 0 ) then
                    raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
                if not isEmpty (Request) then
                    Headers.Add (Request);
                if (  Pos ( 'HOST:', UpperCase (Request) ) = 1  ) then
                    Host := SeparateRight (Request,' ');
                if (  Pos ( 'USER-AGENT', UpperCase (Request) ) = 1  ) then
                    UserAgent := SeparateRight (Request,' ');
                if (  Pos ( 'CONTENT-LENGTH:', UpperCase (Request) ) = 1  ) then
                    RequestSize := StrToIntDef ( SeparateRight (Request,' '), -1 );
                Closed := (  Pos ( 'CONNECTION: CLOSE', UpperCase (Request) ) = 1  );
            until ( Request = '' );
            { читаем тело HTTP-запроса }
            TMemoryStream (InputData).Clear;
            if ( RequestSize >= 0 ) then
            begin
                TMemoryStream (InputData).SetSize (RequestSize);
                sz := Sock.RecvBufferEx ( TMemoryStream (InputData).Memory, RequestSize, TimeOut );
                if ( LastError <> 0 ) then
                    raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
                TMemoryStream (InputData).SetSize (sz);
            end;
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_THTTPSERVERTHREAD_MAIN, E, Exception (FatalException) ],
                ['{1BC1260D-7081-4871-A261-AFDF70B44536}']);
    end;
end;

procedure THTTPServerThread.Return;
var
    I : Integer;
begin
    try
        if Terminated then Exit;
        inherited Return;
        with Sock do
        begin
            { обработка }
            TMemoryStream (OutputData).Clear;
            ResultCode := Process;
            { отправляем протокол HTTP-ответа }
            SendString ( Format ('%s %d%s',[Protocol,ResultCode,CRLF]) );
            if ( LastError <> 0 ) then
                raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
            { отправляем заголовок HTTP-ответа }
            Headers.Add ( Format ('Content-length: %d',[OutputData.Size]) );
            if Closed then
                Headers.Add ('Connection: close');
            Headers.Add ( Format ('Date: %s',[ Rfc822DateTime (now) ]) );
            Headers.Add ( Format ('Server: %s',[ProjectName]) );
            Headers.Add ('');
            for I := 0 to Headers.count - 1 do
            begin
                SendString ( Headers [I] + CRLF);
                if ( LastError <> 0 ) then
                    raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
            end;
            { отправляем тело HTTP-ответа }
            SendBuffer ( TMemoryStream (OutputData).Memory, OutputData.Size );
            if ( LastError <> 0 ) then
                raise ESocketError.CreateFmt (ERR_THTTPSERVERTHREAD_SOCKET_ERROR,[LastError,LastErrorDesc]);
            if Closed then
                Terminate;
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_THTTPSERVERTHREAD_RETURN, E, Exception (FatalException) ],
                ['{F896CC45-8213-4DB4-BE32-FD843EE543FA}']);
    end;
end;

function THTTPServerThread.Process : WORD;
var
    Body : TStringList;
begin
    Result := HTTP_STATUS_INTERNAL_SERVER_ERROR;
    try
        Headers.Clear;
        Headers.Add ('Content-type: Text/Html');
        Body := TStringList.Create;
        try
            Body.Add ('<html>');
            Body.Add ('<head></head>');
            Body.Add ('<body>');
            Body.Add (ProductName);
            Body.Add ('<br>');
            Body.Add (CompanyName);
            Body.Add ('<br>');
            Body.Add ( Format ('Written by: %s',[CopyRight]) );
            Body.Add ('</body>');
            Body.Add ('</html>');
            Body.SaveToStream (OutputData);
            Result := HTTP_STATUS_OK;
        finally
            FreeAndNil (Body);
        end;
    except on E: Exception do
        _raise ([ 'Process', ERR_THTTPSERVERTHREAD_PROCESS, E, Exception (FatalException) ],
                ['{AB972A88-E8C8-48CB-8844-247859E4FDD6}']);
    end;
end;


end.
