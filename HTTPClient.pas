unit HTTPClient;
{******************************************************************************}
{*  HTTP Client Unit                                                          *}
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
    WinSock, BlckSock, SynaUtil, HTTPSend;

{$I 'HTTP.inc'}

type
{ ошибка http }
{$M+}
    EHTTP = class (EClass) end;
{$M-}

{ http-клиент }
{$M+}
    CHTTPClient = class of THTTPClient;
    PHTTPClient = ^THTTPClient;
    THTTPClient = class (TObject)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_HTTP: THTTPSend;            { клиент }
        f_Host: String;               { хост сервера }
        f_Port: WORD;                 { порт сервера }
        f_ProxyHost: String;          { хост proxy-сервера }
        f_ProxyPort: WORD;            { порт proxy-сервера }
        f_ProxyLogin: String;         { логин на proxy-сервер }
        f_ProxyPassword: String;      { пароль на proxy-сервер }
        f_ProxyProtocol: String;      { протокол proxy-сервера }
        f_TimeOut: LongWord;          { врем€ ожидани€ ms }
        f_InputHeaders: TStringList;  { исход€щие заголовки }
        f_OutputHeaders: TStringList; { вход€щие заголовки }
        f_InputData: TStream;         { вход€щий поток }
        f_OutputData: TStream;        { исход€щий поток }
        f_UserAgent: String;          { название браузера }
        f_MimeType: String;           { тип http-запроса }
        f_Method: String;             { метод }
        f_URI: String;                { адрес }
        f_ResultCode: WORD;           { код ответа }
    public
        // host, port, proxyhost, proxyport, proxylogin, proxypassword, proxyprotocol, timeout, method, uri, mime-type
        constructor Create (anArgs: array of const); virtual;
        destructor Destroy; override;
    public
        procedure Clear; virtual;
        procedure Prepare; virtual;
        function Send : Boolean; virtual;
        function Process : Boolean; virtual;
    public
        property HTTP: THTTPSend read f_HTTP;
        property Host: String read f_Host write f_Host;
        property Port: WORD read f_Port write f_Port;
        property ProxyHost: String read f_ProxyHost write f_ProxyHost;
        property ProxyPort: WORD read f_ProxyPort write f_ProxyPort;
        property ProxyLogin: String read f_ProxyLogin write f_ProxyLogin;
        property ProxyPassword: String read f_ProxyPassword write f_ProxyPassword;
        property ProxyProtocol: String read f_ProxyProtocol write f_ProxyProtocol;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
        property InputHeaders: TStringList read f_InputHeaders;
        property OutputHeaders: TStringList read f_OutputHeaders;
        property InputData: TStream read f_InputData;
        property OutputData: TStream read f_OutputData;
        property UserAgent: String read f_UserAgent write f_UserAgent;
        property MimeType: String read f_MimeType write f_MimeType;
        property Method: String read f_Method write f_Method;
        property URI: String read f_URI write f_URI;
        property ResultCode: WORD read f_ResultCode write f_ResultCode;
    end;
{$M-}

{ THTTPClient Errors }
resourcestring
    ERR_THTTPCLIENT_CREATE           = 'ќшибка создани€ веб-клиента!';
    ERR_THTTPCLIENT_DESTROY          = 'ќшибка уничтожени€ веб-клиента!';
    ERR_THTTPCLIENT_SEND             = 'ќшибка отправки запроса!';
    ERR_THTTPCLIENT_CLEAR            = 'ќшибка очистки параметров запроса!';
    ERR_THTTPCLIENT_PREPARE          = 'ќшибка подготовки запроса!';
    ERR_THTTPCLIENT_PROCESS          = 'ќшибка обработки ответа сервера!';
    ERR_THTTPCLIENT_IVALID_MIME_TYPE = 'Ќекорректный тип запроса!';
    ERR_THTTPCLIENT_IVALID_METHOD    = 'Ќекорректный метод!';
    ERR_THTTPCLIENT_IVALID_URI       = 'Ќекорректный URI!';
    ERR_THTTPCLIENT_IVALID_HOST      = 'Ќекорректный адрес!';
    ERR_THTTPCLIENT_IVALID_PORT      = 'Ќекорректный порт!';
    ERR_THTTPCLIENT_INVALID_TIMEOUT  = 'Ќекорректное врем€ ожидани€!';

implementation

{ THTTPClient }
class procedure THTTPClient._raise (anArgs: array of const;
                                    const anEGUID: String = '');
begin
    raise EHTTP.Create ( _([self],anArgs), anEGUID );
end;

class procedure THTTPClient._raise (anArgs: array of const;
                                    anEGUID: array of const);
begin
    raise EHTTP.Create ( _([self],anArgs), anEGUID );
end;

constructor THTTPClient.Create (anArgs: array of const);
begin
    try
        inherited Create;
        { создаем клиент }
        f_HTTP := THTTPSend.Create;
        { первый параметр - хост }
        f_Host := '127.0.0.1';
        if notEmpty (0,anArgs) then
        begin
            f_Host := toString (anArgs [0]);
        end;
        if isEmpty (Host) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_HOST);
        { второй параметр - порт }
        f_Port := 80;
        if notEmpty (1,anArgs) then
        begin
            f_Port := toInteger (anArgs [1]);
        end;
        if not ( Port > 0 ) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_PORT);
        { третий параметр - proxy-хост }
        f_ProxyHost := '';
        if notEmpty (2,anArgs) then
        begin
            f_ProxyHost := toString (anArgs [2]);
        end;
        { четвертый параметр - proxy-порт }
        f_ProxyPort := 0;
        if notEmpty (3,anArgs) then
        begin
            f_ProxyPort := toInteger (anArgs [3]);
        end;
        { п€тый параметр - proxy-логин }
        f_ProxyLogin := '';
        if notEmpty (4,anArgs) then
        begin
            f_ProxyLogin := toString (anArgs [4]);
        end;
        { шестой параметр - proxy-пароль }
        f_ProxyPassword := '';
        if notEmpty (5,anArgs) then
        begin
            f_ProxyPassword := toString (anArgs [5]);
        end;
        { седьмой параметр - протокол proxy-сервера }
        f_ProxyProtocol := 'HTTP';
        if notEmpty (6,anArgs) then
        begin
            f_ProxyProtocol := toString (anArgs [6]);
        end;
        { восьмой параметр - врем€ ожидани€ }
        f_TimeOut := 120000;
        if notEmpty (7,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [7]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_THTTPCLIENT_INVALID_TIMEOUT);
        { дев€тый параметр - метод }
        f_Method := 'GET';
        if notEmpty (8,anArgs) then
        begin
            f_Method := toString (anArgs [8]);
        end;
        if isEmpty (Method) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_METHOD);
        { дес€тый параметр - URI }
        f_URI := '/';
        if notEmpty (9,anArgs) then
        begin
            f_URI := toString (anArgs [9]);
        end;
        if isEmpty (URI) then
            URI := '/';
        { одиннадцатый параметр - MimeType }
        f_MimeType:= 'Text/Html';
        if notEmpty (10,anArgs) then
        begin
            f_MimeType := toString (anArgs [10]);
        end;
        if isEmpty (MimeType) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_MIME_TYPE);
        { создаем заголовки }
        f_InputHeaders := TStringList.Create;
        f_OutputHeaders := TStringList.Create;
        { создаем потоки данных }
        f_InputData := TMemoryStream.Create;
        f_OutputData := TMemoryStream.Create;
    except on E: Exception do
        _raise (['Create',ERR_THTTPCLIENT_CREATE,E],
                ['{07176B42-A450-480A-8A1D-24B268EB19F7}']);
    end;
end;

destructor THTTPClient.Destroy;
begin
    try
        try
            _FillChar ( f_MimeType, Length (f_MimeType), $00 );
            _FillChar ( f_Method, Length (f_Method), $00 );
            _FillChar ( f_URI, Length (f_URI), $00 );
            _FillChar ( f_ProxyHost, Length (f_ProxyHost), $00 );
            _FillChar ( f_ProxyLogin, Length (f_ProxyLogin), $00 );
            _FillChar ( f_ProxyPassword, Length (f_ProxyPassword), $00 );
            f_ProxyPort := 0;
            _FillChar ( f_Host, Length (f_Host), $00 );
            f_Port := 0;
            FreeAndNil (f_HTTP);
            if Assigned (InputHeaders) then
                InputHeaders.Clear;
            FreeAndNil (f_InputHeaders);
            if Assigned (OutputHeaders) then
                OutputHeaders.Clear;
            FreeAndNil (f_OutputHeaders);
            if Assigned (InputData) then
                TMemoryStream (InputData).Clear;
            FreeAndNil (f_InputData);
            if Assigned (OutputData) then
                TMemoryStream (OutputData).Clear;
            FreeAndNil (f_OutputData);
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise (['Destroy',ERR_THTTPCLIENT_DESTROY,E],
                ['{FCA5AE50-DB0D-4259-B270-3DCB301CDFB5}']);
    end;
end;

procedure THTTPClient.Clear;
begin
    try
        { очищаем параметры запроса }
        HTTP.Clear;
        { очищаем передаваемые заголовки }
        InputHeaders.Clear;
        { очищаем передаваемые данные }
        TMemoryStream (InputData).Clear;
    except on E: Exception do
        _raise (['Clear',ERR_THTTPCLIENT_CLEAR,E],
                ['{72552779-CDDA-4D7E-B298-473AF5D11138}']);
    end;
end;

procedure THTTPClient.Prepare;
var
    L : Integer;
begin
    try
        { записываем параметры запроса }
        if isEmpty (Host) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_HOST);
        if not ( Port > 0 ) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_PORT);
        { -- SOCKS-proxy }
        if (  Pos ( 'SOCKS', UpperCase (ProxyProtocol) ) > 0  ) then
        begin
            if notEmpty (ProxyHost) then
                HTTP.Sock.SocksIP := ProxyHost;
            if ( ProxyPort > 0 ) then
                HTTP.Sock.SocksPort := IntToStr (ProxyPort);
            if notEmpty (ProxyLogin) then
                HTTP.Sock.SocksUsername := ProxyLogin;
            if notEmpty (ProxyPassword) then
                HTTP.Sock.SocksPassword := ProxyPassword;
            if ( UpperCase (ProxyProtocol) = 'SOCKS4' ) then
                HTTP.Sock.SocksType := ST_Socks4
            else if ( UpperCase (ProxyProtocol) = 'SOCKS5' ) then
                HTTP.Sock.SocksType := ST_Socks5;
            HTTP.Sock.SocksResolver := FALSE;
        end
        { -- HTTP-proxy }
        else
        begin
            if notEmpty (ProxyHost) then
                HTTP.ProxyHost := ProxyHost;
            if ( ProxyPort > 0 ) then
                HTTP.ProxyPort := IntToStr (ProxyPort);
            if notEmpty (ProxyLogin) then
                HTTP.ProxyUser := ProxyLogin;
            if notEmpty (ProxyPassword) then
                HTTP.ProxyPass := ProxyPassword;
        end;
        if notEmpty (UserAgent) then
            HTTP.UserAgent := UserAgent
        else
            HTTP.UserAgent := ProductName;
        HTTP.TimeOut := TimeOut;
        if isEmpty (MimeType) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_MIME_TYPE);
        HTTP.MimeType := MimeType;
        if isEmpty (Method) then
            raise Exception.Create (ERR_THTTPCLIENT_IVALID_METHOD);
        L := Length (URI);
        if ( Copy (URI,L-1,1) = '/' ) then
            URI := Copy (URI,1,L-1);
        { записываем передаваемые заголовки }
        HTTP.Document.Write ( Pointer (InputHeaders.Text)^, Length (InputHeaders.Text) );
        { записываем передаваемые данные }
        InputData.Position := 0;
        HTTP.Document.CopyFrom (InputData,InputData.Size);
    except on E: Exception do
        _raise (['Prepare',ERR_THTTPCLIENT_PREPARE,E],
                ['{2D500176-8FC8-4C07-A34B-98787BD4981A}']);
    end;
end;

function THTTPClient.Send : Boolean;
begin
    Result := FALSE;
    try
        { подготовка запроса }
        Prepare;
        { отправл€ем запрос }
        ResultCode := HTTP_STATUS_INTERNAL_SERVER_ERROR;
        if HTTP.HTTPMethod ( Method, Format ('http://%s%s:%d',[Host,URI,Port]) ) then
            ResultCode := HTTP.ResultCode;
        { читаем полученные заголовки }
        OutputHeaders.Clear;
        OutputHeaders.Assign (HTTP.Headers);
        { читаем полученные данные }
        TMemoryStream (OutputData).Clear;
        HTTP.Document.Position := 0;
        OutputData.CopyFrom (HTTP.Document,HTTP.Document.Size);
        { обрабатываем полученные данные }
        Result := Process;
    except on E: Exception do
        _raise (['Send',ERR_THTTPCLIENT_SEND,E],
                ['{00A426A1-26DC-48F5-BB3B-462DB4E7F2FF}']);
    end;
end;

function THTTPClient.Process : Boolean;
begin
    Result := TRUE;
    try
        { анализируем код ответа }
        Result := ( ResultCode = HTTP_STATUS_OK );
    except on E: Exception do
        _raise (['Process',ERR_THTTPCLIENT_PROCESS,E],
                ['{0CF99274-1715-41D8-AF67-B000BBDD403E}']);
    end;
end;


end.
