unit SMTPClient;
{******************************************************************************}
{*  SMTP Client Unit                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
{ utils }
    DateUtils, Utils, Strings, VarRecs, Versions, EClasses,
    { synapse }
    blcksock, synautil, synachar, smtpsend, mimemess, mimepart,
    { synapse - open ssl }
    ssl_openssl;

type
{ ошибка smtp }
{$M+}
    ESMTP = class (EClass) end;
{$M-}

{ smtp-клиент }
{$M+}
    CSMTPClient = class of TSMTPClient;
    PSMTPClient = ^TSMTPClient;
    TSMTPClient = class (TObject)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_SMTP: TSMTPSend;        { клиент }
        f_MimePart: TMimePart;    { заголовки }
        f_MimeMessage: TMimeMess; { сообщение }
        f_Host: String;           { хост сервера }
        f_Port: WORD;             { порт сервера }
        f_Login: String;          { логин на сервер }
        f_Password: String;       { пароль на сервер }
        f_ProxyHost: String;      { хост proxy-сервера }
        f_ProxyPort: WORD;        { порт proxy-сервера }
        f_ProxyLogin: String;     { логин на proxy-сервер }
        f_ProxyPassword: String;  { пароль на proxy-сервер }
        f_ProxyProtocol: String;  { протокол proxy-сервера }
        f_TimeOut: LongWord;      { врем€ ожидани€ ms }
        f_AutoTLS: Boolean;       { использовать TLS }
        f_FullSSL: Boolean;       { использовать расширенный SSL }
        f_Sender: String;         { e-mail отправител€ }
        f_Receivers: TStringList; { список e-mail получателей }
        f_Subject: String;        { тема письма }
        f_Data: TStream;          { поток данных }
        f_UserAgent: String;      { название почтового клиента }
    public
        constructor Create (anArgs: array of const); virtual;
        destructor Destroy; override;
    protected
        function SignIn : Boolean; virtual;
        procedure SignOut; virtual;
    public
        procedure Clear; virtual;
        procedure Prepare; virtual;
        function Send : Boolean; virtual;
    public
        property SMTP: TSMTPSend read f_SMTP;
        property MimePart: TMimePart read f_MimePart;
        property MimeMessage: TMimeMess read f_MimeMessage;
        property Host: String read f_Host write f_Host;
        property Port: WORD read f_Port write f_Port;
        property Login: String read f_Login write f_Login;
        property Password: String read f_Password write f_Password;
        property ProxyHost: String read f_ProxyHost write f_ProxyHost;
        property ProxyPort: WORD read f_ProxyPort write f_ProxyPort;
        property ProxyLogin: String read f_ProxyLogin write f_ProxyLogin;
        property ProxyPassword: String read f_ProxyPassword write f_ProxyPassword;
        property ProxyProtocol: String read f_ProxyProtocol write f_ProxyProtocol;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
        property AutoTLS: Boolean read f_AutoTLS write f_AutoTLS;
        property FullSSL: Boolean read f_FullSSL write f_FullSSL;
        property Sender: String read f_Sender write f_Sender;
        property Receivers: TStringList read f_Receivers;
        property Subject: String read f_Subject write f_Subject;
        property Data: TStream read f_Data;
        property UserAgent: String read f_UserAgent write f_UserAgent;
    end;
{$M-}

{ TSMTPClient Errors }
resourcestring
    ERR_TSMTPCLIENT_CREATE          = 'ќшибка создани€ smtp-клиента!';
    ERR_TSMTPCLIENT_DESTROY         = 'ќшибка уничтожени€ smtp-клиента!';
    ERR_TSMTPCLIENT_SEND            = 'ќшибка отправки запроса!';
    ERR_TSMTPCLIENT_PREPARE         = 'ќшибка подготовки запроса!';
    ERR_TSMTPCLIENT_CLEAR           = 'ќшибка очистки передаваемых данных!';
    ERR_TSMTPCLIENT_IVALID_HOST     = 'Ќекорректный адрес!';
    ERR_TSMTPCLIENT_IVALID_PORT     = 'Ќекорректный порт!';
    ERR_TSMTPCLIENT_INVALID_TIMEOUT = 'Ќекорректное врем€ ожидани€!';
    ERR_TSMTPCLIENT_IVALID_LOGIN    = 'Ќекорректный логин!';
    ERR_TSMTPCLIENT_IVALID_PASSWORD = 'Ќекорректный пароль!';

implementation

{ TSMTPClient }
class procedure TSMTPClient._raise (anArgs: array of const;
                                    const anEGUID: String = '');
begin
    raise ESMTP.Create ( _([self],anArgs), anEGUID );
end;

class procedure TSMTPClient._raise (anArgs: array of const;
                                    anEGUID: array of const);
begin
    raise ESMTP.Create ( _([self],anArgs), anEGUID );
end;

constructor TSMTPClient.Create (anArgs: array of const);
begin
    try
        inherited Create;
        { создаем клиент }
        f_SMTP := TSMTPSend.Create;
        f_MimeMessage := TMimeMess.Create;
        f_MimePart := f_MimeMessage.AddPartMultipart ('alternate',NIL);
        { первый параметр - хост }
        f_Host := '127.0.0.1';
        if notEmpty (0,anArgs) then
        begin
            f_Host := toString (anArgs [0]);
        end;
        if isEmpty (Host) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_HOST);
        { второй параметр - порт }
        f_Port := 25;
        if notEmpty (1,anArgs) then
        begin
            f_Port := toInteger (anArgs [1]);
        end;
        if not ( Port > 0 ) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_PORT);
        { третий параметр - логин }
        f_Login := '';
        if notEmpty (2,anArgs) then
        begin
            f_Login := GetEmailAddr ( toString (anArgs [2]) );
        end;
        if isEmpty (Login) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_LOGIN);
        { четвертый параметр - пароль }
        f_Password := '';
        if notEmpty (3,anArgs) then
        begin
            f_Password := GetEmailAddr ( toString (anArgs [3]) );
        end;
        if isEmpty (Password) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_PASSWORD);
        { п€тый параметр - proxy-хост }
        f_ProxyHost := '';
        if notEmpty (4,anArgs) then
        begin
            f_ProxyHost := toString (anArgs [4]);
        end;
        { шестой параметр - proxy-порт }
        f_ProxyPort := 0;
        if notEmpty (5,anArgs) then
        begin
            f_ProxyPort := toInteger (anArgs [5]);
        end;
        { седьмой параметр - proxy-логин }
        f_ProxyLogin := '';
        if notEmpty (6,anArgs) then
        begin
            f_ProxyLogin := toString (anArgs [6]);
        end;
        { восьмой параметр - proxy-пароль }
        f_ProxyPassword := '';
        if notEmpty (7,anArgs) then
        begin
            f_ProxyPassword := toString (anArgs [7]);
        end;
        { дев€тый параметр - протокол proxy-сервера }
        f_ProxyProtocol := 'SOCKS5';
        if notEmpty (8,anArgs) then
        begin
            f_ProxyProtocol := toString (anArgs [8]);
        end;
        { дес€тый параметр - врем€ ожидани€ }
        f_TimeOut := 120000;
        if notEmpty (9,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [9]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_TSMTPCLIENT_INVALID_TIMEOUT);
        { одиннадцатый параметр - TLS }
        f_AutoTLS := FALSE;
        if notEmpty (10,anArgs) then
        begin
            f_AutoTLS := toBoolean (anArgs [10]);
        end;
        { двенадцатый параметр - SSL }
        f_FullSSL := FALSE;
        if notEmpty (11,anArgs) then
        begin
            f_FullSSL := toBoolean (anArgs [11]);
        end;
        { отправитель }
        Sender := Login; 
        { список получателй }
        f_Receivers := TStringList.Create;
        { по-умолчанию отправл€ем почту сами себе }
        Receivers.Add (Sender);
        { тема письма }
        f_Subject := '';
        { создаем исход€щий поток данных }
        f_Data := TMemoryStream.Create;
    except on E: Exception do
        _raise (['Create',ERR_TSMTPCLIENT_CREATE,E],
                ['{B4CB736C-8C7D-4BA2-9171-98CC6E2EF8B3}']);
    end;
end;

destructor TSMTPClient.Destroy;
begin
    try
        _FillChar ( f_Host, Length (f_Host), $00 );
        f_Port := 0;
        _FillChar ( f_Login, Length (f_Login), $00 );
        _FillChar ( f_Password, Length (f_Password), $00 );
        _FillChar ( f_ProxyHost, Length (f_ProxyHost), $00 );
        f_ProxyPort := 0;
        _FillChar ( f_ProxyLogin, Length (f_ProxyLogin), $00 );
        _FillChar ( f_ProxyPassword, Length (f_ProxyPassword), $00 );
        _FillChar ( f_ProxyProtocol, Length (f_ProxyProtocol), $00 );
        FreeAndNil (f_SMTP);
        if Assigned (MimeMessage) then
            MimeMessage.Clear;
        FreeAndNil (f_MimeMessage);
        _FillChar ( f_Sender, Length (f_Sender), $00 );
        if Assigned (Receivers) then
            Receivers.Clear;
        FreeAndNil (f_Receivers);
        _FillChar ( f_Subject, Length (f_Subject), $00 );
        if Assigned (Data) then
            TMemoryStream (Data).Clear;
        FreeAndNil (f_Data);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TSMTPCLIENT_DESTROY,E],
                ['{0CBB9862-A934-4B5D-B02D-6DCCCCAE337D}']);
    end;
end;

procedure TSMTPClient.Clear;
begin
    try
        { очищаем параметры запроса }
        MimePart.Clear;
        MimeMessage.Clear;
        { очищаем передаваемые заголовки }
        _FillChar ( f_Sender, Length (f_Sender), $00 );
        Receivers.Clear;
        _FillChar ( f_Subject, Length (f_Subject), $00 );
        { очищаем передаваемые данные }
        TMemoryStream (Data).Clear;
    except on E: Exception do
        _raise (['Clear',ERR_TSMTPCLIENT_CLEAR,E],
                ['{9CA13FDA-1A52-43B8-ADB6-97FAE4F93399}']);
    end;
end;

procedure TSMTPClient.Prepare;
begin
    try
        { записываем параметры запроса }
        if isEmpty (Host) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_HOST);
        SMTP.TargetHost := Host;
        if not ( Port > 0 ) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_PORT);
        SMTP.TargetPort := IntToStr (Port);
        if isEmpty (Login) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_LOGIN);
        SMTP.UserName := Login;
        if isEmpty (Password) then
            raise Exception.Create (ERR_TSMTPCLIENT_IVALID_PASSWORD);
        { -- SOCKS-proxy }
        SMTP.Password := Password;
        if (  Pos ( 'SOCKS', UpperCase (ProxyProtocol) ) > 0  ) then
        begin
            if notEmpty (ProxyHost) then
                SMTP.Sock.SocksIP := ProxyHost;
            if ( ProxyPort > 0 ) then
                SMTP.Sock.SocksPort := IntToStr (ProxyPort);
            if notEmpty (ProxyLogin) then
                SMTP.Sock.SocksUsername := ProxyLogin;
            if notEmpty (ProxyPassword) then
                SMTP.Sock.SocksPassword := ProxyPassword;
            if ( UpperCase (ProxyProtocol) = 'SOCKS4' ) then
                SMTP.Sock.SocksType := ST_Socks4
            else if ( UpperCase (ProxyProtocol) = 'SOCKS5' ) then
                SMTP.Sock.SocksType := ST_Socks5;
            SMTP.Sock.SocksResolver := FALSE;
        end;
        if notEmpty (UserAgent) then
            MimeMessage.Header.XMailer := UserAgent
        else
            MimeMessage.Header.XMailer := ProductName;
        SMTP.TimeOut := TimeOut;
        SMTP.AutoTLS := AutoTLS;
        SMTP.FullSSL := FullSSL;
        { записываем заголовки }
        MimeMessage.Header.Subject := Subject;
        MimeMessage.Header.From := Sender;
        MimeMessage.Header.ToList.Assign (Receivers);
        { записываем данные }
        Data.Position := 0;
        with MimePart do
        begin
            DecodedLines.CopyFrom (Data,Data.Size);
            Primary := 'text';
            Secondary := 'plain';
            Description := 'Message text';
            Disposition := 'inline';
            CharsetCode := TargetCharset;
            EncodingCode := ME_7BIT;//ME_QUOTED_PRINTABLE;
            EncodePart;
            EncodePartHeader;
        end;
    except on E: Exception do
        _raise (['Prepare',ERR_TSMTPCLIENT_PREPARE,E],
                ['{0E9EFE6D-7C29-4731-852B-82AB1C520975}']);
    end;
end;

function TSMTPClient.SignIn : Boolean;
begin
    Result := SMTP.Login;
    if not Result and ( Pos ('@',Login) > 0 ) then
    begin
        SMTP.UserName := SeparateLeft (Login,'@');
        Result := SMTP.Login;
    end;
end;

procedure TSMTPClient.SignOut;
begin
    SMTP.Logout;
end;

function TSMTPClient.Send : Boolean;
var
    I : Integer;
begin
    Result := FALSE;
    try
        { подготавливаем запрос }
        Prepare;
        { отправл€ем сообщение }
        MimeMessage.EncodeMessage;
        if SignIn then
        try
            if SMTP.MailFrom ( Sender, Length (MimeMessage.Lines.Text) ) then
            begin
                Result := TRUE;
                for I := 0 to MimeMessage.Header.ToList.Count - 1 do
                    Result := Result and SMTP.MailTo (MimeMessage.Header.ToList [I]);
                if Result then
                    Result := SMTP.MailData (MimeMessage.Lines);
            end;
        finally
            SignOut;
        end;
    except on E: Exception do
        _raise (['Send',ERR_TSMTPCLIENT_SEND,E],
                ['{AB3ED554-E777-45F2-B627-9F9D10E7D05C}']);
    end;
end;

end.
