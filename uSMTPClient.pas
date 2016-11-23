unit uSMTPClient;
{******************************************************************************}
{*  Package SMTP Client Unit                                                  *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2012                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, SysUtils, Variants, Classes,
    Controls, ComCtrls, Gauges,
    sListView,
{ utils }
    DateUtils, Utils, Strings, VarRecs, Versions, EClasses,
    DllThreads,
{ synapse }
    WinSock, BlckSock, SynaUtil, SMTPSend,
    PingSend,
{ smtp }
    SMTPClient,
{ kernel }
    Kernel, ProtoClasses, CryptoClasses, MetaClasses, ParaClasses,
    HypoClasses, HyperClasses,
{ engine }
    Engine,
{ SQLite }
    SQLite3, SQLite3DLL, SQLiteTable3;

{ иконки }
const
    pckDefault = 0;
    pckEmpty   = 1;
    pckFull    = 2;
    pckTest    = 3;
    pckError   = 4;
    pckSend    = 5;
    pckReceive = 6;

type
    PLogRecord = ^TLogRecord;
    TLogRecord = packed record
        Caption    : ShortString;
        ImageIndex : Integer;
        Sender     : ShortString;
        Receiver   : ShortString;
        Message    : ShortString;
    end;

type
{ ошибка smtp-клиента }
{$M+}
    ESMTPClientError = class (EThreadError) end;
{$M-}

{ поток пакетного клиента }
{$M+}
    CSMTPPackageClient = class of TSMTPPackageClient;
    PSMTPPackageClient = ^TSMTPPackageClient;
    TSMTPPackageClient = class (TDllThread)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); override;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); override;
    private
        f_Threads: TDllThreads;      { дочерние потоки }
        f_Host: String;              { smtp-хост }
        f_Port: WORD;                { smtp-порт }
        f_Login: String;             { логин почты }
        f_Password: String;          { пароль почты }
        f_AutoTLS: Boolean;          { auto tls }
        f_FullSSL: Boolean;          { full ssl }
        f_ProxyHost: String;         { хост proxy-сервера }
        f_ProxyPort: WORD;           { порт proxy-сервера }
        f_ProxyLogin: String;        { логин на proxy-сервер }
        f_ProxyPassword: String;     { пароль на proxy-сервер }
        f_ProxyProtocol: String;     { протокол proxy-сервера }
        f_TimeOut: LongWord;         { время ожидания ms }
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_Users: TUsers;             { список контактов в рассылке }
        f_UserIndex: LongInt;        { позиция в обходе списка }
    public
        // DBFileName, Host, Port, Login, Password, AutoTLS, FullSSL,
        // ListView, Gauge, StatusPanel, TimeOut,
        // ProxyHost, ProxyPort, ProxyLogin, ProxyPassword, ProxyProtocol
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
        procedure Return; override;
    protected
        procedure WriteStatus (const aMessage: String); overload;
        procedure WriteStatus (const aMessage: String;
                               aParams: array of const); overload;
    public
        property Threads: TDllThreads read f_Threads write f_Threads;
        property Host: String read f_Host write f_Host;
        property Port: WORD read f_Port write f_Port;
        property Login: String read f_Login write f_Login;
        property Password: String read f_Password write f_Password;
        property AutoTLS: Boolean read f_AutoTLS write f_AutoTLS;
        property FullSSL: Boolean read f_FullSSL write f_FullSSL;
        property ProxyHost: String read f_ProxyHost write f_ProxyHost;
        property ProxyPort: WORD read f_ProxyPort write f_ProxyPort;
        property ProxyLogin: String read f_ProxyLogin write f_ProxyLogin;
        property ProxyPassword: String read f_ProxyPassword write f_ProxyPassword;
        property ProxyProtocol: String read f_ProxyProtocol write f_ProxyProtocol;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property Users: TUsers read f_Users write f_Users;
        property UserIndex: LongInt read f_UserIndex write f_UserIndex;
    end;
{$M-}

{ дочерний поток пакетного клиента }
{$M+}
    CSMTPPackageClientThread = class of TSMTPPackageClientThread;
    PSMTPPackageClientThread = ^TSMTPPackageClientThread;
    TSMTPPackageClientThread = class (TDllThread)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); override;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); override;
    private
        f_SMTP: TSMTPClient;         { клиент }
        f_Host: String;              { smtp-хост }
        f_Port: WORD;                { smtp-порт }
        f_Login: String;             { логин почты }
        f_Password: String;          { пароль почты }
        f_AutoTLS: Boolean;          { auto tls }
        f_FullSSL: Boolean;          { full ssl }
        f_ProxyHost: String;         { хост proxy-сервера }
        f_ProxyPort: WORD;           { порт proxy-сервера }
        f_ProxyLogin: String;        { логин на proxy-сервер }
        f_ProxyPassword: String;     { пароль на proxy-сервер }
        f_ProxyProtocol: String;     { протокол proxy-сервера }
        f_TimeOut: LongWord;         { время ожидания ms }
        f_DBFileName: String;        { файл базы данных }
        f_DB: TSQLiteDatabase;       { объект базы данных }
        f_ReceiverID: TID;           { идентификатор получателя }
        f_Receiver: TUser;           { получатель }
        f_ListView: TsListView;      { лог }
        f_Gauge: TGauge;             { показатель прогресса загрузки }
        f_StatusPanel: TStatusPanel; { строка состояния загрузки }
        f_Packages: TPackages;       { список пакетов для рассылки }
        f_PackageIndex: LongInt;     { позиция в обходе списка }
        f_IdleTime: LongInt;         { время ожидания между запросами }
    private
        f_Log: TItems;
        f_Status: String;
        f_MaxProgress: WORD;
        f_Progress: WORD;
    public
        // DBFileName, ReceiverID, Host, Port, Login, Password, AutoTLS, FullSSL,
        // ListView, Gauge, StatusPanel, TimeOut,
        // ProxyHost, ProxyPort, ProxyLogin, ProxyPassword, ProxyProtocol
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
    public
        procedure Main; override;
        procedure Return; override;
    protected
        procedure WriteStatus (const aMessage: String); overload;
        procedure WriteStatus (const aMessage: String;
                               aParams: array of const); overload;
        procedure WriteLog (const aSender: String;
                            const aReceiver: String;
                            const aMessage: String;
                            const anImageIndex: Integer = pckDefault;
                            const aPackageType: String = '';
                            const aMetaClassID: String = ''); overload;
        procedure WriteLog (const aSender: String;
                            const aReceiver: String;
                            const aMessage: String;
                            aParams: array of const;
                            const anImageIndex: Integer = pckDefault;
                            const aPackageType: String = '';
                            const aMetaClassID: String = ''); overload;
    public
        property SMTP: TSMTPClient read f_SMTP;
        property Host: String read f_Host write f_Host;
        property Port: WORD read f_Port write f_Port;
        property Login: String read f_Login write f_Login;
        property Password: String read f_Password write f_Password;
        property AutoTLS: Boolean read f_AutoTLS write f_AutoTLS;
        property FullSSL: Boolean read f_FullSSL write f_FullSSL;
        property ProxyHost: String read f_ProxyHost write f_ProxyHost;
        property ProxyPort: WORD read f_ProxyPort write f_ProxyPort;
        property ProxyLogin: String read f_ProxyLogin write f_ProxyLogin;
        property ProxyPassword: String read f_ProxyPassword write f_ProxyPassword;
        property ProxyProtocol: String read f_ProxyProtocol write f_ProxyProtocol;
        property TimeOut: LongWord read f_TimeOut write f_TimeOut;
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
        property ReceiverID: TID read f_ReceiverID write f_ReceiverID;
        property Receiver: TUser read f_Receiver;
        property ListView: TsListView read f_ListView write f_ListView;
        property Gauge: TGauge read f_Gauge write f_Gauge;
        property StatusPanel: TStatusPanel read f_StatusPanel write f_StatusPanel;
        property Packages: TPackages read f_Packages write f_Packages;
        property PackageIndex: LongInt read f_PackageIndex write f_PackageIndex;
        property IdleTime: LongInt read f_IdleTime write f_IdleTime;
        property Log: TItems read f_Log write f_Log;
        property Status: String read f_Status write f_Status;
        property MaxProgress: WORD read f_MaxProgress write f_MaxProgress;
        property Progress: WORD read f_Progress write f_Progress;
    end;
{$M-}

{ TSMTPPackageClient Errors }
resourcestring
    ERR_TSMTPPACKAGECLIENT_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TSMTPPACKAGECLIENT_INVALID_HOST           = 'Некорректный smtp-хост!';
    ERR_TSMTPPACKAGECLIENT_INVALID_PORT           = 'Некорректный smtp-порт!';
    ERR_TSMTPPACKAGECLIENT_INVALID_LOGIN          = 'Некорректный логин к smtp-серверу!';
    ERR_TSMTPPACKAGECLIENT_INVALID_PROXY_HOST     = 'Некорректный proxy-хост!';
    ERR_TSMTPPACKAGECLIENT_INVALID_PROXY_PORT     = 'Некорректный proxy-порт!';
    ERR_TSMTPPACKAGECLIENT_INVALID_PROXY_PROTOCOL = 'Некорректный proxy-протокол!';
    ERR_TSMTPPACKAGECLIENT_INVALID_TIMEOUT        = 'Некорректный таймаут для соединений!';
    ERR_TSMTPPACKAGECLIENT_CREATE                 = 'Ошибка создания потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENT_DESTROY                = 'Ошибка уничтожения потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENT_MAIN                   = 'Ошибка выполнения главной функции потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENT_RETURN                 = 'Ошибка функции возврата пакетного клиента!';
    ERR_TSMTPPACKAGECLIENT_WRITE_STATUS           = 'Ошибка отображения статуса!';

{ TSMTPPackageClientThread Errors }
resourcestring
    ERR_TSMTPPACKAGECLIENTTHREAD_INCORRECT_DATABASE     = 'Не инициализирован объект БД!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_RECEIVER_ID    = 'Некорректный идентификатор получателя!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_RECEIVER       = 'Некорректный получатель!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_HOST           = 'Некорректный smtp-хост!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PORT           = 'Некорректный smtp-порт!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_LOGIN          = 'Некорректный логин к smtp-серверу!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PROXY_HOST     = 'Некорректный proxy-хост!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PROXY_PORT     = 'Некорректный proxy-порт!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PROXY_PROTOCOL = 'Некорректный proxy-протокол!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_TIMEOUT        = 'Некорректный таймаут для соединений!';
    ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_DATA           = 'Некорректные данные!';
    ERR_TSMTPPACKAGECLIENTTHREAD_CREATE                 = 'Ошибка создания дочернего потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENTTHREAD_DESTROY                = 'Ошибка уничтожения дочернего потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENTTHREAD_MAIN                   = 'Ошибка выполнения главной функции дочернего потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENTTHREAD_RETURN                 = 'Ошибка выполнения функции возврата дочернего потока пакетного клиента!';
    ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_STATUS           = 'Ошибка отображения статуса!';
    ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_LOG              = 'Ошибка записи в лог!';

{ TSMTPPackage Hints }
resourcestring
    MSG_TSMTPPACKAGE_SEND_TO          = 'Отправка пакетов для ''%s''...';
    MSG_TSMTPPACKAGE_SEND_ERROR       = 'Ошибка отправки пакета. SMTP Error: %d';
    MSG_TSMTPPACKAGE_SENDED_PACKAGE   = 'Пакет ''%s'' отправлен получателю.';
    MSG_TSMTPPACKAGE_RECEIVED_PACKAGE = 'Пакет ''%s'' доставлен получателю.';
    MSG_TSMTPPACKAGE_EXECUTED_PACKAGE = 'Пакет ''%s'' обработан получателем.';
    MSG_TSMTPPACKAGE_REJECTED_PACKAGE = 'Пакет ''%s'' отвергнут получателем.';

implementation

{ TSMTPPackageClient }
class procedure TSMTPPackageClient._raise (anArgs: array of const;
                                           const anEGUID: String = '');
begin
    raise ESMTPClientError.Create ( _([self],anArgs), anEGUID );
end;

class procedure TSMTPPackageClient._raise (anArgs: array of const;
                                           anEGUID: array of const);
begin
    raise ESMTPClientError.Create ( _([self],anArgs), anEGUID );
end;

constructor TSMTPPackageClient.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с семнадцатого аргумента }
        if ( High (anArgs) >= 16 ) then
        begin
            SetLength ( Args, High (anArgs)-16 +1 );
            for I := 16 to High (anArgs) do
                args [I-16] := anArgs [I];
        end
        else
            args := _array_of_const ([]);
        inherited Create (args);
        { явно указываем на необходимость уничтожения потока по окончанию работы }
        FreeOnTerminate := TRUE;
        { имя потока }
        Name := ClassName;
        { приоритет }
        Priority := tpIdle;
        { первый параметр - файл БД }
        f_DBFileName := '';
        f_DB := NIL;
        if notEmpty (0,anArgs) then
        begin
            f_DBFileName := toString (anArgs [0]);
            f_DB := TSQLiteDatabase.Create (f_DBFileName);
        end;
        if ( not Assigned (f_DB) ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INCORRECT_DATABASE);
        { второй параметр - хост smtp-сервера }
        f_Host := '';
        if notEmpty (1,anArgs) then
        begin
            f_Host := toString (anArgs [1]);
        end;
        if isEmpty (Host) {or not isIPAddress (Host)} then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_HOST);
        { третий параметр - порт smtp-сервера }
        f_Port := 0;
        if notEmpty (2,anArgs) then
        begin
            f_Port := toInteger (anArgs [2]);
        end;
        if ( Port <= 0 ) and ( Host <> '' ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_PORT);
        { четвертый параметр - логин к smtp-серверу }
        f_Login := '';
        if notEmpty (3,anArgs) then
        begin
            f_Login := toString (anArgs [3]);
        end;
        if isEmpty (Login) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_LOGIN);
        { пятый параметр - пароль к smtp-серверу }
        f_Password := '';
        if notEmpty (4,anArgs) then
        begin
            f_Password := toString (anArgs [4]);
        end;
        { шестой параметр - AutoTLS }
        f_AutoTLS := FALSE;
        if notEmpty (5,anArgs) then
        begin
            f_AutoTLS := toBoolean (anArgs [5]);
        end;
        { седьмой параметр - FullSSL }
        f_FullSSL := FALSE;
        if notEmpty (6,anArgs) then
        begin
            f_FullSSL := toBoolean (anArgs [6]);
        end;
        { восьмой параметр - ListView
          лог почты }
        f_ListView := NIL;
        if notEmpty (7,anArgs) then
        begin
            OBJ := toObject (anArgs [7]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { девятый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (8,anArgs) then
        begin
            OBJ := toObject (anArgs [8]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { десятый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (9,anArgs) then
        begin
            OBJ := toObject (anArgs [9]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        { одиннадцатый параметр - время ожидания ms }
        f_TimeOut := 120000;
        if notEmpty (10,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [10]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_TIMEOUT);
        { двенадцатый параметр - хост proxy-сервера }
        f_ProxyHost := '';
        if notEmpty (11,anArgs) then
        begin
            f_ProxyHost := toString (anArgs [11]);
        end;
        if ( ProxyHost <> '' ) and not isIPAddress (ProxyHost) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_PROXY_HOST);
        { тринадцатый параметр - порт proxy-сервера }
        f_ProxyPort := 0;
        if notEmpty (12,anArgs) then
        begin
            f_ProxyPort := toInteger (anArgs [12]);
        end;
        if ( ProxyPort <= 0 ) and ( ProxyHost <> '' ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENT_INVALID_PROXY_PORT);
        { четырнадцатый параметр - логин proxy-сервера }
        f_ProxyLogin := '';
        if notEmpty (13,anArgs) then
        begin
            f_ProxyLogin := toString (anArgs [13]);
        end;
        { пятнадцатый параметр - пароль proxy-сервера }
        f_ProxyPassword := '';
        if notEmpty (14,anArgs) then
        begin
            f_ProxyPassword := toString (anArgs [14]);
        end;
        { шестнадцатый параметр - протокол proxy-сервера }
        f_ProxyProtocol := '';
        if notEmpty (15,anArgs) then
        begin
            f_ProxyProtocol := toString (anArgs [15]);
        end;
        { создаем контейнер дочерних потоков }
        f_Threads := TDllThreads.Create ([]);
        { список пользователей }
        f_Users := NIL;
        f_UserIndex := -1;
    except on E: Exception do
        _raise ([ 'Create', ERR_TSMTPPACKAGECLIENT_CREATE, E, Exception (FatalException) ],
                ['{4EB8AAA0-18F0-4A07-B8AD-3436B8B84C13}']);
    end;
end;

destructor TSMTPPackageClient.Destroy;
begin
    try
        try
            _FillChar ( f_Host, Length (f_Host), $00 );
            f_Port := 0;
            _FillChar ( f_Login, Length (f_Login), $00 );
            _FillChar ( f_Password, Length (f_Password), $00 );
            _FillChar ( f_ProxyHost, Length (f_ProxyHost), $00 );
            f_ProxyPort := 0;
            _FillChar ( f_ProxyLogin, Length (f_ProxyLogin), $00 );
            _FillChar ( f_ProxyPassword, Length (f_ProxyPassword), $00 );
            if Assigned (Threads) then
                Threads.Terminate;
            FreeAndNil (f_Threads);
            FreeAndNil (f_Users);
            FreeAndNil (f_DB);
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TSMTPPACKAGECLIENT_DESTROY, E, Exception (FatalException) ],
                ['{7FDDAECC-A00B-40A6-9111-B18CC27FF929}']);
    end;
end;

procedure TSMTPPackageClient.Main;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (Users) then
        try
            f_Users := TUsers.Load (DB,[ _([]),
                                         _([]),
                                         _([USER_ID]) ],
                                       [],
                                       [],
                                       [objSimple]) as TUsers;
        except
            FreeAndNil (f_Users);
        end;
        Sleep (TimeOut*10);
    except on E: Exception do
        _raise ([ 'Main', ERR_TSMTPPACKAGECLIENT_MAIN, E, Exception (FatalException) ],
                ['{B32D182D-D54F-49D7-BF26-3F73FD05F2CC}']);
    end;
end;

procedure TSMTPPackageClient.Return;
var
    Thr : TSMTPPackageClientThread;
begin
    try
        if Terminated then Exit;
        if ( not Assigned (Users) or (Users.Count <= 0) ) then
            Terminate
        else if ( UserIndex >= Users.Count - 1 ) then
        begin
            UserIndex := -1;
        end
        { если один из дочерних потоков завершился - запускаем его снова }
        else if ( UserIndex <= Users.Count - 1 ) and
                not Assigned ( Threads.ItemOf [ Users.ItemAt [UserIndex+1].KeyHash ] ) then
        try
            inherited Return;
            UserIndex := UserIndex + 1;
            if notEmpty ( Users.ItemAt [UserIndex].EMail ) then
            try
                Thr := TSMTPPackageClientThread.Create ([ DBFileName,
                                                          Users.ItemAt [UserIndex].ID,
                                                          Host, Port,
                                                          Login, Password,
                                                          AutoTLS, FullSSL,
                                                          ListView,
                                                          Gauge,
                                                          StatusPanel,
                                                          TimeOut,
                                                          ProxyHost, ProxyPort,
                                                          ProxyLogin, ProxyPassword,
                                                          ProxyProtocol,
                                                          FALSE, TRUE,
                                                          TP_IDLE,
                                                          NIL, NIL,
                                                          Users.ItemAt [UserIndex].KeyHash ]);
                if ( Threads.Add (Thr) < 0 ) then
                    FreeAndNil (Thr);
            except
                FreeAndNil (Thr);
            end;
        except on E: Exception do
            WriteStatus (E.Message);
        end;
    except on E: Exception do
        _raise ([ 'Return', ERR_TSMTPPACKAGECLIENT_RETURN, E, Exception (FatalException) ],
                ['{2F0B941D-0C1F-406F-9678-4E3709369E9E}']);
    end;
end;

procedure TSMTPPackageClient.WriteStatus (const aMessage: String);
begin
    try
        if Assigned (StatusPanel) then
        begin
            StatusPanel.Text := aMessage;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TSMTPPACKAGECLIENT_WRITE_STATUS,E],
                ['{1965E534-BC3E-4035-98F7-BB018B88422E}']);
    end;
end;

procedure TSMTPPackageClient.WriteStatus (const aMessage: String;
                                          aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TSMTPPACKAGECLIENT_WRITE_STATUS,E],
                ['{9A423386-583B-4521-8A0C-6365B188B92E}']);
    end;
end;

{ TSMTPPackageClientThread }
class procedure TSMTPPackageClientThread._raise (anArgs: array of const;
                                                 const anEGUID: String = '');
begin
    raise ESMTPClientError.Create ( _([self],anArgs), anEGUID );
end;

class procedure TSMTPPackageClientThread._raise (anArgs: array of const;
                                                 anEGUID: array of const);
begin
    raise ESMTPClientError.Create ( _([self],anArgs), anEGUID );
end;

constructor TSMTPPackageClientThread.Create (anArgs: array of const);
var
    I    : Integer;
    args : array_of_const;
    OBJ  : TObject;
begin
    try
        { передаем параметры создания базового потока,
          начиная с восемнадцатого аргумента }
        if ( High (anArgs) >= 17 ) then
        begin
            SetLength ( Args, High (anArgs)-17 +1 );
            for I := 17 to High (anArgs) do
                args [I-17] := anArgs [I];
        end
        else
            args := _array_of_const ([]);
        inherited Create (args);
        { явно указываем на необходимость уничтожения потока по окончанию работы }
        FreeOnTerminate := TRUE;
        { первый параметр - файл БД }
        f_DBFileName := '';
        f_DB := NIL;
        if notEmpty (0,anArgs) then
        begin
            f_DBFileName := toString (anArgs [0]);
            f_DB := TSQLiteDatabase.Create (f_DBFileName);
        end;
        if ( not Assigned (f_DB) ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INCORRECT_DATABASE);
        { второй параметр - ReceiverID
          идентификатор получателя }
        f_ReceiverID := 0;
        if notEmpty (1,anArgs) then
        begin
            f_ReceiverID := toInt64 (anArgs [1]);
        end;
        if not ( ReceiverID > 0 ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_RECEIVER_ID);
        f_Receiver := TUser.Load (DB,ReceiverID,[objSimple]) as TUser;
        if not Assigned (Receiver) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_RECEIVER);
        { третий параметр - хост smtp-сервера }
        f_Host := '';
        if notEmpty (2,anArgs) then
        begin
            f_Host := toString (anArgs [2]);
        end;
        if isEmpty (Host) {or not isIPAddress (Host)} then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_HOST);
        { четвертый параметр - порт smtp-сервера }
        f_Port := 0;
        if notEmpty (3,anArgs) then
        begin
            f_Port := toInteger (anArgs [3]);
        end;
        if ( Port <= 0 ) and ( Host <> '' ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PORT);
        { пятый параметр - логин к smtp-серверу }
        f_Login := '';
        if notEmpty (4,anArgs) then
        begin
            f_Login := toString (anArgs [4]);
        end;
        if isEmpty (Login) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_LOGIN);
        { шестой параметр - пароль к smtp-серверу }
        f_Password := '';
        if notEmpty (5,anArgs) then
        begin
            f_Password := toString (anArgs [5]);
        end;
        { седьмой параметр - AutoTLS }
        f_AutoTLS := FALSE;
        if notEmpty (6,anArgs) then
        begin
            f_AutoTLS := toBoolean (anArgs [6]);
        end;
        { восьмой параметр - FullSSL }
        f_FullSSL := FALSE;
        if notEmpty (7,anArgs) then
        begin
            f_FullSSL := toBoolean (anArgs [7]);
        end;
        { девятый параметр - ListView
          лог почты }
        f_ListView := NIL;
        if notEmpty (8,anArgs) then
        begin
            OBJ := toObject (anArgs [8]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TsListView) ) then
                f_ListView := TsListView (OBJ);
        end;
        { десятый параметр - Gauge
          показатель прогресса загрузки }
        f_Gauge := NIL;
        if notEmpty (9,anArgs) then
        begin
            OBJ := toObject (anArgs [9]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TControl) ) then
                f_Gauge := TGauge (OBJ);
        end;
        { одиннадцатый параметр - StatusPanel
          показатель прогресса загрузки }
        f_StatusPanel := NIL;
        if notEmpty (10,anArgs) then
        begin
            OBJ := toObject (anArgs [10]);
            if ( Assigned (OBJ) and OBJ.InheritsFrom (TStatusPanel) ) then
                f_StatusPanel := TStatusPanel (OBJ);
        end;
        { двенадцатый параметр - время ожидания ms }
        f_TimeOut := 120000;
        if notEmpty (11,anArgs) then
        begin
            f_TimeOut := toInteger (anArgs [11]);
        end;
        if ( TimeOut = 0 ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_TIMEOUT);
        { тринадцатый параметр - хост proxy-сервера }
        f_ProxyHost := '';
        if notEmpty (12,anArgs) then
        begin
            f_ProxyHost := toString (anArgs [12]);
        end;
        if ( ProxyHost <> '' ) and not isIPAddress (ProxyHost) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PROXY_HOST);
        { четырнадцатый параметр - порт proxy-сервера }
        f_ProxyPort := 0;
        if notEmpty (13,anArgs) then
        begin
            f_ProxyPort := toInteger (anArgs [13]);
        end;
        if ( ProxyPort <= 0 ) and ( ProxyHost <> '' ) then
            raise Exception.Create (ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_PROXY_PORT);
        { пятнадцатый параметр - логин proxy-сервера }
        f_ProxyLogin := '';
        if notEmpty (14,anArgs) then
        begin
            f_ProxyLogin := toString (anArgs [14]);
        end;
        { шестнадцатый параметр - пароль proxy-сервера }
        f_ProxyPassword := '';
        if notEmpty (15,anArgs) then
        begin
            f_ProxyPassword := toString (anArgs [15]);
        end;
        { семнадцатый параметр - протокол proxy-сервера }
        f_ProxyProtocol := '';
        if notEmpty (16,anArgs) then
        begin
            f_ProxyProtocol := toString (anArgs [16]);
        end;
        { список пакетов }
        f_Packages := NIL;
        f_PackageIndex := -1;
        { клиент }
        f_SMTP := TSMTPClient.Create ([ Host, Port,
                                        Login, Password,
                                        ProxyHost, ProxyPort,
                                        ProxyLogin, ProxyPassword,
                                        ProxyProtocol,
                                        TimeOut,
                                        AutoTLS, FullSSL ]);
        { время ожидания }
        f_IdleTime := -1;
        { лог }
        f_Log := TItems.Create ([]);
    except on E: Exception do
        _raise ([ 'Create', ERR_TSMTPPACKAGECLIENTTHREAD_CREATE, E, Exception (FatalException) ],
                ['{9174C67C-2E79-4342-9563-93F462036A7D}']);
    end;
end;


destructor TSMTPPackageClientThread.Destroy;
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
        FreeAndNil (f_Log);
        FreeAndNil (f_SMTP);
        if ( ReceiverID <> USER_ID ) and ( ReceiverID <> 0 ) then
            FreeAndNil (f_Receiver);
        FreeAndNil (f_Packages);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TSMTPPACKAGECLIENTTHREAD_DESTROY, E, Exception (FatalException) ],
                ['{4A5A71BA-9338-42E8-B63D-AE0664CC6B07}']);
    end;
end;

procedure TSMTPPackageClientThread.Main;
var
    Stream : TStream;
begin
    try
        if Terminated then Exit;
        inherited Main;
        if not Assigned (Packages) then
        try
            f_Packages := TPackages.Load (DB,[ _([]),
                                               _([{USER_KEY_HASH}]),
                                               _([Receiver.KeyHash]),
                                               _([USER_ID]),
                                               _([]),
                                               _([PACKAGE_CREATED_STATUS_ID]),
                                               _([ TUser, {TPic,} TMessage, TCategorie, TMetaObject ]) ],
                                             [ _pck_time_stamp_create ]) as TPackages;
            MaxProgress := Packages.Count;
            Progress := 0;
        except
            FreeAndNil (f_Packages);
        end;
        // делаем здесь намеренно задержку, чтобы не перегружать сеть
        // просто возвращаем управление в основной поток
        Sleep (1);
        if not Assigned (Packages) or ( Packages.Count <= 0 ) then
            Terminate
        else if ( PackageIndex >= Packages.Count-1 ) then
            Terminate
        else
        try
            { ожидаем }
            IdleTime := IdleTime +1;
            if ( IdleTime >= TimeOut div 10 ) then
                IdleTime := -1
            else
                Exit;
            { после ожидания - обрабатываем следующий пакет }
            PackageIndex := PackageIndex + 1;
            { прогресс }
            Progress := Progress + 1;
            { записываем пакет }
            if Assigned (Packages.ItemAt [PackageIndex]) then
            begin
                SMTP.Clear;
                { отправитель }
                SMTP.Sender := Login;
                { получатель }
                SMTP.Receivers.Add (Receiver.EMail);
                { тема - хэш-ключ пакета }
                SMTP.Subject := Packages.ItemAt [PackageIndex].KeyHash;
                { тело пакета }
                Stream := TMemoryStream.Create;
                try
                    Packages.ItemAt [PackageIndex].SaveToStream (Stream);
                    SMTP.Data.Position := 0;
                    WriteStreamL (SMTP.Data,Stream);
                finally
                    FreeAndNil (Stream);
                end;
                { отправляем пакет }
                if SMTP.Send then
                try
                    { меняем статус пакета }
                    Packages.ItemAt [PackageIndex].IDStatus := PACKAGE_SENDED_STATUS_ID;
                    Packages.ItemAt [PackageIndex].Save;
                    { пишем в лог статус пакета }
                    WriteLog ( Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetLocalSinIP,
                                         SMTP.SMTP.Sock.GetLocalSinPort ]),
                               Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetRemoteSinIP,
                                         SMTP.SMTP.Sock.GetRemoteSinPort ]),
                               Format (MSG_TSMTPPACKAGE_SENDED_PACKAGE,
                                       [ Packages.ItemAt [PackageIndex].KeyHash ]),
                               pckSend,
                               Packages.ItemAt [PackageIndex].PckType.IDExternal,
                               Packages.ItemAt [PackageIndex].MetaClass.GetClassID );
                except on E: Exception do
                    { пишем в лог причину ошибки }
                    WriteLog ( Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetLocalSinIP,
                                         SMTP.SMTP.Sock.GetLocalSinPort ]),
                               Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetRemoteSinIP,
                                         SMTP.SMTP.Sock.GetRemoteSinPort ]),
                               Format ('%s : %s',
                                       [ ERR_TSMTPPACKAGECLIENTTHREAD_INVALID_DATA,
                                         E.Message ]),
                               pckError );
                end
                else
                    WriteLog ( Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetLocalSinIP,
                                         SMTP.SMTP.Sock.GetLocalSinPort ]),
                               Format ('%s:%d',
                                       [ SMTP.SMTP.Sock.GetRemoteSinIP,
                                         SMTP.SMTP.Sock.GetRemoteSinPort ]),
                               Format (MSG_TSMTPPACKAGE_SEND_ERROR,
                                       [ SMTP.SMTP.ResultCode ]),
                               pckError );
            end;
        except on Error: Exception do
            WriteStatus (Error.Message);
        end;
    except on E: Exception do
        _raise ([ 'Main', ERR_TSMTPPACKAGECLIENTTHREAD_MAIN, E, Exception (FatalException) ],
                ['{5C2304B4-064F-41E4-B1F4-E62E8A32F063}']);
    end;
end;

procedure TSMTPPackageClientThread.Return;
var
    Itm        : TListItem;
    ImageIndex : Integer;
begin
    try
        inherited Return;
        if Assigned (ListView) and Assigned (Log) then
        begin
            while Log.Count > 0 do
            begin
                Itm := ListView.Items.Add;
                Itm.Caption := PLogRecord (Log.Item [0])^.Caption;
                Itm.ImageIndex := PLogRecord (Log.Item [0])^.ImageIndex;
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Sender );
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Receiver );
                Itm.SubItems.Add ( PLogRecord (Log.Item [0])^.Message );
                Dispose ( PLogRecord (Log.Item [0]) );
                Log.Delete (0);
            end;
        end;
        if Assigned (StatusPanel) and notEmpty (Status) then
        begin
            StatusPanel.Text := Status;
        end;
        if Assigned (Gauge) then
        begin
            Gauge.MinValue := 0;
            Gauge.MaxValue := MaxProgress;
            Gauge.Progress := Progress;
        end;
        ProcessMessages;
    except on E: Exception do
        _raise ([ 'Return', ERR_TSMTPPACKAGECLIENTTHREAD_RETURN, E, Exception (FatalException) ],
                ['{90BA460C-22DC-441D-869F-EEED8E58F321}']);
    end;
end;


procedure TSMTPPackageClientThread.WriteStatus (const aMessage: String);
begin
    try
        Status := aMessage;
    except on E: Exception do
        _raise (['WriteStatus',ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_STATUS,E],
                ['{59A32DEC-8F90-484B-BE3B-1F0865639758}']);
    end;
end;

procedure TSMTPPackageClientThread.WriteStatus (const aMessage: String;
                                                aParams: array of const);
begin
    try
        WriteStatus ( Format (aMessage,aParams) );
    except on E: Exception do
        _raise (['WriteStatus',ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_STATUS,E],
                ['{4805A25C-377F-4A7D-A77A-FF54BBDE3152}']);
    end;
end;

procedure TSMTPPackageClientThread.WriteLog (const aSender: String;
                                             const aReceiver: String;
                                             const aMessage: String;
                                             const anImageIndex: Integer = pckDefault;
                                             const aPackageType: String = '';
                                             const aMetaClassID: String = '');
var
    Rec : PLogRecord;
begin
    try
        if Assigned (Log) then
        begin
            Rec := AllocMem ( SizeOf (TLogRecord) + 1 );
            Rec^.Caption := _DateTimeToStr (now);
            Rec^.ImageIndex := anImageIndex;
            Rec^.Sender := aSender;
            Rec^.Receiver := aReceiver;
            if notEmpty (aMetaClassID) then
                Rec^.Message := Format ('%s : %s : %s',[aMetaClassID,aPackageType,aMessage])
            else
                Rec^.Message := aMessage;
            Log.Add (Rec);
        end;
    except on E: Exception do
        _raise (['WriteLog',ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_LOG,E],
                ['{99585726-2FEB-4526-98C9-E1041206208B}']);
    end;
end;

procedure TSMTPPackageClientThread.WriteLog (const aSender: String;
                                             const aReceiver: String;
                                             const aMessage: String;
                                             aParams: array of const;
                                             const anImageIndex: Integer = pckDefault;
                                             const aPackageType: String = '';
                                             const aMetaClassID: String = '');
begin
    try
        WriteLog ( aSender, aReceiver, Format (aMessage,aParams), anImageIndex, aPackageType, aMetaClassID );
    except on E: Exception do
        _raise (['WriteLog',ERR_TSMTPPACKAGECLIENTTHREAD_WRITE_LOG,E],
                ['{80E318A5-1959-423B-A7B9-970A39E738AB}']);
    end;
end;


end.
