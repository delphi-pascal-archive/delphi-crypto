unit DLLThreads;
{******************************************************************************}
{*  Unit for use Threads in Dinamic Link Library                              *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2010                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, Messages, SysUtils, Classes, Variants,
{ utils }
    DateUtils, Utils, Strings, VarRecs, Versions, EClasses,
{ kernel }
    Kernel;

{ сообщение синхронизации }
const
    WM_THREAD_SYNCHRONIZE = WM_USER + 1024;
    
type
{ ошибка потока }
{$M+}
    EThreadError = class (EClass) end;
{$M-}

{ предекларация потока }
{$M+}
    TDllThread = class;
{$M-}

{ метод потока }
    PDllThreadMethod = ^TDllThreadMethod;
    TDllThreadMethod = procedure of object;
{ внешний метод потока }
    PDllThreadExternalMethod = ^TDllThreadExternalMethod;
    TDllThreadExternalMethod = procedure (Sender: TDllThread);

{ предекларация синхронизированного списка потоков }
{$M+}
    TDllThreads = class;
{$M-}

{ поток }
{$M+}
    CDllThread = class of TDllThread;
    PDllThread = ^TDllThread;
    TDllThread = class (TThread)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Owner: TDllThreads;                 { владелец - синхронизированный список }
        f_Name: String;                       { наименование потока }
        f_OnMain: TDllThreadExternalMethod;   { внешняя реализация главной функции потока }
        f_OnReturn: TDllThreadExternalMethod; { внешняя реализация функции возврата в поток-родитель }
    protected
        class function toThreadPriority (const aValue: TVarRec) : TThreadPriority; virtual;
    protected
        procedure SetOwner (const aValue: TDllThreads);
        procedure SetOnMain (const aValue: Pointer);
        procedure SetOnReturn (const aValue: Pointer);
        procedure Execute; override;
        procedure DoSynchronize;
    public
        constructor Create (anArgs: array of const); virtual;
        destructor Destroy; override;
        procedure Main; virtual;
        procedure Return; virtual;
        property Owner: TDllThreads read f_Owner write SetOwner default NIL;
        property Name: String read f_Name write f_Name;
        property OnMain: TDllThreadExternalMethod read f_OnMain write f_OnMain default NIL;
        property OnReturn: TDllThreadExternalMethod read f_OnReturn write f_OnReturn default NIL;
    end;
{$M-}

{ синхронизированный список потоков }
{$M+}
    CDllThreads = class of TDllThreads;
    PDllThreads = ^TDllThreads;
    TDllThreads = class (TItems)
    private
        f_Terminated: Boolean; { признак завершения работы потоков списка }
        WC: TWndClassEx;       { класс окна - синхронизатора }
        f_Handle: HWND;        { дискриптор окна - синхронизатора }
    protected
        procedure WMSynchronize (var Message: TMessage); message WM_THREAD_SYNCHRONIZE;
    protected
        function Check (anItem: Pointer) : Boolean; override;
        function Check (anItem: TDllThread) : Boolean; overload; virtual;
    protected
        function GetItemAt (anIndex: Integer) : TDllThread; overload; virtual;
        function GetItemOf (aName: String) : TDllThread; overload; virtual;
        procedure SetItemAt (anIndex: Integer; anItem: TDllThread); overload; virtual;
        procedure SetItemOf (aName: String; anItem: TDllThread); overload; virtual;
    public
        constructor Create; override;
        destructor Destroy; override;
    protected
        class function ClassName : PAnsiChar; overload;
        class function ItemClassType : CDllThread; virtual;
    public
        function Add (anItem: TDllThread) : Integer; overload; virtual;
        function Add (anArgs: array of const) : Integer; override;
        procedure Clear (doFree: Boolean = TRUE); override;
    public
        procedure Resume; virtual;
        procedure Suspend; virtual;
        procedure Terminate; virtual;
    public
        property ItemAt [anIndex: Integer]: TDllThread read GetItemAt write SetItemAt default NIL; default;
        property ItemOf [aName: String]: TDllThread read GetItemOf write SetItemOf;
        property Terminated: Boolean read f_Terminated default FALSE;
        property Handle: HWND read f_Handle default 0;
    end;
{$M-}

{ TThreadPriority }
const
    TP_UNKNOWN       = 0;
    TP_IDLE          = 1;
    TP_LOWEST        = 2;
    TP_LOWER         = 3;
    TP_NORMAL        = 4;
    TP_HIGHER        = 5;
    TP_HIGHEST       = 6;
    TP_TIME_CRITICAL = 7;

{ ProcessMessages }
function ProcessMessage (var Msg: TMsg) : Boolean;
procedure ProcessMessages;

{ TDllThread Errors }
resourcestring
    ERR_TDLL_THREAD_CREATE           = 'Ошибка создания потока!';
    ERR_TDLL_THREAD_DESTROY          = 'Ошибка уничтожения потока!';
    ERR_TDLL_THREAD_MAIN             = 'Ошибка главной функции потока!';
    ERR_TDLL_THREAD_RETURN           = 'Ошибка функции возврата потока!';
    ERR_TDLL_THREAD_DO_SYNCHRONIZE   = 'Ошибка обработки события синхронизации потока!';
    ERR_TDLL_THREAD_GET_OWNER_HANDLE = 'Не указан дискриптор синхронизатора!';
    ERR_TDLL_THREAD_EXECUTE          = 'Ошибка выполнения потока!';
    ERR_TDLL_THREAD_SET_OWNER        = 'Ошибка установки владельца потока!';
    ERR_TDLL_THREAD_SET_ON_MAIN      = 'Ошибка установки внешней основной функции потока!';
    ERR_TDLL_THREAD_SET_ON_RETURN    = 'Ошибка установки внешней функции возврата потока!';

{ TDllThreads Errors }
resourcestring
    ERR_TDLLTHREADS_CREATE               = 'Ошибка создания списка потоков!';
    ERR_TDLLTHREADS_GET_HANDLE           = 'Ошибка получения дискриптора синхронизатора!';
    ERR_TDLLTHREADS_DESTROY              = 'Ошибка уничтожения списка потоков!';
    ERR_TDLLTHREADS_GET_CLASS_NAME       = 'Ошибка чтения имени класса!';
    ERR_TDLLTHREADS_WMSYNCHRONIZE_WPARAM = 'Message.WParam = ''%d'' не содержит ссылку на экземпляр класса %s!';
    ERR_TDLLTHREADS_WMSYNCHRONIZE        = 'Ошибка синхронизации потоков списка!';
    ERR_TDLLTHREADS_CHECK                = 'Ошибка валидации потока!';
    ERR_TDLLTHREADS_GET_ITEM_AT          = 'Ошибка чтения потока из списка!';
    ERR_TDLLTHREADS_GET_ITEM_OF          = 'Ошибка чтения потока из списка!';
    ERR_TDLLTHREADS_SET_ITEM_AT          = 'Ошибка записи потока в список!';
    ERR_TDLLTHREADS_SET_ITEM_OF          = 'Ошибка записи потока в список!';
    ERR_TDLLTHREADS_GET_COUNT            = 'Ошибка подсчета количества элементов в списке потоков!';
    ERR_TDLLTHREADS_ADD                  = 'Ошибка добавления потока в список!';
    ERR_TDLLTHREADS_ADD_WITH_CREATE      = 'Ошибка создания потока в списке!';
    ERR_TDLLTHREADS_NOT_FOUND            = 'Поток ''%s'' не найден в списке!';
    ERR_TDLLTHREADS_EQUALS               = 'Ошибка сравнения списков потоков!';
    ERR_TDLLTHREADS_RESUME               = 'Ошибка возобновления работы потоков списка!';
    ERR_TDLLTHREADS_SUSPEND              = 'Ошибка приостановки работы потоков списка!';
    ERR_TDLLTHREADS_TERMINATE            = 'Ошибка завершения потоков списка!';
    ERR_TDLLTHREADS_CLEAR                = 'Ошибка уничтожения потоков списка!';

implementation

{ TDllThread }
class procedure TDllThread._raise (anArgs: array of const;
                                   const anEGUID: String = '');
begin
    raise EThreadError.Create ( _([self],anArgs), anEGUID );
end;

class procedure TDllThread._raise (anArgs: array of const;
                                   anEGUID: array of const);
begin
    raise EThreadError.Create ( _([self],anArgs), anEGUID );
end;

constructor TDllThread.Create (anArgs: array of const);
var
    CreateSuspended : Boolean;
begin
    try
        { первый параметр - CreateSuspended : Boolean
          указывает на необходимость создания "простаивающего" потока,
          для запуска такого потока следует вызвать Resume }
        CreateSuspended := FALSE;
        if notEmpty (0,anArgs) then
            CreateSuspended := toBoolean (anArgs [0]);
        inherited Create (CreateSuspended);
        { второй параметр - FreeOnTerminate : Boolean
          указывает потоку на необходимость самоуничтожения
          после завершения работы }
        FreeOnTerminate := FALSE;
        if notEmpty (1,anArgs) then
            FreeOnTerminate := toBoolean (anArgs [1]);
        { третий параметр - Priority : TThreadPriority
          указывает на приоритет созданного потока }
        Priority := tpNormal;
        if notEmpty (2,anArgs) then
            Priority := toThreadPriority (anArgs [2]);
        { четвертый параметр - OnMain : TDllThreadExternalMethod
          выполнение действий внутри потока }
        f_OnMain := NIL;
        if notEmpty (3,anArgs) then
            SetOnMain ( toPointer (anArgs [3]) );
        { пятый параметр - OnReturn : TDllThreadExternalMethod
          выполнение действий в контексте потока-родителя }
        f_OnReturn := NIL;
        if notEmpty (4,anArgs) then
            SetOnReturn ( toPointer (anArgs [4]) );
        { шестой параметр - Name : String
          наименование потока }
        f_Name := '';
        if notEmpty (5,anArgs) then
            f_Name := toString (anArgs [5]);
    except on E: Exception do
        _raise ([ 'Create', ERR_TDLL_THREAD_CREATE, E, Exception (FatalException) ],
                ['{14C97B56-15A9-4905-9E83-40CF1CABEE1E}']);
    end;
end;

destructor TDllThread.Destroy;
begin
    try
        try
            if Assigned (Owner) and ( Owner.Count > 0 ) then
                Owner.Delete ( Owner.Index [self] );
        finally
            inherited Destroy;
        end;
    except on E: Exception do
        _raise ([ 'Destroy', ERR_TDLL_THREAD_DESTROY, E, Exception (FatalException) ],
                ['{CD33F273-D15D-4315-9425-57B7128D9E47}']);
    end;
end;

class function TDllThread.toThreadPriority (const aValue: TVarRec) : TThreadPriority;
const
    THREAD_PRIORITY_INT  : array [TP_UNKNOWN..TP_TIME_CRITICAL] of String = (
        '',
        'tpIdle',
        'tpLowest',
        'tpLower',
        'tpNormal',
        'tpHigher',
        'tpHighest',
        'tpTimeCritical'
    );
var
    I : Integer;
    s : String;
begin
    Result := tpNormal;

    I := toInteger (aValue);
    case I of
        TP_IDLE          : Result := tpIdle;
        TP_LOWEST        : Result := tpLowest;
        TP_LOWER         : Result := tpLower;
        TP_NORMAL        : Result := tpNormal;
        TP_HIGHER        : Result := tpHigher;
        TP_HIGHEST       : Result := tpHighest;
        TP_TIME_CRITICAL : Result := tpTimeCritical;
    else
        begin
            s := toString (aValue);
            if      ( s = THREAD_PRIORITY_INT [TP_IDLE] ) then
                Result := tpIdle
            else if ( s = THREAD_PRIORITY_INT [TP_LOWEST] ) then
                Result := tpLowest
            else if ( s = THREAD_PRIORITY_INT [TP_LOWER] ) then
                Result := tpLower
            else if ( s = THREAD_PRIORITY_INT [TP_NORMAL] ) then
                Result := tpNormal
            else if ( s = THREAD_PRIORITY_INT [TP_HIGHER] ) then
                Result := tpHigher
            else if ( s = THREAD_PRIORITY_INT [TP_HIGHEST] ) then
                Result := tpHighest
            else if ( s = THREAD_PRIORITY_INT [TP_TIME_CRITICAL] ) then
                Result := tpTimeCritical;
        end;
    end;
end;

procedure TDllThread.Main;
begin
    try
        if Terminated then Exit;
        if Assigned (f_OnMain) then
            f_OnMain (self);
    except on E: Exception do
        _raise ([ 'Main', ERR_TDLL_THREAD_MAIN, E, Exception (FatalException) ],
                ['{11BC90F8-8983-4DB7-9313-678E8A022ADD}']);
    end;
end;

procedure TDllThread.Return;
begin
    try
        if Terminated then Exit;
        if Assigned (f_OnReturn) then
            f_OnReturn (self);
    except on E: Exception do
        _raise ([ 'Return', ERR_TDLL_THREAD_RETURN, E, Exception (FatalException) ],
                ['{B6FDB927-880F-4A13-83B0-EDB6B26ACC39}']);
    end;
end;

procedure TDllThread.DoSynchronize;
begin
    try
        if not (Terminated or Suspended) then
        begin
            Return;
            ProcessMessages;
        end;
    except on E: Exception do
        _raise ([ 'DoSynchronize', ERR_TDLL_THREAD_DO_SYNCHRONIZE, E, Exception (FatalException) ],
                ['{2F8E3F93-5195-476C-8B0B-762D11A138C5}']);
    end;
end;

procedure TDllThread.Execute;
begin
    try
        repeat
            Main;
            if ( not isLibrary ) then
                Synchronize (DoSynchronize)
            else if ( Assigned (f_Owner) and (f_Owner.Handle > 0) ) then
                SendMessageW ( f_Owner.Handle, WM_THREAD_SYNCHRONIZE, LongInt (self), 0)
            else
                raise EInvalidOperation.Create (ERR_TDLL_THREAD_GET_OWNER_HANDLE);
            ProcessMessages;
        until Terminated;
    except on E: Exception do
        _raise ([ 'Execute', ERR_TDLL_THREAD_EXECUTE, E, Exception (FatalException) ],
                ['{6A38CFD1-31BB-482C-A57A-6E4A02AAD300}']);
    end;
end;

procedure TDllThread.SetOwner (const aValue: TDllThreads);
begin
    try
        if Assigned (aValue) then
            f_Owner := aValue;
    except on E: Exception do
        _raise ([ 'SetOwner', ERR_TDLL_THREAD_SET_OWNER, E, Exception (FatalException) ],
                ['{540A313D-7C81-4FE8-A479-DB8BCCF27E79}']);
    end;
end;

procedure TDllThread.SetOnMain (const aValue: Pointer);
var
    Method : TDllThreadExternalMethod;
begin
    try
        try
            @Method := NIL;
            try @Method := aValue; except @Method := NIL; end;
        finally
            if Assigned (Method) then
                @f_OnMain := aValue;
        end;
    except on E: Exception do
        _raise ([ 'SetOnMain', ERR_TDLL_THREAD_SET_ON_MAIN, E, Exception (FatalException) ],
                ['{59CADACE-227D-460A-B076-D95797DC0B9E}']);
    end;
end;

procedure TDllThread.SetOnReturn (const aValue: Pointer);
var
    Method : TDllThreadExternalMethod;
begin
    try
        try
            @Method := NIL;
            try @Method := aValue; except @Method := NIL; end;
        finally
            if Assigned (Method) then
                @f_OnReturn := aValue;
        end;
    except on E: Exception do
        _raise ([ 'SetOnReturn', ERR_TDLL_THREAD_SET_ON_RETURN, E, Exception (FatalException) ],
                ['{DB0B7973-1810-491C-A925-1DEC145215DB}']);
    end;
end;

{ TDllThreads }
constructor TDllThreads.Create;
var
    M : TMemoryBasicInformation;

    { обработчик событий синхронизирующего окна }
    function WindowProc (WND: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam) : LResult; stdcall;
    var
        Message : TMessage;
    begin
        case Msg of
            WM_DESTROY:
            begin
                Result := 0;
                PostQuitMessage (0);
            end;
            WM_THREAD_SYNCHRONIZE:
            begin
                Result := 0;
                Message.Msg := Msg;
                Message.WParam := WParam;
                Message.LParam := LParam;
                Message.Result := Result;
                WMSynchronize (Message);
            end;
            else Result := DefWindowProc (WND,Msg,WParam,LParam);
        end;
        ProcessMessages;
    end;

begin
    try
        inherited Create;
        f_Terminated := FALSE;
        f_Handle := 0;
        if isLibrary then
        begin
            { создаем синхронизирующее окно }
            WC.cbSize := SizeOf (WC);
            WC.style := CS_HREDRAW or CS_VREDRAW;
            WC.lpfnWndProc := @WindowProc;
            WC.cbClsExtra := 0;
            WC.cbWndExtra := 0;
            WC.hInstance := hModule (M.AllocationBase); 
            WC.hIcon := 0;
            WC.hCursor := 0;
            WC.hbrBackground := COLOR_BTNFACE+1;
            WC.lpszMenuName := NIL;
            WC.lpszClassName := self.ClassName;
            RegisterClassEx (WC);
            f_Handle := CreateWindowEx ( 0,
                                         WC.lpszClassName,
                                         PChar (  Format ('%s%d',[ WC.lpszClassName, Longint (self) ])  ),
                                         WS_OVERLAPPEDWINDOW,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         WC.hInstance,
                                         NIL );
            if (f_Handle <= 0) then
                raise EInvalidOperation.Create (ERR_TDLLTHREADS_GET_HANDLE);
        end;
    except on E: Exception do
        _raise (['Create',ERR_TDLLTHREADS_CREATE,E],
                ['{07C3E5AF-5FBA-4198-BBA7-E7467E995720}']);
    end;
end;

destructor TDllThreads.Destroy;
begin
    try
        Clear;
        if ( f_Handle > 0 ) then
            SendMessageW ( f_Handle, WM_DESTROY, Longint (self), 0);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TDLLTHREADS_DESTROY,E],
                ['{781EE968-05B2-424D-AF80-E19F33D5B01F}']);
    end;
end;

class function TDllThreads.ClassName : PAnsiChar;
var
    I    : Integer;
    ansi : array [0..SizeOf (ShortString)] of Char;
    str  : ShortString;
begin
    Result := #0;
    try
        str := inherited ClassName;
        for I := 1 to Length (str) do
            ansi [I-1] := str [I];
        for I := Length (str) to SizeOf (ShortString) do
            ansi [I-1] := #0;
        Result := ansi;
    except on E: Exception do
        _raise (['ClassName',ERR_TDLLTHREADS_GET_CLASS_NAME,E],
                ['{9CFF0BFE-CF9D-4139-8498-CFE7EF7261A7}']);
    end;
end;

procedure TDllThreads.WMSynchronize (var Message: TMessage);
var
    OBJ : TObject;
begin
    try
        OBJ := TObject (Message.WParam);
        if Check (OBJ) then
            TDllThread (OBJ).DoSynchronize
        else
            raise Exception.CreateFmt (ERR_TDLLTHREADS_WMSYNCHRONIZE_WPARAM,
                                       [Message.WParam,TDllThread.ClassName]);
    except on E: Exception do
        _raise (['WMSynchronize',ERR_TDLLTHREADS_WMSYNCHRONIZE,E],
                ['{4DB215F6-0217-4693-857D-A4F5AF0F0499}']);
    end;
end;

function TDllThreads.Check (anItem: Pointer) : Boolean;
begin
    Result := FALSE;
    try
        Result := inherited Check (anItem) and
                  TObject (anItem).InheritsFrom (TDllThread) and
                  Check ( TDllThread (anItem) );
    except on E: Exception do
        _raise (['Check',ERR_TDLLTHREADS_CHECK,E],
                ['{BC71F19B-0385-44BE-AD50-FD2BD00D9049}']);
    end;
end;

function TDllThreads.Check (anItem: TDllThread) : Boolean;
begin
    Result := TRUE;
    try
        Result := TRUE;
    except on E: Exception do
        _raise ([ 'Check', ERR_TDLLTHREADS_CHECK, E, Exception (anItem.FatalException) ],
                ['{30442236-E6E8-4444-A78D-D25D344ED65D}']);
    end;
end;

function TDllThreads.GetItemAt (anIndex: Integer) : TDllThread;
begin
    Result := NIL;
    try
        Result := TDllThread (inherited Item [anIndex]);
    except on E: Exception do
        _raise ([ Format ('GetItemAt [%d]',[anIndex]), ERR_TDLLTHREADS_GET_ITEM_AT, E, Exception (Result.FatalException) ],
                ['{286CE744-594A-4D29-AD0D-013733B68866}']);
    end;
end;

procedure TDllThreads.SetItemAt (anIndex: Integer; anItem: TDllThread);
begin
    try
        if Check (anItem) then
        begin
            inherited Item [anIndex] := anItem;
            anItem.Owner := self;
        end;
    except on E: Exception do
        _raise ([ Format ('SetItemAt [%d]',[anIndex]), ERR_TDLLTHREADS_SET_ITEM_AT, E, Exception (anItem.FatalException) ],
                ['{4E7C221D-0165-445E-B618-CBEE833A0F15}']);
    end;
end;

function TDllThreads.GetItemOf (aName: String) : TDllThread;
var
    I     : Integer;
    Found : Boolean;
begin
    Result := NIL;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if Assigned ( TDllThread (Item [I]) ) and
               (  UpperCase ( TDllThread (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Result := TDllThread (Item [I]);
                Found := TRUE;
                Break;
            end;
        {if not Found then
            raise Exception.CreateFmt (ERR_TDLLTHREADS_NOT_FOUND,[aName]);}
    except on E: Exception do
        _raise ([ Format ('GetItemOf [''%s'']',[aName]), ERR_TDLLTHREADS_GET_ITEM_OF, E, Exception (Result.FatalException) ],
                ['{68B2A3D1-A5A0-4BF9-BFA5-5912A8752A64}']);
    end;
end;

procedure TDllThreads.SetItemOf (aName: String; anItem: TDllThread);
var
    I     : Integer;
    Found : Boolean;
begin
    Found := FALSE;
    try
        Found := FALSE;
        for I := 0 to Count - 1 do
            if Assigned ( TDllThread (Item [I]) ) and
               (  UpperCase ( TDllThread (Item [I]).Name ) = UpperCase (aName)  ) then
            begin
                Item [I] := anItem;
                Found := TRUE;
                Break;
            end;
        if not Found then
            raise Exception.CreateFmt (ERR_TDLLTHREADS_NOT_FOUND,[aName]);
    except on E: Exception do
        _raise ([ Format ('SetItemOf [''%s'']',[aName]), ERR_TDLLTHREADS_SET_ITEM_OF, E, Exception (anItem.FatalException) ],
                ['{3BE3039F-A226-4BBF-90E1-0AEE54235A9C}']);
    end;
end;

class function TDllThreads.ItemClassType : CDllThread;
begin
    Result := TDllThread;
end;

function TDllThreads.Add (anItem: TDllThread) : Integer;
begin
    Result := -1;
    try
        if Check (anItem) then
        begin
            Result := inherited Add (anItem);
            anItem.Owner := self;
        end;
    except on E: Exception do
        _raise ([ 'Add', ERR_TDLLTHREADS_ADD, E, Exception (anItem.FatalException) ],
                ['{B2C4E68C-F75F-476F-B83F-B47BA39EB0C4}']);
    end;
end;

function TDllThreads.Add (anArgs: array of const) : Integer;
var
    Thread : TDllThread;
begin
    Result := -1;
    try
        Thread := ItemClassType.Create (anArgs);
        if Assigned (Thread) then
        begin
            Result := Add (Thread);
            if (Result < 0) then
                FreeAndNil (Thread);
        end;
    except on E: Exception do
        _raise ([ 'Add', ERR_TDLLTHREADS_ADD_WITH_CREATE, E, Exception (Thread.FatalException) ],
                ['{4066BE22-7EEA-4974-B8BC-B167E592F287}']);
    end;
end;

procedure TDllThreads.Resume;
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) then
                ItemAt [I].Resume
            else
                Delete (I);
    except on E: Exception do
        _raise (['Resume',ERR_TDLLTHREADS_RESUME,E],
                ['{8FA6C5C0-17FE-4B8D-B739-1FADAE0697AD}']);
    end;
end;

procedure TDllThreads.Suspend;
var
    I : Integer;
begin
    try
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) then
                ItemAt [I].Suspend
            else
                Delete (I);
    except on E: Exception do
        _raise (['Suspend',ERR_TDLLTHREADS_SUSPEND,E],
                ['{2A4636CF-C837-4F6C-A5A6-59DED7000FA6}']);
    end;
end;

procedure TDllThreads.Terminate;
var
    I : Integer;
begin
    try
        f_Terminated := TRUE;
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) then
                ItemAt [I].Terminate
            else
                Delete (I);
    except on E: Exception do
        _raise (['Terminate',ERR_TDLLTHREADS_TERMINATE,E],
                ['{858116B4-5FEE-4C76-9831-BA3144CE5FAA}']);
    end;
end;

procedure TDllThreads.Clear (doFree: Boolean = TRUE);
var
    I : Integer;
begin
    try
        f_Terminated := TRUE;
        for I := 0 to Count - 1 do
            if Assigned (ItemAt [I]) then
            begin
                ItemAt [I].FreeOnTerminate := doFree;
                ItemAt [I].Terminate;
            end;
        inherited Clear (FALSE);
    except on E: Exception do
        _raise (['Clear',ERR_TDLLTHREADS_CLEAR,E],
                ['{67588EBA-D45C-41DE-808D-9258CD70DF81}']);
    end;
end;

{ ProcessMessages }
function ProcessMessage (var Msg: TMsg) : Boolean;
begin
    Result := FALSE;
    if PeekMessage (Msg, 0, 0, 0, PM_REMOVE) then
    begin
        Result := TRUE;
        if ( Msg.Message <> WM_QUIT ) then
        begin
            TranslateMessage (Msg);
            DispatchMessage (Msg);
        end;
    end;
end;

procedure ProcessMessages;
var
    Msg : TMsg;
begin
    while ProcessMessage (Msg) do {loop};
end;


end.
