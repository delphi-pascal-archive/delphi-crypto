unit uLoginDialog;
{******************************************************************************}
{*  Login Dialog Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Controls, Forms,
    Kernel, DialogClasses,
    uProtoDialog, uMetaDialog,
    Utils, Strings, Versions, VarRecs,
    EClasses,
    SQLite3, SQLite3DLL, SQLiteTable3;

{ кнопки }
const
    btLogin    = 0;
    btRegister = 1;
    btCancel   = 2;

{ действия }
const
    mrRegister = 12;
    mrLogin    = mrOk;

type
{ диалоговое окно авторизации }
{$M+}
    CLoginDialog = class of TLoginDialog;
    PLoginDialog = ^TLoginDialog;
    TLoginDialog = class (TMetaDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
    protected
        procedure OnFormShow (Sender: TObject); override;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure doLogin; virtual;
        procedure doRegister; virtual;
    public
        constructor Create (const aDBFileName: String;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (const aDBFileName: String;
                             anArgs: array of const) : Integer; overload; virtual;
        class function Execute (const aDBFileName: String;
                                anArgs: array of const) : Boolean; overload; virtual;
    public
        property DBFileName: String read f_DBFileName;
        property DB: TSQLiteDatabase read f_DB write f_DB;
    end;
{$M-}

resourcestring
    ERR_TLOGINDIALOG_LOGIN            = 'Ошибка авторизации!';
    ERR_TLOGINDIALOG_LOGIN_USER       = 'Ошибка авторизации пользователя ''%s''!';
    ERR_TLOGINDIALOG_REGISTER         = 'Ошибка регистрации!';
    ERR_TLOGINDIALOG_REGISTER_USER    = 'Ошибка регистрации пользователя ''%s''!';
    ERR_TLOGINDIALOG_INVALID_PASSWORD = 'Неверный пароль пользователя ''%s''!';

implementation

uses
    Engine,
    uRegisterDialog;

{ TLoginDialog }
constructor TLoginDialog.Create (const aDBFileName: String;
                                 anArgs: array of const);
begin
    try
        inherited Create ([ 'авторизация', dlgLogin ],
                          [ _(['Вход',btnOk,mrLogin]),
                            _(['Регистрация',btnCustom,mrRegister{,'alLeft'}]),
                            _(['Отмена',btnCancel,mrCancel]) ],
                          [ _(['login','логин',dtString,'']),
                            _(['password','пароль',dtPassword,'']) ]);
        maxHeight := minHeight;
        f_DBFileName := aDBFileName;
        f_DB := TSQLiteDatabase.Create (DBFileName);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{33F27F40-3873-44F6-B52D-DD2FCFB01BF9}']);
    end;
end;

destructor TLoginDialog.Destroy;
begin
    try
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{F57DF9E2-92C7-4536-A115-67F657788362}']);
    end;
end;

procedure TLoginDialog.OnFormShow (Sender: TObject);
var
    I : Integer;
begin
    try
        inherited OnFormShow (Sender);
        { устанавливаем верный порядок следования,
          пересчитываем ширину }
        for I := Buttons.Count - 1 downto 0 do
            Buttons.ItemAt [I].Resize;
    except on E: Exception do
        _raise (['OnFormShow',ERR_TPROTODIALOG_ON_SHOW,E],
                ['{6E362917-A02B-4EC1-9A94-8D016F0F0D31}']);
    end;
end;

procedure TLoginDialog.GetData (anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited GetData ([ 'авторизация', dlgLogin ],
                           [ _(['Вход',btnOk,mrLogin]),
                             _(['Регистрация',btnCustom,mrRegister{,'alLeft'}]),
                             _(['Отмена',btnCancel,mrCancel]) ],
                           [ _(['login','логин',dtString,'']),
                             _(['password','пароль',dtPassword,'']) ]);
        { уравниваем ширину, чтобы затем установить
          верный порядок следования при выравнивании }
        for I := Buttons.Count - 1 downto 0 do
            Buttons.ItemAt [I].Width := 32;
        { первый параметр - логин }
        if notEmpty (0,anArgs) then
            ValueOf ['login'] := toString (anArgs [0]);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{E308C9BF-A7D9-455A-BE55-021C77EA5170}']);
    end;
end;

procedure TLoginDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrLogin    : doLogin;
            mrRegister : doRegister;
            mrCancel   : Halt;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{B8580CCC-0BBA-4415-8C8C-0050230729B8}']);
    end;
end;

procedure TLoginDialog.doLogin;
begin
    try
        if not ( TUser.Find (DB,ValueOf ['login'],0) > 0 ) then
        begin
            //raise Exception.CreateFmt (ERR_TUSER_NOT_REGISTER,
            //                           [ ValueOf ['login'] ]);
            ShowErrorFmt (ERR_TUSER_NOT_REGISTER,
                          [ ValueOf ['login'] ]);
            Exit;
        end;
        User := TUser.Load (DB,ValueOf ['login']) as TUser;
        if not Assigned (User) or ( objLoadFailed in User.Mode ) or not ( User.ID > 0 ) then
        begin
            //raise Exception.CreateFmt (ERR_TLOGINDIALOG_LOGIN_USER,
            //                           [ ValueOf ['login'] ]);
            ShowErrorFmt (ERR_TLOGINDIALOG_LOGIN_USER,
                          [ ValueOf ['login'] ]);
            FreeAndNil (User);
            Exit;
        end;
        User.Password := ValueOf ['password'];
        if not User.CheckPassword then
        begin
            //raise Exception.CreateFmt (ERR_TLOGINDIALOG_INVALID_PASSWORD,
            //                           [ ValueOf ['login'] ]);
            ShowErrorFmt (ERR_TLOGINDIALOG_INVALID_PASSWORD,
                          [ ValueOf ['login'] ]);
            FreeAndNil (User);
            Exit;
        end;
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doLogin',ERR_TLOGINDIALOG_LOGIN,E],
                ['{97844670-ADCE-4BC7-8D7D-38269FE71B2C}']);
    end;
end;

procedure TLoginDialog.doRegister;
begin
    try
        if not TRegisterDialog.Execute ( DBFileName, [ ValueOf ['login'] ] ) then
            Exit;
        if not Assigned (User) or ( objLoadFailed in User.Mode ) or not ( User.ID > 0 ) then
            raise Exception.CreateFmt (ERR_TLOGINDIALOG_REGISTER_USER,
                                       [ ValueOf ['login'] ]);
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doRegister',ERR_TLOGINDIALOG_REGISTER,E],
                ['{47DE4124-7E82-4260-80F6-C082CA528860}']);
    end;
end;

class function TLoginDialog.Open (const aDBFileName: String;
                                  anArgs: array of const) : Integer;
begin
    try
        with Create (aDBFileName,anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{BB1B7FA1-7A8C-48D0-A619-2D3926F83C5C}']);
    end;
end;

class function TLoginDialog.Execute (const aDBFileName: String;
                                     anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{8451DE46-7C41-4759-AF6E-8D9C61DEF0BC}']);
    end;
end;


end.
