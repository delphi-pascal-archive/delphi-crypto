unit uMessageDialog;
{******************************************************************************}
{*  Message Dialog Unit                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, ImgList,
    acPNG, acAlphaImageList,
    sSkinProvider, sSkinManager,
    sPanel, sPageControl, sLabel, sButton, sBitBtn, sSpeedButton,
    sEdit, Mask, sMaskEdit, sRichEdit, sComboBoxes,
    Kernel, DialogClasses, Engine,
    uProtoDialog, uTextDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses,
    SQLite3, SQLite3DLL, SQLiteTable3;

{ кнопки }
const
    btCancel = 1;
    btDelete = 0;

{ действия }
const
    mrDelete = 15;

type
{$M+}
    TMessageDeleteDialog = class;
{$M-}

{ диалоговое окно удаления сообщения }
{$M+}
    CMessageDeleteDialog = class of TMessageDeleteDialog;
    PMessageDeleteDialog = ^TMessageDeleteDialog;
    TMessageDeleteDialog = class (TTextDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_Message: TMessage;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure doDelete; virtual;
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
        property Message : TMessage read f_Message write f_Message;
    end;
{$M-}

resourcestring
    ERR_TMESSAGEDIALOG_SAVE     = 'Ошибка сохранения сообщения!';
    ERR_TMESSAGEDIALOG_DELETE   = 'Ошибка удаления сообщения!';

resourcestring
    ERR_TMESSAGEDIALOG_CREATE   = 'Ошибка создания!';
    ERR_TMESSAGEDIALOG_DESTROY  = 'Ошибка уничтожения!';
    ERR_TMESSAGEDIALOG_GET_DATA = 'Ошибка чтения данных!';
    ERR_TMESSAGEDIALOG_SET_DATA = 'Ошибка записи данных!';

implementation

constructor TMessageDeleteDialog.Create (const aDBFileName: String;
                                           anArgs: array of const);
begin
    try
        inherited Create ([ 'удаление категории', dlgDelete,
                            '[B]Вы действительно хотите удалить сообщение?[/B]' ],
                          [ _(['Да',btnOK,mrYes]),
                            _(['Нет',btnCancel,mrNo]) ]);
        f_DBFileName := aDBFileName;
        f_DB := TSQLiteDatabase.Create (DBFileName);
        f_Message := TMessage.Create (DB,[]);
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            Message.ID := toInt64 (anArgs [0]);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{CED6A5B4-99F0-42C6-8DB0-0F15816A8F22}']);
    end;
end;

destructor TMessageDeleteDialog.Destroy;
begin
    try
        FreeAndNil (f_Message);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{9215F5A3-D395-487C-BAF4-14F33CE54657}']);
    end;
end;

procedure TMessageDeleteDialog.GetData (anArgs: array of const);
begin
    try
        { если указан идентификатор, то загружаем сообщение }
        if ( Message.ID > 0 ) then
            Message.Load
        else
            raise Exception.Create (ERR_TMESSAGE_INVALID_ID);
        inherited GetData ([ 'удаление категории', dlgDelete,
                             Format ('[B]Вы действительно хотите удалить сообщение "%s"?[/B]'#13#10+
                                     '[B]"%s..."[/B]',
                                     [ Message.Subject,
                                       Copy (Message.Text,1,64) ]) ],
                           [ _(['Да',btnOK,mrDelete]),
                             _(['Нет',btnCancel,mrNo]) ]);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{A1FFEEA0-E74F-46AD-ADB7-1E667F757257}']);
    end;
end;

procedure TMessageDeleteDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrDelete : doDelete;
            else       inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{CDD800D2-0504-4198-8AAD-185AB8A1F7A5}']);
    end;
end;

procedure TMessageDeleteDialog.doDelete;
begin
    try
        { удаляем сообщение }
        Message.Delete;
        SetMailData (Message);
        inherited SetData (mrYes);
    except on E: Exception do
        _raise (['doDelete',ERR_TMESSAGEDIALOG_DELETE,E],
                ['{83096119-DE90-4396-8A20-92BA483182DE}']);
    end;
end;

class function TMessageDeleteDialog.Open (const aDBFileName: String;
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
                ['{8E6DF3EF-AF65-4A1C-A177-D8910D20BEB8}']);
    end;
end;

class function TMessageDeleteDialog.Execute (const aDBFileName: String;
                                               anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrYes );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{CCAB0A89-7831-4331-8B4F-234AD1165A80}']);
    end;
end;


end.