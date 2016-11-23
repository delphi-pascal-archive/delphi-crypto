unit uCategorieDialog;
{******************************************************************************}
{*  Categorie Dialog Unit                                                     *}
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
    btSave   = 0;
    btCancel = 1;

    btDelete = 0;

{ действия }
const
    mrSave   = 14;
    mrDelete = 15;

type
{$M+}
    TCategorieDialog = class;
    TCategorieContent = class;
    TCategorieDeleteDialog = class;
{$M-}

{ диалоговое окно создания/редактирования категории }
{$M+}
    CCategorieDialog = class of TCategorieDialog;
    PCategorieDialog = ^TCategorieDialog;
    TCategorieDialog = class (TProtoDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_Categorie: TCategorie;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure doSave; virtual;
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
        property Categorie: TCategorie read f_Categorie write f_Categorie;
    end;
{$M-}

{ содержимое }
{$M+}
    CCategorie = class of TCategorie;
    PCategorie = ^TCategorieContent;
    TCategorieContent = class (TFrame)
        pnlBackGround: TsPanel;
        pnlLabels: TsPanel;
        lbParentName: TsLabel;
        lbName: TsLabel;
        lbIndexPic: TsLabel;
        lbDescription: TsLabel;
        pnlControls: TsPanel;
        ParentName: TsComboBoxEx;
        imgParent: TsAlphaImageList;
        CategorieName: TsEdit;
        Pic: TsComboBoxEx;
        imgPic: TsAlphaImageList;
        Description: TsRichEdit;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anOwner: TProtoDialog;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
    end;
{$M-}

{ диалоговое окно удаления категории }
{$M+}
    CCategorieDeleteDialog = class of TCategorieDeleteDialog;
    PCategorieDeleteDialog = ^TCategorieDeleteDialog;
    TCategorieDeleteDialog = class (TTextDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_Categorie: TCategorie;
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
        property Categorie : TCategorie read f_Categorie write f_Categorie;
    end;
{$M-}

resourcestring
    ERR_TCATEGORIEDIALOG_SAVE     = 'Ошибка сохранения категории!';
    ERR_TCATEGORIEDIALOG_DELETE   = 'Ошибка удаления категории!';

resourcestring
    ERR_TCATEGORIEDIALOG_CREATE   = 'Ошибка создания!';
    ERR_TCATEGORIEDIALOG_DESTROY  = 'Ошибка уничтожения!';
    ERR_TCATEGORIEDIALOG_GET_DATA = 'Ошибка чтения данных!';
    ERR_TCATEGORIEDIALOG_SET_DATA = 'Ошибка записи данных!';

implementation

{$R *.dfm}

constructor TCategorieDialog.Create (const aDBFileName: String;
                                     anArgs: array of const);
begin
    try
        inherited Create ([ 'категория', dlgCustom ],
                          [ _(['OK',btnOk,mrSave]),
                            _(['Отмена',btnCancel,mrCancel]) ],
                          NIL);
        Content := TCategorieContent.Create (self,anArgs);
        minHeight := 60 + TCategorieContent (Content).Height + pnlButtons.Height;
        Height := minHeight;
        minWidth := 450;
        Width := minWidth;
        with TCategorieContent (Content) do
        begin
            Parent := pnlBack;
            Align := alClient;
            AlignWithMargins := TRUE;
            Margins.Left := 10;
            Margins.Top := 0;
            Margins.Right := 10;
            Margins.Bottom := 8;
        end;
        f_DBFileName := aDBFileName;
        f_DB := TSQLiteDatabase.Create (DBFileName);
        f_Categorie := TCategorie.Create (DB,[]);
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            Categorie.ID := toInt64 (anArgs [0]);
        { второй параметр - идентификатор предка }
        if notEmpty (1,anArgs) then
            Categorie.IDParent := toInt64 (anArgs [1]);
        Categorie.IDAuthor := USER_ID;
        Categorie.IDOwner := USER_ID;
        Categorie.IDType := CATEGORIE_FORUM_TYPE_ID;
        Categorie.IDStatus := CATEGORIE_OPENED_STATUS_ID;
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{F2074D2C-AEB4-411C-A9DA-63A50A3A2F64}']);
    end;
end;

destructor TCategorieDialog.Destroy;
begin
    try
        FreeAndNil (f_Categorie);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{5757917C-5A43-4DAA-ACEA-2E9DB2D3D735}']);
    end;
end;

procedure TCategorieDialog.GetData (anArgs: array of const);
begin
    try
        if ( Categorie.ID > 0 ) then
            inherited GetData ([ 'редактирование категории', dlgEdit ],
                               [ _(['OK',btnOk,mrSave]),
                                 _(['Отмена',btnCancel,mrCancel]) ])
        else
            inherited GetData ([ 'создание категории', dlgAdd ],
                               [ _(['OK',btnOk,mrSave]),
                                 _(['Отмена',btnCancel,mrCancel]) ]);
        { если указан идентификатор, то загружаем категорию }
        if ( Categorie.ID > 0 ) then
            Categorie.Load
        else if ( Categorie.IDParent > 0 ) then
            Categorie.Parent.Load;
        with TCategorieContent (Content) do
            GetData (anArgs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{C9D77C92-C33C-4B26-A969-D779A6DB0B60}']);
    end;
end;

procedure TCategorieDialog.SetData (anIndex: Integer);
begin
    try
        with TCategorieContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrSave : doSave;
            else     inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{F1D02BA4-524D-459C-A459-FDFFC8B32881}']);
    end;
end;

procedure TCategorieDialog.doSave;
var
    I : Integer;
begin
    try
        { сохраняем категорию }
        Categorie.Save;
        { создаем пакеты }
        SetMailData (Categorie);
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doSave',ERR_TCATEGORIEDIALOG_SAVE,E],
                ['{E574C327-C300-446F-BBA6-58F510A9BD85}']);
    end;
end;

class function TCategorieDialog.Open (const aDBFileName: String;
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
                ['{AAAB8AE8-E0EE-45DE-959B-D06F87F03FF0}']);
    end;
end;

class function TCategorieDialog.Execute (const aDBFileName: String;
                                         anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{CBC0C7FE-CDE3-4461-97D4-33215207F3B3}']);
    end;
end;

class procedure TCategorieContent._raise (anArgs: array of const;
                                          const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TCategorieContent._raise (anArgs: array of const;
                                          anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TCategorieContent.Create (anOwner: TProtoDialog;
                                      anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited Create (anOwner);
        for I := 0 to pnlControls.ControlCount - 1 do
            if ( pnlControls.Controls [I] is TCustomEdit ) then
                TEdit (pnlControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
    except on E: Exception do
        _raise (['Create',ERR_TCATEGORIEDIALOG_CREATE,E],
                ['{4311B692-4A53-493D-A065-92901A4F9ED2}']);
    end;
end;

destructor TCategorieContent.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCATEGORIEDIALOG_DESTROY,E],
                ['{7717D58E-1512-4DF5-85AD-FBDE76CA8714}']);
    end;
end;

procedure TCategorieContent.GetData (anArgs: array of const);
begin
    try
        with TCategorieDialog (Owner) do
        begin
            if ( Categorie.IDParent = ROOT_CATEGORIE_ID ) then
            begin
                ParentName.Items.Delete (1);
                ParentName.ItemIndex := 0;
            end
            else
            begin
                ParentName.ItemsEx.Items [1].Caption := Categorie.Parent.Name;
                if ( Categorie.IDParent > 0 ) then
                    ParentName.ItemIndex := 1
                else
                    ParentName.ItemIndex := 0;
            end;
            ParentName.Enabled := ( Categorie.ID = 0 );
            CategorieName.Text := Categorie.Name;
            Pic.ItemIndex := Categorie.IndexPic;
            Description.Clear;
            Description.Text := Categorie.Description;
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TCATEGORIEDIALOG_GET_DATA,E],
                ['{01739D3B-839B-493C-9042-1A429F49A1C0}']);
    end;
end;

procedure TCategorieContent.SetData (anIndex: Integer);
begin
    try
        with TCategorieDialog (Owner) do
        begin
            if ( ParentName.SelectedItem.Index = 0 ) then
            begin
                Categorie.HashParent := ROOT_CATEGORIE_KEY_HASH;
                Categorie.IDParent := ROOT_CATEGORIE_ID;
                Categorie.IDType := CATEGORIE_FORUM_TYPE_ID;
            end
            else
                Categorie.IDType := CATEGORIE_TOPIC_TYPE_ID;
            Categorie.Name := CategorieName.Text;
            Categorie.IndexPic := Pic.ItemIndex;
            Categorie.Description := Description.Text;
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TCATEGORIEDIALOG_SET_DATA,E],
                ['{1675B9EF-964D-4C68-828E-4F2FB7E7F8CF}']);
    end;
end;

constructor TCategorieDeleteDialog.Create (const aDBFileName: String;
                                           anArgs: array of const);
begin
    try
        inherited Create ([ 'удаление категории', dlgDelete,
                            '[B]Вы действительно хотите удалить категорию?[/B]' ],
                          [ _(['Да',btnOK,mrYes]),
                            _(['Нет',btnCancel,mrNo]) ]);
        f_DBFileName := aDBFileName;
        f_DB := TSQLiteDatabase.Create (DBFileName);
        f_Categorie := TCategorie.Create (DB,[]);
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            Categorie.ID := toInt64 (anArgs [0]);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{AA363060-81E0-4A77-9283-6A41EAA790EC}']);
    end;
end;

destructor TCategorieDeleteDialog.Destroy;
begin
    try
        FreeAndNil (f_Categorie);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{169C1AEE-F86E-4A72-8700-CBD5EBE62FF1}']);
    end;
end;

procedure TCategorieDeleteDialog.GetData (anArgs: array of const);
begin
    try
        { если указан идентификатор, то загружаем категорию }
        if ( Categorie.ID > 0 ) then
            Categorie.Load
        else
            raise Exception.Create (ERR_TCATEGORIE_INVALID_ID);
        inherited GetData ([ 'удаление категории', dlgDelete,
                             Format ('[B]Вы действительно хотите удалить категорию "%s"?[/B]',[Categorie.Name]) ],
                           [ _(['Да',btnOK,mrDelete]),
                             _(['Нет',btnCancel,mrNo]) ]);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{7D3B4AFF-3E64-4FD5-862E-6FA914EF15A5}']);
    end;
end;

procedure TCategorieDeleteDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrDelete : doDelete;
            else       inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{FB758305-D742-4097-8750-A871C48ECC87}']);
    end;
end;

procedure TCategorieDeleteDialog.doDelete;
begin
    try
        { удаляем категорию }
        Categorie.Delete;
        //SetMailData (Categorie);
        inherited SetData (mrYes);
    except on E: Exception do
        _raise (['doDelete',ERR_TCATEGORIEDIALOG_DELETE,E],
                ['{8A7BCA6D-1415-4E80-BB56-BC24F3CD44C9}']);
    end;
end;

class function TCategorieDeleteDialog.Open (const aDBFileName: String;
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
                ['{686BE92F-BB89-4C5C-84FF-CED7024116B5}']);
    end;
end;

class function TCategorieDeleteDialog.Execute (const aDBFileName: String;
                                               anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrYes );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{192A8BDB-8834-40F7-AFF6-F057B000F9BA}']);
    end;
end;


end.
