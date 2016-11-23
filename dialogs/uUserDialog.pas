unit uUserDialog;
{******************************************************************************}
{*  User Dialog Unit                                                          *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus,
    ImgList, acAlphaImageList, jpeg, pngimage, acPNG,
    sSkinProvider, sSkinManager,
    sPanel, sPageControl, sGroupBox, sButton, sBitBtn, sSpeedButton, sTabControl,
    sLabel, sCheckBox,
    sEdit, sCurrEdit, sCurrencyEdit, sRichEdit, sMemo, Mask, sMaskEdit,
    sSpinEdit, sTooledit, sCustomComboEdit,
    sComboEdit, sComboBox, sComboBoxes,
    Crypto, Kernel, DialogClasses, Engine,
    uProtoDialog, uTextDialog,
    uRandomTestDialog, uAsymmetricTestDialog, uSymmetricTestDialog, uHashTestDialog,
    Utils, Strings, Versions, VarRecs,
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
    TUserDialog = class;
    TUserContent = class;
    TUserDeleteDialog = class;
{$M-}

{ диалоговое окно регистрации }
{$M+}
    CUserDialog = class of TUserDialog;
    PUserDialog = ^TUserDialog;
    TUserDialog = class (TProtoDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_LocalUser: TUser;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure OnPicChange (Sender: TObject); virtual;
    protected
        function CheckLogin (const aLogin: String) : Boolean; virtual;
        function CheckEMail (const anEMail: String) : Boolean; virtual;
        function CheckIP (const anIP: String) : Boolean; virtual;
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
        property LocalUser: TUser read f_LocalUser write f_LocalUser;
    end;
{$M-}

{ содержимое }
{$M+}
    CUserContent = class of TUserContent;
    PUserContent = ^TUserContent;
    TUserContent = class (TFrame)
        tabs: TsPageControl;
        tbMain: TsTabSheet;
        pnlMainLabels: TsPanel;
        lbLogin: TsLabel;
        lbMail: TsLabel;
        lbIP: TsLabel;
        lbPort: TsLabel;
        lbPic: TsLabel;
        lbDescription: TsLabel;
        pnlMainControls: TsPanel;
        login: TsEdit;
        mail: TsEdit;
        ip: TsEdit;
        port: TsSpinEdit;
        pnlPic: TsPanel;
        imgPic: TImage;
        pic: TsFilenameEdit;
        description: TsRichEdit;
        tbCrypto: TsTabSheet;
        pnlCryptoControls: TsPanel;
        btRandomTest: TsSpeedButton;
        btAsymmetricTest: TsSpeedButton;
        btSymmetricTest: TsSpeedButton;
        btHashTest: TsSpeedButton;
        random: TsComboBox;
        PublicKey: TsRichEdit;
        asymmetric: TsComboBox;
        symmetric: TsComboBox;
        mode: TsComboBox;
        hash: TsComboBox;
        pnlCryptoLabels: TsPanel;
        lbRandom: TsLabel;
        lbAsymmetric: TsLabel;
        lbSymmetric: TsLabel;
        lbMode: TsLabel;
        lbHash: TsLabel;
        lbPublicKey: TsLabel;
        sex: TsComboBoxEx;
        birthday: TsDateEdit;
        imgSex: TsAlphaImageList;
        procedure btHashTestClick(Sender: TObject);
        procedure btSymmetricTestClick(Sender: TObject);
        procedure btAsymmetricTestClick(Sender: TObject);
        procedure btRandomTestClick(Sender: TObject);
        procedure FrameResize(Sender: TObject);
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
    CUserDeleteDialog = class of TUserDeleteDialog;
    PUserDeleteDialog = ^TUserDeleteDialog;
    TUserDeleteDialog = class (TTextDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
        f_LocalUser: TUser;
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
        property LocalUser : TUser read f_LocalUser write f_LocalUser;
    end;
{$M-}

resourcestring
    ERR_TUSERCONTENT_CREATE          = 'Ошибка создания!';
    ERR_TUSERCONTENT_DESTROY         = 'Ошибка уничтожения!';
    ERR_TUSERCONTENT_GET_VALUE       = 'Ошибка чтения поля!';
    ERR_TUSERCONTENT_SET_VALUE       = 'Ошибка записи поля!';
    ERR_TUSERCONTENT_GET_DATA        = 'Ошибка чтения данных!';
    ERR_TUSERCONTENT_SET_DATA        = 'Ошибка записи данных!';
    ERR_TUSERCONTENT_TEST_RANDOM     = 'Ошибка тестирования генератора псевдо-случайных чисел!';
    ERR_TUSERCONTENT_TEST_ASYMMETRIC = 'Ошибка тестирования асимметричного шифра!';
    ERR_TUSERCONTENT_TEST_SYMMETRIC  = 'Ошибка тестирования симметричного шифра!';
    ERR_TUSERCONTENT_TEST_HASH       = 'Ошибка тестирования хэш-функции!';

resourcestring
    ERR_TUSERDIALOG_SAVE                = 'Ошибка сохранения!';
    ERR_TUSERDIALOG_SAVE_USER           = 'Ошибка сохранения пользователя ''%s''!';
    ERR_TUSERDIALOG_USER_ALREADY_EXISTS = 'Пользователь ''%s'' уже зарегестрирован в системе!';
    ERR_TUSERDIALOG_DELETE              = 'Ошибка удаления!';
    ERR_TUSERDIALOG_DELETE_USER         = 'Ошибка удаления пользователя ''%s''!';

implementation

{$R *.dfm}

constructor TUserDialog.Create (const aDBFileName: String;
                                anArgs: array of const);
begin
    try
        inherited Create ([ 'пользователь', dlgCustom ],
                          [ _(['OK',btnOK,mrSave]),
                            _(['Отмена',btnCancel,mrCancel]) ],
                          NIL);
        Content := TUserContent.Create (self,anArgs);
        minHeight := 64 + TUserContent (Content).Height + 8 + pnlButtons.Height;
        maxHeight := minHeight;
        Height := minHeight;
        minWidth := 500;
        Width := minWidth;
        with TUserContent (Content) do
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
        f_LocalUser := TUser.Create (DB,[]);
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            LocalUser.ID := toInt64 (anArgs [0]);
        { второй параметр - логин }
        if notEmpty (1,anArgs) then
            LocalUser.Login := toString (anArgs [1]);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{9DC89816-68BA-46D3-A2FF-0948A78503F5}']);
    end;
end;

destructor TUserDialog.Destroy;
begin
    try
        //ShowMessage (f_LocalUser.Pic.Picture.Graphic.ClassName);
        FreeAndNil (f_LocalUser);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{BDAA0FA5-67DD-4DAB-876A-8F0285BD6CAD}']);
    end;
end;

procedure TUserDialog.GetData (anArgs: array of const);
begin
    try
        if ( LocalUser.ID > 0 ) then
            inherited GetData ([ 'редактирование пользователя', dlgEdit ],
                               [ _(['OK',btnOk,mrSave]),
                                 _(['Отмена',btnCancel,mrCancel]) ])
        else
            inherited GetData ([ 'создание пользователя', dlgAdd ],
                               [ _(['OK',btnOk,mrSave]),
                                 _(['Отмена',btnCancel,mrCancel]) ]);
        { если указан идентификатор или логин, то загружаем пользователя }
        if ( LocalUser.ID > 0 ) or ( Length (LocalUser.Login) > 0 ) then
            LocalUser.Load;
        with TUserContent (Content) do
            GetData (anArgs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{085C08B3-9C1B-4CCE-865D-9417E05207A9}']);
    end;
end;

procedure TUserDialog.SetData (anIndex: Integer);
begin
    try
        with TUserContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrSave : doSave;
            else     inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{BB00AE28-DBF1-4CD1-B4F9-497A88898CEF}']);
    end;
end;

function TUserDialog.CheckLogin (const aLogin: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TUserContent (Content) do
    try
        Result := TUser.CheckLogin (aLogin,Errors);
    finally
        if not Result then
        begin
            login.Text := '';
            tabs.ActivePage := tbMain;
            login.SetFocus;
            raise Exception.Create (Errors);
        end;
    end;
end;

function TUserDialog.CheckEMail (const anEMail: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TUserContent (Content) do
    try
        Result := TUser.CheckEMail (anEMail,Errors);
    finally
        if not Result then
        begin
            mail.Text := '';
            tabs.ActivePage := tbMain;
            mail.SetFocus;
            raise Exception.Create (Errors);
        end;
    end;
end;

function TUserDialog.CheckIP (const anIP: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TUserContent (Content) do
    try
        Result := TUser.CheckIP (anIP,Errors);
    finally
        if not Result then
        begin
            ip.Text := '';
            tabs.ActivePage := tbMain;
            ip.SetFocus;
            raise Exception.Create (Errors);
        end;
    end;
end;

procedure TUserDialog.OnPicChange (Sender: TObject);
begin
    with TUserContent (Content) do
    try
        if notEmpty (pic.FileName) then
        try
            imgPic.Picture.Bitmap.FreeImage;
            if ValidateFileSize (pic.FileName,24) then
                imgPic.Picture.LoadFromFile (pic.FileName);
        except on E: Exception do
            ShowError (E.Message);
        end;
    finally
    end;
end;

procedure TUserDialog.doSave;
var
    isNew : Boolean;
begin
    try
        with TUserContent (Content) do
        begin
            try
                CheckLogin ( Trim (login.Text) );
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            LocalUser.Login := login.Text;
            LocalUser.Crypto.genRandom := random.Text;
            LocalUser.Crypto.algAsymmetric := asymmetric.Text;
            LocalUser.Crypto.algSymmetric := symmetric.Text;
            LocalUser.Crypto.modeSymmetric := mode.Text;
            LocalUser.Crypto.algHash := hash.Text;
            LocalUser.PublicKey := Trim (PublicKey.Text);
            if notEmpty (mail.Text) then
            try
                CheckEMail ( Trim (mail.Text) );
                LocalUser.EMail := Trim (mail.Text);
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            if notEmpty (ip.Text) then
            try
                CheckIP ( Trim (ip.Text) );
                LocalUser.IP := Trim (ip.Text);
                LocalUser.Port := port.Value;
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            LocalUser.Description := Trim (description.Text);
            LocalUser.Sex := sex.ItemIndex;
            LocalUser.Birthday := birthday.Date;
            if notEmpty (pic.FileName) and
               FileExists (pic.FileName) and
               ValidateFileSize (pic.FileName,24) then
            begin
                LocalUser.Pic.Picture.LoadFromFile (pic.FileName);
                LocalUser.Pic.DataFormat := ExtractFileExt (pic.FileName);
            end
            else if Assigned (imgPic.Picture) and Assigned (imgPic.Picture.Graphic) then { anonymous }
                LocalUser.Pic.Picture.Assign (imgPic.Picture);
            isNew := not ( LocalUser.ID > 0 );
            { сохраняем пользователя }
            LocalUser.Save;
            { создаем пакеты }
            SetMailData (LocalUser);
            if isNew then
                UpdateMailData (LocalUser);
        end;
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doSave',ERR_TUSERDIALOG_SAVE,E],
                ['{E7BD19AD-48F8-4037-8602-E1DB92182D16}']);
    end;
end;

class function TUserDialog.Open (const aDBFileName: String;
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
                ['{432F8F63-4FC7-4F93-92BA-BA0AAC3568D8}']);
    end;
end;

class function TUserDialog.Execute (const aDBFileName: String;
                                    anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{6BCAB149-F35D-404E-B83A-826E5B4799E3}']);
    end;
end;

class procedure TUserContent._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TUserContent._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TUserContent.Create (anOwner: TProtoDialog;
                                 anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited Create (anOwner);
        for I := 0 to pnlMainControls.ControlCount - 1 do
        begin
            if ( pnlMainControls.Controls [I] is TCustomEdit ) then
                TEdit (pnlMainControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
            if ( pnlMainControls.Controls [I] is TCustomComboBox ) then
                TComboBox (pnlMainControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
        end;
        for I := 0 to pnlCryptoControls.ControlCount - 1 do
        begin
            if ( pnlCryptoControls.Controls [I] is TCustomEdit ) then
                TEdit (pnlCryptoControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
            if ( pnlCryptoControls.Controls [I] is TCustomComboBox ) then
                TComboBox (pnlCryptoControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
        end;
        sex.ItemIndex := 0;
        birthday.Date := _StrToDate ('01.05.1886');
        {$IFNDEF HTTP}
        ip.Enabled := FALSE;
        port.Enabled := FALSE;
        {$ENDIF}
        {$IFNDEF SMTP_POP3}
        mail.Enabled := FALSE;
        {$ENDIF}
    except on E: Exception do
        _raise (['Create',ERR_TUSERCONTENT_CREATE,E],
                ['{3A9A7D67-E915-4922-919D-28DD42BDEE50}']);
    end;
end;

destructor TUserContent.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TUSERCONTENT_DESTROY,E],
                ['{D8B4F1AA-CA70-41F4-B878-5662032DBF03}']);
    end;
end;

procedure TUserContent.FrameResize (Sender: TObject);
begin
    try
        btRandomTest.Left := random.Left + random.Width;
        btAsymmetricTest.Left := asymmetric.Left + asymmetric.Width;
        btSymmetricTest.Left := Symmetric.Left + Symmetric.Width;
        btHashTest.Left := hash.Left + hash.Width;
    except on E: Exception do
        _raise (['FrameResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{F584291C-04F9-468A-BCCE-65F2BA2880AA}']);
    end;
end;

procedure TUserContent.GetData (anArgs: array of const);
var
    I : Integer;
begin
    try
        { основные }
        with TUserDialog (Owner) do
            if Assigned (LocalUser) and ( LocalUser.ID > 0 ) then
            begin
                login.Text := LocalUser.Login;
                mail.Text := LocalUser.EMail;
                ip.Text := LocalUser.IP;
                port.Value := LocalUser.Port;
                description.Clear;
                description.Text := LocalUser.Description;
                sex.ItemIndex := LocalUser.Sex;
                birthday.Date := LocalUser.BirthDay;
            end;
        { изображение }
        with TUserDialog (Owner) do
        begin
            pic.OnChange := OnPicChange;
            if Assigned (LocalUser.Pic.Picture) and
               ( (LocalUser.Pic.Picture.Width > 0) or
                 (LocalUser.Pic.Picture.Height > 0) ) then
                imgPic.Picture.Assign (LocalUser.Pic.Picture);
        end;
        { генератор псевдо-случайных чисел }
        with random.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (RANDOM_TYPE_STRING) + 1 to High (RANDOM_TYPE_STRING) do
                Add (RANDOM_TYPE_STRING [I]);
            with TUserDialog (Owner) do
                random.Text := LocalUser.Crypto.genRandom;
        finally
            EndUpdate;
        end;
        { асимм. шифр }
        with asymmetric.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (PKCRYPTO_TYPE_STRING) + 1 to High (PKCRYPTO_TYPE_STRING) do
                Add (PKCRYPTO_TYPE_STRING [I]);
            with TUserDialog (Owner) do
                asymmetric.Text := LocalUser.Crypto.algAsymmetric;
        finally
            EndUpdate;
        end;
        { симм. шифр }
        with symmetric.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (CRYPTO_TYPE_STRING) + 1 to High (CRYPTO_TYPE_STRING) do
                Add (CRYPTO_TYPE_STRING [I]);
            with TUserDialog (Owner) do
                symmetric.Text := LocalUser.Crypto.algSymmetric;
        finally
            EndUpdate;
        end;
        with mode.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (CRYPTO_MODE_STRING) + 1 to High (CRYPTO_MODE_STRING) do
                Add (CRYPTO_MODE_STRING [I]);
            with TUserDialog (Owner) do
                mode.Text := LocalUser.Crypto.modeSymmetric;
        finally
            EndUpdate;
        end;
        { хэш-функция }
        with hash.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (HASH_TYPE_STRING) + 1 to High (HASH_TYPE_STRING) do
                Add (HASH_TYPE_STRING [I]);
            with TUserDialog (Owner) do
                hash.Text := LocalUser.Crypto.algHash;
        finally
            EndUpdate;
        end;
        { ключ }
        with TUserDialog (Owner) do
        begin
            PublicKey.Clear;
            PublicKey.Text := LocalUser.PublicKey;
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TUSERCONTENT_GET_DATA,E],
                ['{43AC50B2-594D-4DF4-BA88-B21CAEEDBFD6}']);
    end;
end;

procedure TUserContent.SetData (anIndex: Integer);
begin
    try
    except on E: Exception do
        _raise (['SetData',ERR_TUSERCONTENT_SET_DATA,E],
                ['{316F43EC-F62A-439A-85DF-3C572D013BD3}']);
    end;
end;

procedure TUserContent.btRandomTestClick (Sender: TObject);
begin
    try
        RandomTest (random.Text);
    except on E: Exception do
        _raise (['btRandomTestClick',ERR_TUSERCONTENT_TEST_RANDOM,E],
                ['{60601A22-AC5D-4C99-82AE-A4F3486D8FD8}']);
    end;
end;

procedure TUserContent.btAsymmetricTestClick (Sender: TObject);
begin
    try
        AsymmetricTest (asymmetric.Text);
    except on E: Exception do
        _raise (['btAsymmetricTestClick',ERR_TUSERCONTENT_TEST_ASYMMETRIC,E],
                ['{CD931084-2479-4F9C-B0BE-52D3EBD79113}']);
    end;
end;

procedure TUserContent.btSymmetricTestClick (Sender: TObject);
begin
    try
        SymmetricTest (symmetric.Text,mode.Text);
    except on E: Exception do
        _raise (['btSymmetricTestClick',ERR_TUSERCONTENT_TEST_SYMMETRIC,E],
                ['{9CDC49A3-7A99-43DD-920F-3D91CA278C5D}']);
    end;
end;

procedure TUserContent.btHashTestClick (Sender: TObject);
begin
    try
        HashTest (hash.Text);
    except on E: Exception do
        _raise (['btHashTestClick',ERR_TUSERCONTENT_TEST_HASH,E],
                ['{A5DF5FD4-841A-4473-8D73-2E6131582717}']);
    end;
end;

constructor TUserDeleteDialog.Create (const aDBFileName: String;
                                      anArgs: array of const);
begin
    try
        inherited Create ([ 'удаление контакта', dlgDelete,
                            '[B]Вы действительно хотите удалить контакт?[/B]' ],
                          [ _(['Да',btnOK,mrYes]),
                            _(['Нет',btnCancel,mrNo]) ]);
        f_DBFileName := aDBFileName;
        f_DB := TSQLiteDatabase.Create (DBFileName);
        f_LocalUser := TUser.Create (DB,[]);
        { первый параметр - идентификатор }
        if notEmpty (0,anArgs) then
            LocalUser.ID := toInt64 (anArgs [0]);
        { второй параметр - логин }
        if notEmpty (1,anArgs) then
            LocalUser.Login := toString (anArgs [1]);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{37CFAE24-ECB8-40E4-8528-2EDF57AA3456}']);
    end;
end;

destructor TUserDeleteDialog.Destroy;
begin
    try
        FreeAndNil (f_LocalUser);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{F5D216BB-9CE8-4D0D-9E16-056C8ECB4A4F}']);
    end;
end;

procedure TUserDeleteDialog.GetData (anArgs: array of const);
begin
    try
        { если указан идентификатор или логин, то загружаем пользователя }
        if ( LocalUser.ID > 0 ) or ( Length (LocalUser.Login) > 0 ) then
            LocalUser.Load
        else
            raise Exception.Create (ERR_TCATEGORIE_INVALID_ID);
        inherited GetData ([ 'удаление контакта', dlgDelete,
                             Format ('[B]Вы действительно хотите удалить контакт "%s"?[/B]',[LocalUser.Login]) ],
                           [ _(['Да',btnOK,mrDelete]),
                             _(['Нет',btnCancel,mrNo]) ]);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{A540560B-4192-4EB4-A019-EF825E57152F}']);
    end;
end;

procedure TUserDeleteDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrDelete : doDelete;
            else       inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{2418C268-644D-4183-B1F2-6CAFE499F58F}']);
    end;
end;

procedure TUserDeleteDialog.doDelete;
begin
    try
        { удаляем пользователя }
        LocalUser.Delete;
        //SetMailData (LocalUser);
        inherited SetData (mrYes);
    except on E: Exception do
        _raise (['doDelete',ERR_TUSERDIALOG_DELETE,E],
                ['{5537519F-D137-4545-B188-40FAC11A35A3}']);
    end;
end;

class function TUserDeleteDialog.Open (const aDBFileName: String;
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
                ['{85E09EC6-F709-49E0-9B8F-B2EBEFC1E18C}']);
    end;
end;

class function TUserDeleteDialog.Execute (const aDBFileName: String;
                                          anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrYes );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{59CF8973-57FC-4898-A461-1FCE9C7E663D}']);
    end;
end;


end.
