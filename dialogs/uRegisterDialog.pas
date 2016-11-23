unit uRegisterDialog;
{******************************************************************************}
{*  Register Dialog Unit                                                      *}
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
    uProtoDialog, uMetaDialog,
    uRandomTestDialog, uAsymmetricTestDialog, uSymmetricTestDialog, uHashTestDialog,
    Utils, Strings, Versions, VarRecs,
    EClasses,
    SQLite3, SQLite3DLL, SQLiteTable3;

{ кнопки }
const
    btRegister = 0;
    btCancel   = 1;

{ действия }
const
    mrRegister = 13;

type
{$M+}
    TRegisterContent = class;
    TRegisterDialog = class;
{$M-}

{ диалоговое окно регистрации }
{$M+}
    CRegisterDialog = class of TRegisterDialog;
    PRegisterDialog = ^TRegisterDialog;
    TRegisterDialog = class (TProtoDialog)
    private
        f_DBFileName: String;
        f_DB: TSQLiteDatabase;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure OnChangeKey (Sender: TObject); virtual;
        procedure OnGetKey (Sender: TObject); virtual;
        procedure OnPicChange (Sender: TObject); virtual;
    protected
        function CheckLogin (const aLogin: String) : Boolean; virtual;
        function CheckPassword (const aPassword: String) : Boolean; virtual;
        function CheckEMail (const anEMail: String) : Boolean; virtual;
        function CheckIP (const anIP: String) : Boolean; virtual;
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

{ содержимое }
{$M+}
    CRegisterContent = class of TRegisterContent;
    PRegisterContent = ^TRegisterContent;
    TRegisterContent = class (TFrame)
        tabs: TsPageControl;
        tbMain: TsTabSheet;
        pnlMainLabels: TsPanel;
        lbLogin: TsLabel;
        lbPassword1: TsLabel;
        lbPassword2: TsLabel;
        lbMail: TsLabel;
        lbMailPassword: TsLabel;
        lbIP: TsLabel;
        lbPort: TsLabel;
        lbPic: TsLabel;
        lbDescription: TsLabel;
        pnlMainControls: TsPanel;
        login: TsEdit;
        password2: TsMaskEdit;
        password1: TsMaskEdit;
        mail: TsEdit;
        MailPassword: TsMaskEdit;
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
        PrivateKey: TsComboEdit;
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
        lbPrivateKey: TsLabel;
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
    private
        f_KeysGenerated : Boolean;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anOwner: TProtoDialog;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
    property
        KeysGenerated : Boolean read f_KeysGenerated;
    end;
{$M-}

resourcestring
    ERR_TREGISTERCONTENT_CREATE              = 'Ошибка создания!';
    ERR_TREGISTERCONTENT_DESTROY             = 'Ошибка уничтожения!';
    ERR_TREGISTERCONTENT_GET_VALUE           = 'Ошибка чтения поля!';
    ERR_TREGISTERCONTENT_SET_VALUE           = 'Ошибка записи поля!';
    ERR_TREGISTERCONTENT_GET_DATA            = 'Ошибка чтения данных!';
    ERR_TREGISTERCONTENT_SET_DATA            = 'Ошибка записи данных!';
    ERR_TREGISTERCONTENT_PASSWORDS_NOT_EQUAL = 'Пароли не совпадают!';
    ERR_TREGISTERCONTENT_TEST_RANDOM         = 'Ошибка тестирования генератора псевдо-случайных чисел!';
    ERR_TREGISTERCONTENT_TEST_ASYMMETRIC     = 'Ошибка тестирования асимметричного шифра!';
    ERR_TREGISTERCONTENT_TEST_SYMMETRIC      = 'Ошибка тестирования симметричного шифра!';
    ERR_TREGISTERCONTENT_TEST_HASH           = 'Ошибка тестирования хэш-функции!';

resourcestring
    ERR_TREGISTERDIALOG_REGISTER            = 'Ошибка регистрации!';
    ERR_TREGISTERDIALOG_REGISTER_USER       = 'Ошибка регистрации пользователя ''%s''!';
    ERR_TREGISTERDIALOG_USER_ALREADY_EXISTS = 'Пользователь ''%s'' уже зарегестрирован в системе!';
    ERR_TREGISTERDIALOG_INVALID_PASSWORD    = 'Ошибка проверки пароля пользователя ''%s''!';

implementation

{$R *.dfm}

constructor TRegisterDialog.Create (const aDBFileName: String;
                                    anArgs: array of const);
begin
    try
        inherited Create ([ 'регистрация', dlgPassword ],
                          [ _(['OK',btnOK,mrRegister]),
                            _(['Отмена',btnCancel,mrCancel]) ],
                          NIL);
        Content := TRegisterContent.Create (self,anArgs);
        minHeight := 64 + TRegisterContent (Content).Height + 8 + pnlButtons.Height;
        maxHeight := minHeight;
        Height := minHeight;
        minWidth := 500;
        Width := minWidth;
        with TRegisterContent (Content) do
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
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{494D680B-F0A4-487A-878E-8507B7239C92}']);
    end;
end;

destructor TRegisterDialog.Destroy;
begin
    try
        if not ( User.ID > 0 ) then
            FreeAndNil (User);
        FreeAndNil (f_DB);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{37C8DD88-0247-43F4-800F-5139595B0E95}']);
    end;
end;

procedure TRegisterDialog.GetData (anArgs: array of const);
begin
    try
        inherited GetData ([ 'регистрация', dlgPassword ],
                           [ _(['OK',btnOK,mrRegister]),
                             _(['Отмена',btnCancel,mrCancel]) ]);
        User := TUser.Create (DB,[]);
        with TRegisterContent (Content) do
            GetData (anArgs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{EE7F3E3E-EFF5-4C79-82A0-99DD19FFBF43}']);
    end;
end;

procedure TRegisterDialog.SetData (anIndex: Integer);
begin
    try
        with TRegisterContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrRegister : doRegister;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{7A6A34F7-8274-44F9-8EC0-AB262DDA0352}']);
    end;
end;

function TRegisterDialog.CheckLogin (const aLogin: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TRegisterContent (Content) do
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

function TRegisterDialog.CheckPassword (const aPassword: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TRegisterContent (Content) do
    try
        Result := TUser.CheckPassword (aPassword,Errors);
    finally
        if not Result then
        begin
            password1.Text := '';
            password2.Text := '';
            tabs.ActivePage := tbMain;
            password1.SetFocus;
            raise Exception.Create (Errors);
        end;
    end;
end;

function TRegisterDialog.CheckEMail (const anEMail: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TRegisterContent (Content) do
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

function TRegisterDialog.CheckIP (const anIP: String) : Boolean;
var
    Errors : String;
begin
    Result := TRUE;
    Errors := '';
    with TRegisterContent (Content) do
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

procedure TRegisterDialog.OnChangeKey (Sender: TObject);
begin
    with TRegisterContent (Content) do
    try
        PublicKey.Text := '';
        PrivateKey.Text := '';
        { ключи обнулены }
        f_KeysGenerated := FALSE;
    finally
    end;
end;

procedure TRegisterDialog.OnGetKey (Sender: TObject);
var
    pub, prv : String;
    salt     : String;
begin
    with TRegisterContent (Content) do
    try
        User.Crypto.genRandom := random.Text;
        User.Crypto.algAsymmetric := asymmetric.Text;
        User.Crypto.algSymmetric := symmetric.Text;
        User.Crypto.modeSymmetric := mode.Text;
        User.Crypto.algHash := hash.Text;
        User.Crypto.GenerateKey (salt);
        User.Salt := salt;
        if ( Trim (password1.Text) <> Trim (password2.Text) ) then
            raise Exception.Create (ERR_TREGISTERCONTENT_PASSWORDS_NOT_EQUAL)
        else if CheckPassword ( Trim (password1.Text) ) then
            User.Password := Trim (password1.Text)
        else
            raise Exception.CreateFmt (ERR_TREGISTERDIALOG_INVALID_PASSWORD,
                                       [login.Text]);
        User.Crypto.GenerateKeys (pub,prv);
        User.PublicKey := pub;
        User.PrivateKey := prv;
        PublicKey.Text := pub;
        // в целях безопасности не выводим никакие производные от приватного ключа!
        // в текстовом поле приватного ключа
        // выведем хэш от связанного с ним публичного ключа - это безопасно
        PrivateKey.Text := User.Crypto.Hash (User.PublicKey);
        { ключи сгенерированы }
        f_KeysGenerated := TRUE;
    finally
        _FillChar ( prv, Length (prv), $00 );
        _FillChar ( pub, Length (pub), $00 );
        _FillChar ( salt, Length (salt), $00 );
        //password1.Text := '';
        //password2.Text := '';
    end;
end;

procedure TRegisterDialog.OnPicChange (Sender: TObject);
begin
    with TRegisterContent (Content) do
    try
        imgPic.Picture.Bitmap.FreeImage;
        try
            if ValidateFileSize (pic.FileName,24) then
                imgPic.Picture.LoadFromFile (pic.FileName);
        except on E: Exception do
            ShowError (E.Message);
        end;
    finally
    end;
end;

procedure TRegisterDialog.doRegister;
begin
    try
        with TRegisterContent (Content) do
        begin
            try
                CheckLogin ( Trim (login.Text) );
                User.Login := login.Text;
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            { если ключи еще не сгенерированы - генерируем }
            if not KeysGenerated then
            try
                OnGetKey (NIL);
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end
            { если ключи уже сгенерированы - перезаписываем пароль }
            else
            try
                if ( Trim (password1.Text) <> Trim (password2.Text) ) then
                    raise Exception.Create (ERR_TREGISTERCONTENT_PASSWORDS_NOT_EQUAL)
                else if CheckPassword ( Trim (password1.Text) ) then
                    User.Password := Trim (password1.Text)
                else
                    raise Exception.CreateFmt (ERR_TREGISTERDIALOG_INVALID_PASSWORD,
                                               [login.Text]);
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            { тестируем пароль }
            try
                User.CheckPassword;
            except on E: Exception do begin
                ShowErrorFmt (ERR_TREGISTERDIALOG_INVALID_PASSWORD,
                              [ User.Login ]);
                Exit;
            end; end;
            if notEmpty (mail.Text) then
            try
                CheckEMail ( Trim (mail.Text) );
                User.EMail := Trim (mail.Text);
                User.EMailPassword := MailPassword.Text;
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            if notEmpty (ip.Text) then
            try
                CheckIP ( Trim (ip.Text) );
                User.IP := Trim (ip.Text);
                User.Port := port.Value;
            except on E: Exception do begin
                ShowError (E.Message);
                Exit;
            end; end;
            User.Description := Trim (description.Text);
            User.Sex := sex.ItemIndex;
            User.Birthday := birthday.Date;
            if notEmpty (pic.FileName) and
               FileExists (pic.FileName) and
               ValidateFileSize (pic.FileName,24) then
            begin
                User.Pic.Picture.LoadFromFile (pic.FileName);
                User.Pic.DataFormat := ExtractFileExt (pic.FileName);
            end
            else { anonymous }
                User.Pic.Picture.Assign (imgPic.Picture);
            User.UseProxy := FALSE;
            User.ProxyIP := '';
            User.ProxyPort := 0;
            User.ProxyLogin := '';
            User.ProxyPassword := '';
            User.SMTPHost := '';
            User.SMTPPort := 0;
            User.POP3Host := '';
            User.POP3Port := 0;
            User.AutoTLS := FALSE;
            User.FullSSL := FALSE;
            User.Save;
        end;
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doRegister',ERR_TREGISTERDIALOG_REGISTER,E],
                ['{5AA92079-09AA-4013-BF16-009683AC3CEF}']);
    end;
end;

class function TRegisterDialog.Open (const aDBFileName: String;
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
                ['{28D5020B-B8EC-4D49-A97C-5B6BDCF599DE}']);
    end;
end;

class function TRegisterDialog.Execute (const aDBFileName: String;
                                        anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (aDBFileName,anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{81847764-BFD4-4024-85C7-1EC5BCBD262E}']);
    end;
end;

class procedure TRegisterContent._raise (anArgs: array of const;
                                         const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TRegisterContent._raise (anArgs: array of const;
                                         anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TRegisterContent.Create (anOwner: TProtoDialog;
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
        MailPassword.Enabled := FALSE;
        {$ENDIF}
        { ключи обнулены }
        f_KeysGenerated := FALSE;
    except on E: Exception do
        _raise (['Create',ERR_TREGISTERCONTENT_CREATE,E],
                ['{25D04B54-1F92-4B61-BBA9-91A7ACA8E268}']);
    end;
end;

destructor TRegisterContent.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TREGISTERCONTENT_DESTROY,E],
                ['{5F73C783-49B7-4430-9C8A-AA28CC9EE597}']);
    end;
end;

procedure TRegisterContent.FrameResize (Sender: TObject);
begin
    try
        btRandomTest.Left := random.Left + random.Width;
        btAsymmetricTest.Left := asymmetric.Left + asymmetric.Width;
        btSymmetricTest.Left := Symmetric.Left + Symmetric.Width;
        btHashTest.Left := hash.Left + hash.Width;
    except on E: Exception do
        _raise (['FrameResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{D808173D-AB93-4A3E-BF13-7FFC763CBAC2}']);
    end;
end;

procedure TRegisterContent.GetData (anArgs: array of const);
var
    I : Integer;
begin
    try
        { логин }
        Login.OnChange := TRegisterDialog (Owner).OnChangeKey;
        { пароль }
        Password1.OnChange := TRegisterDialog (Owner).OnChangeKey;
        Password2.OnChange := TRegisterDialog (Owner).OnChangeKey;
        { изображение }
        with TRegisterDialog (Owner) do
            pic.OnChange := OnPicChange;
        { генератор псевдо-случайных чисел }
        with random.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (RANDOM_TYPE_STRING) + 1 to High (RANDOM_TYPE_STRING) do
                Add (RANDOM_TYPE_STRING [I]);
            with TRegisterDialog (Owner) do
            begin
                random.Text := User.Crypto.genRandom;
                random.OnChange := OnChangeKey;
            end;
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
            with TRegisterDialog (Owner) do
            begin
                asymmetric.Text := User.Crypto.algAsymmetric;
                asymmetric.OnChange := OnChangeKey;
            end;
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
            with TRegisterDialog (Owner) do
            begin
                symmetric.Text := User.Crypto.algSymmetric;
                symmetric.OnChange := OnChangeKey;
            end;
        finally
            EndUpdate;
        end;
        with mode.Items do
        try
            BeginUpdate;
            Clear;
            for I := Low (CRYPTO_MODE_STRING) + 1 to High (CRYPTO_MODE_STRING) do
                Add (CRYPTO_MODE_STRING [I]);
            with TRegisterDialog (Owner) do
            begin
                mode.Text := User.Crypto.modeSymmetric;
                mode.OnChange := OnChangeKey;
            end;
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
            with TRegisterDialog (Owner) do
            begin
                hash.Text := User.Crypto.algHash;
                hash.OnChange := OnChangeKey;
            end;
        finally
            EndUpdate;
        end;
        { ключи }
        PrivateKey.Button.OnClick := TRegisterDialog (Owner).OnGetKey;
        PrivateKey.Text := '';
        PublicKey.Text := '';
        { первый параметр - логин }
        if notEmpty (0,anArgs) then
            login.Text := toString (anArgs [0]);
    except on E: Exception do
        _raise (['GetData',ERR_TREGISTERCONTENT_GET_DATA,E],
                ['{054F09E2-C43E-406B-8ABE-B3E93AF29519}']);
    end;
end;

procedure TRegisterContent.SetData (anIndex: Integer);
begin
    try
    except on E: Exception do
        _raise (['SetData',ERR_TREGISTERCONTENT_SET_DATA,E],
                ['{1DF6AAE4-0C62-4FD4-A592-09226E9AB566}']);
    end;
end;

procedure TRegisterContent.btRandomTestClick (Sender: TObject);
begin
    try
        RandomTest (random.Text);
    except on E: Exception do
        _raise (['btRandomTestClick',ERR_TREGISTERCONTENT_TEST_RANDOM,E],
                ['{303F5087-699D-49CA-B4CC-50446834226C}']);
    end;
end;

procedure TRegisterContent.btAsymmetricTestClick (Sender: TObject);
begin
    try
        AsymmetricTest (asymmetric.Text);
    except on E: Exception do
        _raise (['btAsymmetricTestClick',ERR_TREGISTERCONTENT_TEST_ASYMMETRIC,E],
                ['{AD8BC542-4D45-4C96-BE07-D541510D7A76}']);
    end;
end;

procedure TRegisterContent.btSymmetricTestClick (Sender: TObject);
begin
    try
        SymmetricTest (symmetric.Text,mode.Text);
    except on E: Exception do
        _raise (['btSymmetricTestClick',ERR_TREGISTERCONTENT_TEST_SYMMETRIC,E],
                ['{EC5462DE-6650-49CC-8D69-B3B3C88B4EBA}']);
    end;
end;

procedure TRegisterContent.btHashTestClick (Sender: TObject);
begin
    try
        HashTest (hash.Text);
    except on E: Exception do
        _raise (['btHashTestClick',ERR_TREGISTERCONTENT_TEST_HASH,E],
                ['{938219D0-038C-4BB6-BE95-1C8E072CAAD0}']);
    end;
end;


end.
