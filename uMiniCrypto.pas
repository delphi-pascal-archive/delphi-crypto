unit uMiniCrypto;
{******************************************************************************}
{*  MiniCrypto Form Unit                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011-2012                                        *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ExtCtrls,
    ComCtrls, StdCtrls, Forms, Menus, ActnList, StdActns, Dialogs,
    ToolWin, AppEvnts,
    sSkinManager, sSkinProvider,
    ImgList, acAlphaImageList, jpeg, pngimage, acPNG,
    sPanel, sPageControl, sTabControl, sGroupBox, sScrollBox, sStatusBar, acCoolBar,
    Buttons, sBitBtn, sSpeedButton,
    sEdit, sMemo, sRichEdit, RxRichEd,
    Mask, sMaskEdit, sTooledit, sSpinEdit,
    sCustomComboEdit, sComboBoxes, sComboBox,
    sLabel, sColorSelect, sTreeView, sListView, sGauge, sCheckBox;

type
    TfmMiniCrypto = class (TForm)
        SkinProvider: TsSkinProvider;
        SkinBlack: TsSkinManager;
        AppEvents: TApplicationEvents;
        imgIcons: TsAlphaImageList;
    { actions }
        lstActions: TActionList;
        actTextCopy: TEditCopy;
        actTextCut: TEditCut;
        actTextPaste: TEditPaste;
        actTextDelete: TEditDelete;
        actTextSelectAll: TEditSelectAll;
    { menu }
        mnTextPopup: TPopupMenu;
        mnItemCopy: TMenuItem;
        mnItemCut: TMenuItem;
        mnItemPaste: TMenuItem;
        mnItemDelete: TMenuItem;
        mnItemBreak: TMenuItem;
        mnItemSelectAll: TMenuItem;
    { tabs }
        tabs: TsPageControl;
        { main }
        tbMain: TsTabSheet;
        Password: TsMaskEdit;
        Plain: TsRichEdit;
        Cipher: TsRichEdit;
        btEncrypt: TsSpeedButton;
        btDecrypt: TsSpeedButton;
        { crypto }
        tbCrypto: TsTabSheet;
        pnlCryptoControls: TsPanel;
        random: TsComboBox;
        symmetric: TsComboBox;
        mode: TsComboBox;
        hash: TsComboBox;
        cbOldVersion: TsCheckBox;
        pnlCryptoLabels: TsPanel;
        btRandomTest: TsSpeedButton;
        btSymmetricTest: TsSpeedButton;
        btHashTest: TsSpeedButton;
        lbRandom: TsLabel;
        lbSymmetric: TsLabel;
        lbMode: TsLabel;
        lbHash: TsLabel;
        lbOldVersion: TsLabel;
        { about }
        tbAbout: TsTabSheet;
        imgAbout: TImage;
        lbAuthor: TsLabel;
        lbAbout: TsLabel;
        lbText: TsLabel;
    { events }
        procedure FormCreate (Sender: TObject);
        procedure FormDestroy (Sender: TObject);
        procedure FormResize (Sender: TObject);
        procedure AppEventsHint (Sender: TObject);
        procedure btHashTestClick (Sender: TObject);
        procedure btSymmetricTestClick (Sender: TObject);
        procedure btRandomTestClick (Sender: TObject);
        procedure cbOldVersionClick (Sender: TObject);
        procedure PasswordKeyDown (Sender: TObject; var Key: Word;
                                   Shift: TShiftState);
        procedure btDecryptClick (Sender: TObject);
        procedure btEncryptClick (Sender: TObject);
    protected
        procedure FileDropped (var Msg: TMessage); message WM_DROPFILES;
    end;

var
    MainForm: TfmMiniCrypto;

implementation

{$R *.dfm}

uses
    ShellAPI,
    DialogClasses,
    VarRecs,
    Strings,
    Versions,
    EClasses,
    Crypto,
    BBCode;

procedure TfmMiniCrypto.FileDropped (var Msg: TMessage);
var
     FileName : String;
     Ext      : String;
     S        : array [0..256] of Char;
     Count    : Integer;
     L        : Cardinal;
     Control  : TWinControl;
begin
     Count := DragQueryFile (Msg.WParam,$FFFFFFFF,S,L);
     if ( Count > 0 ) then
     begin
         { get first file (0) }
         DragQueryFile( THandle (Msg.WParam), 0, S, SizeOf (S) ) ;
         try
             FileName := StrPas (S);
             if FileExists (FileName) then
             begin
                 Ext := LowerCase ( ExtractFileExt (FileName) );
                 if isEmpty (Ext) or ( Ext = '.txt' ) then
                 begin
                     Control := FindControl ( WindowFromPoint (Mouse.CursorPos) );
                     if ( Control = Cipher ) then
                     begin
                         Cipher.Clear;
                         Cipher.Lines.LoadFromFile (S);
                     end
                     else if ( Control = Plain ) then
                     begin
                         Plain.Clear;
                         Plain.Lines.LoadFromFile (S);
                     end;
                 end
                 else
                     raise Exception.CreateFmt ('Недопустимый формат файла: ''*%s''!'#13#10+
                                                'Разрешен прием текстовых файлов: ''untyped'', ''*.txt''',
                                                [Ext]);
             end
             else
                 raise Exception.CreateFmt ('Файл ''%s'' не найден!',[FileName]);
         finally
             DragFinish ( THandle (Msg.WParam) );
         end;
     end;
end;

procedure TfmMiniCrypto.AppEventsHint(Sender: TObject);
begin
    Caption := Application.Hint;
end;

procedure TfmMiniCrypto.btDecryptClick (Sender: TObject);
var
    genRandom : TRandomType;
    algSymm   : TCryptoAlgoritm;
    modeSymm  : TCryptoMode;
    algHash   : THashAlgoritm;
    S         : String;
    pass      : String;
    sgn       : String;
    cph       : String;
    msg       : String;
    I         : Integer;
begin
    try
        if ( Trim (Password.Text) = '' ) then
            raise Exception.Create ('Введите пароль!');
        if not cbOldVersion.Checked then
        begin
            genRandom := StrToRandomType (random.Text);
            algSymm := StrToCryptoAlgoritm (symmetric.Text);
            modeSymm := StrToCryptoMode (mode.Text);
            algHash := StrToHashAlgoritm (hash.Text);
            if not TCryptoKernel.TestSymmetric (algSymm,modeSymm) then
                raise Exception.CreateFmt ('Ошибка тестирования %s в режиме %s',[symmetric.Text,mode.Text]);
            if not TCryptoKernel.TestHash (algHash) then
                raise Exception.CreateFmt ('Ошибка тестирования %s',[hash.Text]);
            S := Trim (Cipher.Text);
            S := StrReplace (S,#32,'');
            S := StrReplace (S,#13#10,'');
            try
                pass := TCryptoKernel.Hash ( Trim (Password.Text),
                                             TCryptoKernel.Hash ( IntToHex (Length (Trim (Password.Text)), 4), algHash ),
                                             algHash );
                sgn := Copy ( S, Length (S) - Length (pass) + 1, Length (pass) );
                cph := Copy ( S, 1, Length (S) - Length (pass) );
                msg := TCryptoKernel.Decrypt (cph,pass,algSymm,modeSymm);
                Plain.Clear;
                if not ( TCryptoKernel.HMAC (msg,pass,algHash) = sgn ) then
                    raise Exception.Create ('Ошибка криптосистемы!:'#13#10'Ошибка проверки цифровой подписи!');
                //Plain.Text := msg;
                InsertBBCode ( Plain, msg );
            finally
                _FillChar ( pass, Length (pass), $00 );
                _FillChar ( sgn, Length (sgn), $00 );
                _FillChar ( cph, Length (cph), $00 );
                _FillChar ( msg, Length (msg), $00 );
                _FillChar ( S, Length (S), $00 );
            end;
        end
        { поддержка ранних версий }
        else
        begin
            genRandom := StrToRandomType ('ISAAC');
            algSymm := StrToCryptoAlgoritm ('AES256-TwoFish256-Serpent256');
            modeSymm := StrToCryptoMode ('OFB');
            algHash := StrToHashAlgoritm ('SHA256');
            if not TCryptoKernel.TestSymmetric (algSymm,modeSymm) then
                raise Exception.CreateFmt ('Ошибка тестирования %s в режиме %s',[symmetric.Text,mode.Text]);
            if not TCryptoKernel.TestHash (algHash) then
                raise Exception.CreateFmt ('Ошибка тестирования %s',[hash.Text]);
            S := Trim (Cipher.Text);
            S := StrReplace (S,#32,'');
            S := StrReplace (S,#13#10,'');
            try
                pass := TCryptoKernel.Hash (Password.Text,algHash);
                for I := 0 to 255 do
                    pass := TCryptoKernel.Hash (  pass + TCryptoKernel.Hash ( Format('%d',[I]), algHash ), algHash  );
                sgn := Copy ( S, Length (S) - Length (pass) + 1, Length (pass) );
                cph := Copy ( S, 1, Length (S) - Length (pass) );
                msg := TCryptoKernel.Decrypt (cph,pass,algSymm,modeSymm);
                Plain.Clear;
                if not ( TCryptoKernel.HMAC (msg,pass,algHash) = sgn ) then
                    raise Exception.Create ('Ошибка криптосистемы!:'#13#10'Ошибка проверки цифровой подписи!');
                //Plain.Text := msg;
                InsertBBCode ( Plain, msg );
            finally
                _FillChar ( pass, Length (pass), $00 );
                _FillChar ( sgn, Length (sgn), $00 );
                _FillChar ( cph, Length (cph), $00 );
                _FillChar ( msg, Length (msg), $00 );
                _FillChar ( S, Length (S), $00 );
            end;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('Ошибка дешифровки: '#13#10'%s',[E.Message]);
    end;
end;

procedure TfmMiniCrypto.btEncryptClick (Sender: TObject);
var
    genRandom : TRandomType;
    algSymm   : TCryptoAlgoritm;
    modeSymm  : TCryptoMode;
    algHash   : THashAlgoritm;
    S         : String;
    pass      : String;
    I         : Integer;
begin
    try
        if ( Trim (Password.Text) = '' ) then
            raise Exception.Create ('Введите пароль!');
        if not cbOldVersion.Checked then
        begin
            genRandom := StrToRandomType (random.Text);
            algSymm := StrToCryptoAlgoritm (symmetric.Text);
            modeSymm := StrToCryptoMode (mode.Text);
            algHash := StrToHashAlgoritm (hash.Text);
            if not TCryptoKernel.TestSymmetric (algSymm,modeSymm) then
                raise Exception.CreateFmt ('Ошибка тестирования %s в режиме %s',[symmetric.Text,mode.Text]);
            if not TCryptoKernel.TestHash (algHash) then
                raise Exception.CreateFmt ('Ошибка тестирования %s',[hash.Text]);
            S := Plain.Text;
            try
                pass := TCryptoKernel.Hash ( Trim (Password.Text),
                                             TCryptoKernel.Hash ( IntToHex (Length (Trim (Password.Text)), 4), algHash ),
                                             algHash );
                S := TCryptoKernel.Encrypt (S,pass,algSymm,modeSymm,genRandom) +
                     TCryptoKernel.HMAC (S,pass,algHash);
                Cipher.Clear;
                Cipher.Text := S;
            finally
                _FillChar ( pass, Length (pass), $00 );
                _FillChar ( S, Length (S), $00 );
            end;
        end
        { поддержка ранних версий }
        else
        begin
            genRandom := StrToRandomType ('ISAAC');
            algSymm := StrToCryptoAlgoritm ('AES256-TwoFish256-Serpent256');
            modeSymm := StrToCryptoMode ('OFB');
            algHash := StrToHashAlgoritm ('SHA256');
            if not TCryptoKernel.TestSymmetric (algSymm,modeSymm) then
                raise Exception.CreateFmt ('Ошибка тестирования %s в режиме %s',[symmetric.Text,mode.Text]);
            if not TCryptoKernel.TestHash (algHash) then
                raise Exception.CreateFmt ('Ошибка тестирования %s',[hash.Text]);
            S := Plain.Text;
            try
                pass := TCryptoKernel.Hash (Password.Text,algHash);
                for I := 0 to 255 do
                    pass := TCryptoKernel.Hash (  pass + TCryptoKernel.Hash ( Format('%d',[I]), algHash ), algHash  );
                S := TCryptoKernel.Encrypt (S,pass,algSymm,modeSymm,genRandom) +
                     TCryptoKernel.HMAC (S,pass,algHash);
                Cipher.Clear;
                Cipher.Text := S;
            finally
                _FillChar ( pass, Length (pass), $00 );
                _FillChar ( S, Length (S), $00 );
            end;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('Ошибка шифрования: '#13#10'%s',[E.Message]);
    end;
end;

procedure TfmMiniCrypto.FormCreate (Sender: TObject);
var
    I : Integer;
begin
    Caption := ProductName;
    Hint := ProductName;
    { генератор псевдо-случайных чисел }
    with random.Items do
    try
        BeginUpdate;
        Clear;
        for I := Low (RANDOM_TYPE_STRING) + 1 to High (RANDOM_TYPE_STRING) do
            Add (RANDOM_TYPE_STRING [I]);
        random.Text := 'ISAAC';
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
        symmetric.Text := 'AES256-Serpent256-TwoFish256';
    finally
        EndUpdate;
    end;
    with mode.Items do
    try
        BeginUpdate;
        Clear;
        for I := Low (CRYPTO_MODE_STRING) + 1 to High (CRYPTO_MODE_STRING) do
            Add (CRYPTO_MODE_STRING [I]);
        mode.Text := 'OFB';
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
        hash.Text := 'SHA256';
    finally
        EndUpdate;
    end;
    { drag and drop }
    DragAcceptFiles (Handle,TRUE);
end;

procedure TfmMiniCrypto.FormDestroy (Sender: TObject);
begin
    { drag and drop }
    DragAcceptFiles (Handle,FALSE);
end;

procedure TfmMiniCrypto.cbOldVersionClick (Sender: TObject);
begin
    if TCheckBox (Sender).Checked then
    begin
        random.Text := 'ISAAC';
        symmetric.Text := 'AES256-Serpent256-TwoFish256';
        mode.Text := 'OFB';
        hash.Text := 'SHA256';
    end;
end;

procedure TfmMiniCrypto.FormResize (Sender: TObject);
begin
    Plain.Height := ( tbMain.Height - 48 ) div 2 - 4;
    Cipher.Height := ( tbMain.Height - 48 ) div 2 - 4;
end;

procedure TfmMiniCrypto.PasswordKeyDown (Sender: TObject; var Key: Word;
                                         Shift: TShiftState);
begin
    if ( Key = 13 ) then
    begin
        if ( Trim (Cipher.Text) = '' ) then
            btEncrypt.OnClick (NIL)
        else
            btDecrypt.OnClick (NIL);
    end;
end;

procedure TfmMiniCrypto.btRandomTestClick (Sender: TObject);
begin
    try
        if not RandomTest (random.Text) then
            raise Exception.CreateFmt ('Ошибка тестирования генератора псевдо-случайных чисел ''%s''!',
                                       [random.Text]);
    except on E: Exception do
        raise Exception.CreateFmt ('Ошибка тестирования: '#13#10'%s',[E.Message]);
    end;
end;

procedure TfmMiniCrypto.btSymmetricTestClick (Sender: TObject);
begin
    try
        if not SymmetricTest (symmetric.Text,mode.Text) then
            raise Exception.CreateFmt ('Ошибка тестирования симметричного шифра ''%s'' в режиме ''%s''!',
                                       [symmetric.Text,mode.Text]);
    except on E: Exception do
        raise Exception.CreateFmt ('Ошибка тестирования: '#13#10'%s',[E.Message]);
    end;
end;

procedure TfmMiniCrypto.btHashTestClick (Sender: TObject);
begin
    try
        if not HashTest (hash.Text) then
            raise Exception.CreateFmt ('Ошибка тестирования хэш-функции ''%s''!',
                                       [hash.Text]);
    except on E: Exception do
        raise Exception.CreateFmt ('Ошибка тестирования: '#13#10'%s',[E.Message]);
    end;
end;


end.
