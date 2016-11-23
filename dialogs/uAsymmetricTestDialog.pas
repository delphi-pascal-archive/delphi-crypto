unit uAsymmetricTestDialog;
{******************************************************************************}
{*  Asymmetric Test Dialog Unit                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    {$I '../crypto/PRNG.uses.inc'}
    {$I '../crypto/RSA.uses.inc'}
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, ImgList,
    acPNG, acAlphaImageList,
    sSkinProvider, sSkinManager,
    sPanel, sPageControl, sLabel, sButton, sBitBtn, sSpeedButton,
    Mask, sMaskEdit, sRichEdit,
    Crypto, Kernel, DialogClasses,
    uProtoDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

{ кнопки }
const
    btAutoTest = 0;
    btEncrypt  = 1;
    btDecrypt  = 2;

{ действия }
const
    mrAutoTest = 14;
    mrEncrypt  = 15;
    mrDecrypt  = 16;

type
{$M+}
    TAsymmetricTestDialog = class;
    TAsymmetricTestContent = class;
{$M-}

{ диалоговое окно тестирования асимметричного шифра }
{$M+}
    CAsymmetricTestDialog = class of TAsymmetricTestDialog;
    PAsymmetricTestDialog = ^TAsymmetricTestDialog;
    TAsymmetricTestDialog = class (TProtoDialog)
    private
        f_AsymmetricAlgoritm: String;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure doAutoTest; virtual;
        procedure doEncrypt; virtual;
        procedure doDecrypt; virtual;
    public
        constructor Create (anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const) : Boolean; overload; virtual;
    public
        property AsymmetricAlgoritm : String read f_AsymmetricAlgoritm write f_AsymmetricAlgoritm;
    end;
{$M-}

{ содержимое }
{$M+}
    CAsymmetricTestContent = class of TAsymmetricTestContent;
    PAsymmetricTestContent = ^TAsymmetricTestContent;
    TAsymmetricTestContent = class (TFrame)
        tabs: TsPageControl;
        tbRSA: TsTabSheet;
        pnlRSAControls: TsPanel;
        btGenerateKeysRSA: TsSpeedButton;
        KeyE: TsRichEdit;
        KeyN: TsRichEdit;
        KeyD: TsRichEdit;
        seed: TsRichEdit;
        plain: TsRichEdit;
        Cipher: TsRichEdit;
        pnlRSALabels: TsPanel;
        lbE: TsLabel;
        lbN: TsLabel;
        lbD: TsLabel;
        lbSeed: TsLabel;
        lbPlain: TsLabel;
        lbCipher: TsLabel;
        procedure btGenerateKeysRSAClick(Sender: TObject);
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

resourcestring
    ERR_TASYMMETRICTESTDIALOG_AUTO_TEST = 'Ошибка тестирования асимметричного шифра!';
    ERR_TASYMMETRICTESTDIALOG_ENCRYPT   = 'Ошибка шифрования!';
    ERR_TASYMMETRICTESTDIALOG_DECRYPT   = 'Ошибка дешифровки!';
    ERR_TASYMMETRICTESTDIALOG_TEST      = 'Ошибка тестирования асимметричного шифра ''%s''!';

resourcestring
    ERR_TASYMMETRICTESTCONTENT_CREATE        = 'Ошибка создания!';
    ERR_TASYMMETRICTESTCONTENT_DESTROY       = 'Ошибка уничтожения!';
    ERR_TASYMMETRICTESTCONTENT_GET_DATA      = 'Ошибка чтения данных!';
    ERR_TASYMMETRICTESTCONTENT_SET_DATA      = 'Ошибка записи данных!';
    ERR_TASYMMETRICTESTCONTENT_GENERATE_KEYS = 'Ошибка генерации ключевой пары!';

implementation

{$R *.dfm}

constructor TAsymmetricTestDialog.Create (anArgs: array of const);
begin
    try
        inherited Create ([ 'тестирование асимметричного шифра', dlgBinary ],
                          [ _(['OK',btnOk,mrAutoTest]),
                            _(['Зашифровать',btnCustom,mrEncrypt,'alLeft']),
                            _(['Расшифровать',btnCustom,mrDecrypt,'alLeft']) ],
                          NIL);
        Content := TAsymmetricTestContent.Create (self,anArgs);
        minHeight := 64 + TAsymmetricTestContent (Content).Height + 8 + pnlButtons.Height;
        maxHeight := minHeight;
        Height := minHeight;
        minWidth := 495;
        Width := minWidth;
        with TAsymmetricTestContent (Content) do
        begin
            Parent := pnlBack;
            Align := alClient;
            AlignWithMargins := TRUE;
            Margins.Left := 10;
            Margins.Top := 0;
            Margins.Right := 10;
            Margins.Bottom := 8;
        end;
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{79D9D029-C1CA-452E-A626-CB799B43849F}']);
    end;
end;

destructor TAsymmetricTestDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{3F104E29-8B43-44DB-BB41-C523C84BE78C}']);
    end;
end;

procedure TAsymmetricTestDialog.GetData (anArgs: array of const);
begin
    try
        inherited GetData ([ 'тестирование асимметричного шифра', dlgBinary ],
                           [ _(['OK',btnOk,mrAutoTest]),
                            _(['Зашифровать',btnCustom,mrEncrypt,'alLeft']),
                            _(['Расшифровать',btnCustom,mrDecrypt,'alLeft']) ]);
        { первый параметр - алгоритм асимметричного шифрования }
        if notEmpty (0,anArgs) then
            AsymmetricAlgoritm := toString (anArgs [0]);
        Caption := Format ('%s : %s',[Caption,AsymmetricAlgoritm]);
        with TAsymmetricTestContent (Content) do
            GetData (anArgs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{9B8B205C-19E6-467D-B6D7-14255DB5C526}']);
    end;
end;

procedure TAsymmetricTestDialog.SetData (anIndex: Integer);
begin
    try
        with TAsymmetricTestContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrAutoTest : doAutoTest;
            mrEncrypt  : doEncrypt;
            mrDecrypt  : doDecrypt;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{586E2C6F-688E-4744-8C5F-BCB3DA9CDB21}']);
    end;
end;

procedure TAsymmetricTestDialog.doAutoTest;
begin
    try
        if not TCryptoKernel.TestAsymmetric ([AsymmetricAlgoritm]) then
            raise Exception.CreateFmt (ERR_TASYMMETRICTESTDIALOG_TEST,
                                       [AsymmetricAlgoritm]);
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doAutoTest',ERR_TASYMMETRICTESTDIALOG_AUTO_TEST,E],
                ['{40CE08C7-9611-416D-B845-BFAFA1310112}']);
    end;
end;

procedure TAsymmetricTestDialog.doEncrypt;
begin
    try
        with TAsymmetricTestContent (Content) do
        begin
            Cipher.Clear;
            if (tabs.ActivePage = tbRSA) then
            Cipher.Text := TestEncryptRSA ( StrReplace ( Trim (KeyE.Text), #13#10, '' ),
                                            StrReplace ( Trim (KeyN.Text), #13#10, '' ),
                                            StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                            StrReplace ( Trim (Plain.Text), #13#10, '' )
                                           );
        end;
    except on E: Exception do
        _raise (['doEncrypt',ERR_TASYMMETRICTESTDIALOG_ENCRYPT,E],
                ['{E47F6922-FC19-4741-9CB3-33AF64178F95}']);
    end;
end;

procedure TAsymmetricTestDialog.doDecrypt;
begin
    try
        with TAsymmetricTestContent (Content) do
        begin
            Plain.Clear;
            if (tabs.ActivePage = tbRSA) then
            Plain.Text := TestDecryptRSA ( StrReplace ( Trim (KeyD.Text), #13#10, '' ),
                                           StrReplace ( Trim (KeyN.Text), #13#10, '' ),
                                           StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                          );
        end;
    except on E: Exception do
        _raise (['doDecrypt',ERR_TASYMMETRICTESTDIALOG_DECRYPT,E],
                ['{345350AD-037D-42CF-98BB-138890735D57}']);
    end;
end;

class function TAsymmetricTestDialog.Open (anArgs: array of const) : Integer;
begin
    try
        with Create (anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{90FAA12D-37BC-416A-90DC-7B70339F1E85}']);
    end;
end;

class function TAsymmetricTestDialog.Execute (anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{33EAF83B-ACC6-4E43-94DA-83A14A8505CF}']);
    end;
end;

class procedure TAsymmetricTestContent._raise (anArgs: array of const;
                                               const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TAsymmetricTestContent._raise (anArgs: array of const;
                                               anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TAsymmetricTestContent.Create (anOwner: TProtoDialog;
                                           anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited Create (anOwner);
        for I := 0 to pnlRSAControls.ControlCount - 1 do
            if ( pnlRSAControls.Controls [I] is TCustomEdit ) then
                TEdit (pnlRSAControls.Controls [I]).PopupMenu := TProtoDialog (Owner).mnTextPopup;
    except on E: Exception do
        _raise (['Create',ERR_TASYMMETRICTESTCONTENT_CREATE,E],
                ['{F8A38E2C-B30B-4BB4-AF6E-0BF5419CCD51}']);
    end;
end;

destructor TAsymmetricTestContent.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TASYMMETRICTESTCONTENT_DESTROY,E],
                ['{EE066A7D-A417-49A3-8E65-E27F7614B61F}']);
    end;
end;

procedure TAsymmetricTestContent.FrameResize (Sender: TObject);
begin
    try
        btGenerateKeysRSA.Left := KeyE.Left + KeyE.Width;
    except on E: Exception do
        _raise (['FrameResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{980518D6-A38D-4121-9699-E2069AB2FB83}']);
    end;
end;

procedure TAsymmetricTestContent.GetData (anArgs: array of const);
var
    alg : TPKCryptoType;
begin
    try
        with TAsymmetricTestDialog (Owner) do
        begin
            alg := StrToPKCryptoType (AsymmetricAlgoritm);
            case alg of
                pkctRSA1024,
                pkctRSA2048,
                pkctRSA4096,
                pkctRSA8192 : begin
                                  tabs.ActivePage := tbRSA;
                                  btGenerateKeysRSAClick (NIL);
                              end
                else          raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_ASYMMETRIC_CRYPTO_TYPE);
            end;
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TASYMMETRICTESTCONTENT_GET_DATA,E],
                ['{34BAB508-F3E8-4F93-9784-BB8C457DFEA3}']);
    end;
end;

procedure TAsymmetricTestContent.SetData (anIndex: Integer);
begin
    try
    except on E: Exception do
        _raise (['SetData',ERR_TASYMMETRICTESTCONTENT_SET_DATA,E],
                ['{02B57ABE-8D6C-40FD-98F3-D4DE1634DACB}']);
    end;
end;

procedure TAsymmetricTestContent.btGenerateKeysRSAClick (Sender: TObject);
var
    pub     : String;
    pub_e   : String;
    eLength : WORD;
    pub_n   : String;
    nLength : WORD;
    prv     : String;
    prv_d   : String;
    dLength : WORD;
    prv_n   : String;
begin
    try
        with TAsymmetricTestDialog (Owner) do
        try
            pub := '';
            prv := '';
            TCryptoKernel.GenerateKeys (pub,prv,[AsymmetricAlgoritm]);
            eLength := HexToInt ( Copy (pub,1,4) );
            pub_e := Copy ( pub, 5, eLength );
            nLength := HexToInt ( Copy (pub,eLength+5,4) );
            pub_n := Copy ( pub, eLength+9, nLength );
            dLength := HexToInt ( Copy (prv,1,4) );
            prv_d := Copy ( prv, 5, dLength );
            nLength := HexToInt ( Copy (prv,dLength+5,4) );
            prv_n := Copy ( prv, dLength+9, nLength );
            KeyN.Clear;
            KeyN.Text := pub_n;
            KeyE.Clear;
            KeyE.Text := pub_e;
            KeyD.Clear;
            KeyD.Text := prv_d;
            Cipher.Clear;
            Plain.Clear;
            Plain.Text := LPad ('',Length (pub_n) div 4,'0');
            Plain.MaxLength := Length (pub_n) div 4;
            Seed.Clear;
            Seed.Text := LPad ('',Length (pub_n),'0');
            Seed.MaxLength := Length (pub_n);
        finally
            _FillChar ( pub, Length (pub), $00 );
            _FillChar ( prv, Length (prv), $00 );
            _FillChar ( pub_e, Length (pub_e), $00 );
            _FillChar ( pub_n, Length (pub_n), $00 );
            nLength := 0;
            eLength := 0;
        end;
    except on E: Exception do
        _raise (['btGenerateKeysRSAClick',ERR_TASYMMETRICTESTCONTENT_GENERATE_KEYS,E],
                ['{F49B8911-88F7-45D4-8620-F8A572B6FF1E}']);
    end;
end;


end.
