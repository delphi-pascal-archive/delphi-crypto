unit uSymmetricTestDialog;
{******************************************************************************}
{*  Symmetric Test Dialog Unit                                                *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    {$I '../crypto/PRNG.uses.inc'}
    {$I '../crypto/AES.uses.inc'}
    {$I '../crypto/Anubis.uses.inc'}
    {$I '../crypto/Serpent.uses.inc'}
    {$I '../crypto/Shacal.uses.inc'}
    {$I '../crypto/BlowFish.uses.inc'}
    {$I '../crypto/TwoFish.uses.inc'}
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
    TSymmetricTestDialog = class;
    TSymmetricTestContent = class;
{$M-}

{ диалоговое окно тестирования симметричного шифра }
{$M+}
    CSymmetricTestDialog = class of TSymmetricTestDialog;
    PSymmetricTestDialog = ^TSymmetricTestDialog;
    TSymmetricTestDialog = class (TProtoDialog)
    private
        f_SymmetricAlgoritm: String;
        f_SymmetricMode: String;
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
        property SymmetricAlgoritm : String read f_SymmetricAlgoritm write f_SymmetricAlgoritm;
        property SymmetricMode : String read f_SymmetricMode write f_SymmetricMode;
    end;
{$M-}

{ содержимое }
{$M+}
    CSymmetricTestContent = class of TSymmetricTestContent;
    PSymmetricTestContent = ^TSymmetricTestContent;
    TSymmetricTestContent = class (TFrame)
        pnlBackGround: TsPanel;
        pnlLabels: TsPanel;
        lbKey: TsLabel;
        lbSeed: TsLabel;
        lbPlain: TsLabel;
        lbCipher: TsLabel;
        pnlControls: TsPanel;
        btGenerateKey: TsSpeedButton;
        key: TsRichEdit;
        seed: TsRichEdit;
        plain: TsRichEdit;
        Cipher: TsRichEdit;
        procedure btGenerateKeyClick(Sender: TObject);
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
    ERR_TSYMMETRICTESTDIALOG_AUTO_TEST = 'Ошибка тестирования симметричного шифра!';
    ERR_TSYMMETRICTESTDIALOG_ENCRYPT   = 'Ошибка шифрования!';
    ERR_TSYMMETRICTESTDIALOG_DECRYPT   = 'Ошибка дешифровки!';
    ERR_TSYMMETRICTESTDIALOG_TEST      = 'Ошибка тестирования симметричного шифра ''%s'' в режиме ''%s''!';

resourcestring
    ERR_TSYMMETRICTESTCONTENT_CREATE       = 'Ошибка создания!';
    ERR_TSYMMETRICTESTCONTENT_DESTROY      = 'Ошибка уничтожения!';
    ERR_TSYMMETRICTESTCONTENT_GET_DATA     = 'Ошибка чтения данных!';
    ERR_TSYMMETRICTESTCONTENT_SET_DATA     = 'Ошибка записи данных!';
    ERR_TSYMMETRICTESTCONTENT_GENERATE_KEY = 'Ошибка генерации ключа!';

implementation

{$R *.dfm}

constructor TSymmetricTestDialog.Create (anArgs: array of const);
begin
    try
        inherited Create ([ 'тестирование симметричного шифра', dlgBinary ],
                          [ _(['OK',btnOk,mrAutoTest]),
                            _(['Зашифровать',btnCustom,mrEncrypt,'alLeft']),
                            _(['Расшифровать',btnCustom,mrDecrypt,'alLeft']) ],
                          NIL);
        Content := TSymmetricTestContent.Create (self,anArgs);
        minHeight := 64 + TSymmetricTestContent (Content).Height + 8 + pnlButtons.Height;
        maxHeight := minHeight;
        Height := minHeight;
        minWidth := 495;
        Width := minWidth;
        with TSymmetricTestContent (Content) do
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
                ['{14989C26-A505-45BC-BBEA-701A7384067B}']);
    end;
end;

destructor TSymmetricTestDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{71D50E81-D769-434B-8D2E-2FDF6028DB46}']);
    end;
end;

procedure TSymmetricTestDialog.GetData (anArgs: array of const);
begin
    try
        inherited GetData ([ 'тестирование симметричного шифра', dlgBinary ],
                           [ _(['OK',btnOk,mrAutoTest]),
                            _(['Зашифровать',btnCustom,mrEncrypt,'alLeft']),
                            _(['Расшифровать',btnCustom,mrDecrypt,'alLeft']) ]);
        { первый параметр - алгоритм симметричного шифрования }
        if notEmpty (0,anArgs) then
            SymmetricAlgoritm := toString (anArgs [0]);
        { первый параметр - режим симметричного шифрования }
        if notEmpty (1,anArgs) then
            SymmetricMode := toString (anArgs [1]);
        Caption := Format ('%s : %s - %s',[Caption,SymmetricAlgoritm,SymmetricMode]);
        with TSymmetricTestContent (Content) do
            GetData (anArgs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{37A1F6AA-DC3B-4DEB-9BB8-D68B54007150}']);
    end;
end;

procedure TSymmetricTestDialog.SetData (anIndex: Integer);
begin
    try
        with TSymmetricTestContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrAutoTest : doAutoTest;
            mrEncrypt  : doEncrypt;
            mrDecrypt  : doDecrypt;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{013C55DA-1945-4163-8643-0E2D5A29D0B8}']);
    end;
end;

procedure TSymmetricTestDialog.doAutoTest;
begin
    try
        if not TCryptoKernel.TestSymmetric ([SymmetricAlgoritm,SymmetricMode]) then
            raise Exception.CreateFmt (ERR_TSYMMETRICTESTDIALOG_TEST,
                                       [SymmetricAlgoritm,SymmetricMode]);
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doAutoTest',ERR_TSYMMETRICTESTDIALOG_AUTO_TEST,E],
                ['{7E476D0C-953F-420B-B101-05D0EFD7C834}']);
    end;
end;

procedure TSymmetricTestDialog.doEncrypt;
var
    alg  : TCryptoType;
    mode : TCryptoMode;
begin
    try
        with TSymmetricTestContent (Content) do
        begin
            alg := StrToCryptoType (SymmetricAlgoritm);
            mode := StrToCryptoMode (SymmetricMode);
            Cipher.Clear;
            case mode of
                cmECB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256      : Cipher.Text := TestEncryptAESECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                              );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256   : Cipher.Text := TestEncryptAnubisECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256  : Cipher.Text := TestEncryptSerpentECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            ctShacal      : Cipher.Text := TestEncryptShacalECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctBlowFish    : Cipher.Text := TestEncryptBlowFishECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                   );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256  : Cipher.Text := TestEncryptTwoFishECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            else            raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmCBC : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256      : Cipher.Text := TestEncryptAESCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                              );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256   : Cipher.Text := TestEncryptAnubisCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256  : Cipher.Text := TestEncryptSerpentCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            ctShacal      : Cipher.Text := TestEncryptShacalCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctBlowFish    : Cipher.Text := TestEncryptBlowFishCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                   );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256  : Cipher.Text := TestEncryptTwoFishCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            else            raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmCFB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256      : Cipher.Text := TestEncryptAESCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                              );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256   : Cipher.Text := TestEncryptAnubisCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256  : Cipher.Text := TestEncryptSerpentCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            ctShacal      : Cipher.Text := TestEncryptShacalCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctBlowFish    : Cipher.Text := TestEncryptBlowFishCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                   );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256  : Cipher.Text := TestEncryptTwoFishCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            else            raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmOFB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256      : Cipher.Text := TestEncryptAESOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                               StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                              );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256   : Cipher.Text := TestEncryptAnubisOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256  : Cipher.Text := TestEncryptSerpentOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            ctShacal      : Cipher.Text := TestEncryptShacalOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                 );
                            ctBlowFish    : Cipher.Text := TestEncryptBlowFishOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                    StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                   );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256  : Cipher.Text := TestEncryptTwoFishOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                   StrReplace ( Trim (Plain.Text), #13#10, '' )
                                                                                  );
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
        end;
    except on E: Exception do
        _raise (['doEncrypt',ERR_TSYMMETRICTESTDIALOG_ENCRYPT,E],
                ['{33335BE5-CD21-44EF-B999-65EDB633AB97}']);
    end;
end;

procedure TSymmetricTestDialog.doDecrypt;
var
    alg  : TCryptoType;
    mode : TCryptoMode;
begin
    try
        with TSymmetricTestContent (Content) do
        begin
            alg := StrToCryptoType (SymmetricAlgoritm);
            mode := StrToCryptoMode (SymmetricMode);
            Plain.Clear;
            case mode of
                cmECB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256     : Plain.Text := TestDecryptAESECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                            );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256  : Plain.Text := TestDecryptAnubisECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256 : Plain.Text := TestDecryptSerpentECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            ctShacal     : Plain.Text := TestDecryptShacalECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctBlowFish   : Plain.Text := TestDecryptBlowFishECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                 );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256 : Plain.Text := TestDecryptTwoFishECB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmCBC : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256     : Plain.Text := TestDecryptAESCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                            );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256  : Plain.Text := TestDecryptAnubisCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256 : Plain.Text := TestDecryptSerpentCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            ctShacal     : Plain.Text := TestDecryptShacalCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctBlowFish   : Plain.Text := TestDecryptBlowFishCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                 );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256 : Plain.Text := TestDecryptTwoFishCBC ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmCFB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256     : Plain.Text := TestDecryptAESCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                            );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256  : Plain.Text := TestDecryptAnubisCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256 : Plain.Text := TestDecryptSerpentCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            ctShacal     : Plain.Text := TestDecryptShacalCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctBlowFish   : Plain.Text := TestDecryptBlowFishCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                 );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256 : Plain.Text := TestDecryptTwoFishCFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                cmOFB : case alg of
                            ctAES128,
                            ctAES192,
                            ctAES256     : Plain.Text := TestDecryptAESOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                             StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                            );
                            ctAnubis128,
                            ctAnubis192,
                            ctAnubis256  : Plain.Text := TestDecryptAnubisOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctSerpent128,
                            ctSerpent192,
                            ctSerpent256 : Plain.Text := TestDecryptSerpentOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            ctShacal     : Plain.Text := TestDecryptShacalOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                               );
                            ctBlowFish   : Plain.Text := TestDecryptBlowFishOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                  StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                 );
                            ctTwoFish128,
                            ctTwoFish192,
                            ctTwoFish256 : Plain.Text := TestDecryptTwoFishOFB ( StrReplace ( Trim (Key.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Seed.Text), #13#10, '' ),
                                                                                 StrReplace ( Trim (Cipher.Text), #13#10, '' )
                                                                                );
                            else           raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
                        end;
                else    raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_MODE);
            end;
        end;
    except on E: Exception do
        _raise (['doDecrypt',ERR_TSYMMETRICTESTDIALOG_DECRYPT,E],
                ['{40DE47CF-251C-45A5-B086-537E4DB478FA}']);
    end;
end;

class function TSymmetricTestDialog.Open (anArgs: array of const) : Integer;
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
                ['{D4DBBAC3-08BA-4F20-A627-1F9E02675622}']);
    end;
end;

class function TSymmetricTestDialog.Execute (anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{6F108927-DB55-4A02-8192-10B63C08887C}']);
    end;
end;

class procedure TSymmetricTestContent._raise (anArgs: array of const;
                                              const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TSymmetricTestContent._raise (anArgs: array of const;
                                              anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TSymmetricTestContent.Create (anOwner: TProtoDialog;
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
        _raise (['Create',ERR_TSYMMETRICTESTCONTENT_CREATE,E],
                ['{4311B692-4A53-493D-A065-92901A4F9ED2}']);
    end;
end;

destructor TSymmetricTestContent.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TSYMMETRICTESTCONTENT_DESTROY,E],
                ['{7717D58E-1512-4DF5-85AD-FBDE76CA8714}']);
    end;
end;

procedure TSymmetricTestContent.FrameResize (Sender: TObject);
begin
    try
        btGenerateKey.Left := Key.Left + Key.Width;
    except on E: Exception do
        _raise (['FrameResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{B439D424-F961-4953-9087-4F78A753C128}']);
    end;
end;

procedure TSymmetricTestContent.GetData (anArgs: array of const);
begin
    try
        btGenerateKeyClick (NIL);
    except on E: Exception do
        _raise (['GetData',ERR_TSYMMETRICTESTCONTENT_GET_DATA,E],
                ['{59CD9615-BE28-4E5A-B40B-FAC79647A1EA}']);
    end;
end;

procedure TSymmetricTestContent.SetData (anIndex: Integer);
begin
    try
    except on E: Exception do
        _raise (['SetData',ERR_TSYMMETRICTESTCONTENT_SET_DATA,E],
                ['{D586F79C-0FAA-4D4D-99E0-15B26E1211DA}']);
    end;
end;

procedure TSymmetricTestContent.btGenerateKeyClick (Sender: TObject);
var
    alg   : TCryptoType;
    mode  : TCryptoMode;
    BlocSize : WORD;
    k     : String;
begin
    try
        with TSymmetricTestDialog (Owner) do
        try
            alg := StrToCryptoType (SymmetricAlgoritm);
            case alg of
                ctAES128,
                ctAES192,
                ctAES256      : BlocSize := 16;
                ctAnubis128,
                ctAnubis192,
                ctAnubis256   : BlocSize := 16;
                ctSerpent128,
                ctSerpent192,
                ctSerpent256  : BlocSize := 16;
                ctShacal      : BlocSize := 32;
                ctBlowFish    : BlocSize := 8;
                ctTwoFish128,
                ctTwoFish192,
                ctTwoFish256  : BlocSize := 16;
                else            raise Exception.Create (ERR_TCRYPTOKERNEL_UNKNOWN_SYMMETRIC_CRYPTO_TYPE);
            end;
            mode := StrToCryptoMode (SymmetricMode);
            TCryptoKernel.GenerateKey (k,[SymmetricAlgoritm]);
            Key.Clear;
            Key.Text := k;
            Plain.Clear;
            Plain.Text := LPad ('',BlocSize*2,'0');
            Plain.MaxLength := BlocSize*2;
            Seed.Clear;
            Seed.Enabled := ( mode <> cmECB );
            if Seed.Enabled then
            begin
                Seed.Text := LPad ('',BlocSize*2,'0');
                Seed.MaxLength := BlocSize*2;
            end;
            Cipher.Clear;
        finally
            _FillChar ( k, Length (k), $00 );
        end;
    except on E: Exception do
        _raise (['btGenerateKeyClick',ERR_TSYMMETRICTESTCONTENT_GENERATE_KEY,E],
                ['{C3942FD4-DA42-4C5C-B5F3-E4C2F69E257D}']);
    end;
end;


end.
