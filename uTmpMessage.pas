unit uTmpMessage;
{******************************************************************************}
{*  Template Message Unit                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I 'std.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, RxRichEd, RichEdit,
  sFrameAdapter,
  sPanel, sScrollBox, sLabel, Buttons, sSpeedButton, acPNG, jpeg;

const
    clQuote = $f4f4f4;

type
    TMessageText = class (TRxRichEdit)
    private
         OwnParent : TWinControl;
    protected
         procedure Change; override;
    public
         constructor Create (anOwner: TWinControl; const aName: String); overload; virtual;
    end;

    CTmpMessage = class of TTmpMessage;
    PTmpMessage = ^TTmpMessage;
    TTmpMessage = class (TFrame)
        FrameAdapter: TsFrameAdapter;
        FpnlMsg: TsPanel;
        FimgBottomLeft: TImage;
        FimgBottomRight: TImage;
        FimgTop: TImage;
        FimgRight: TImage;
        FimgBack: TImage;
        FimgTopLeft: TImage;
        FlbAuthor: TsWebLabel;
        FimgAuthor: TImage;
        FimgClip: TImage;
        FimgBullet: TImage;
        FimgBottom: TImage;
        FimgTopRight: TImage;
        FimgLeft: TImage;
        FlbCaption: TsWebLabel;
        FpnlButtons: TsPanel;
        FbtEdit: TsSpeedButton;
        FbtDelete: TsSpeedButton;
        FbtQuote: TsSpeedButton;
        procedure FrameResize (Sender: TObject);
        procedure FrameMouseActivate (Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y, HitTest: Integer;
                                      var MouseActivate: TMouseActivate);
        procedure FpnlMsgPaint (Sender: TObject; Canvas: TCanvas);
        procedure FbtEditClick (Sender: TObject);
        procedure FbtDeleteClick (Sender: TObject);
        procedure FbtQuoteClick (Sender: TObject);
    public
        FedMsg: TMessageText;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Index: WORD;
        f_Data: Pointer;
        f_Created: Boolean;
        f_Loaded: Boolean;
        f_Adapter: TsFrameAdapter;
        f_pnlMsg: TsPanel;
        f_lbCaption: TsWebLabel;
        f_edMsg: TMessageText;
        f_lbAuthor: TsWebLabel;
        f_imgAuthor: TImage;
        f_imgClip: TImage;
        f_imgBullet: TImage;
        f_pnlButtons: TsPanel;
        f_btEdit: TsSpeedButton;
        f_btDelete: TsSpeedButton;
        f_btQuote: TsSpeedButton;
    public
        constructor Create (anOwner: TWinControl;
                            const anIndex: WORD); overload; virtual;
        destructor Destroy; override;
    public
        procedure Load; virtual;
        //procedure AlphaShow (const wTime: WORD);
    public
        property Index: WORD read f_Index;
        property Data: Pointer read f_Data write f_Data;
        property Created: Boolean read f_Created write f_Created;
        property Loaded: Boolean read f_Loaded write f_Loaded;
        property edMsg: TMessageText read f_edMsg;
    end;

resourcestring
    ERR_TTMPMESSAGE_CREATE      = 'Ошибка создания шаблона сообщения!';
    ERR_TTMPMESSAGE_DESTROY     = 'Ошибка уничтожения шаблона сообщения!';
    ERR_TTMPMESSAGE_LOAD        = 'Ошибка загрузки шаблона сообщения!';
    ERR_TTMPMESSAGE_ON_RESIZE   = 'Ошибка изменения размеров шаблона сообщения!!';
    ERR_TTMPMESSAGE_ON_ACTIVATE = 'Ошибка активации шаблона сообщения!';

implementation

{$R *.dfm}

uses
{ utils }
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses,
{ engine }
    Engine,
{ SQLite }
    SQLite3, SQLite3DLL, SQLiteTable3,
{ main window }
    uMain,
{ smiles }
    uSmileDialog;

constructor TMessageText.Create (anOwner: TWinControl; const aName: String);
begin
    inherited Create (anOwner);
    Parent := anOwner;
    OwnParent := Parent;
    while not (  ( OwnParent is TForm ) or ( OwnParent is TFrame )  ) do
    begin
        if Assigned (OwnParent.Parent) then
            OwnParent := OwnParent.Parent;
    end;
    Name := aName;
    ParentColor := FALSE;
    ParentFont := FALSE;
    Color := clWhite;
    Font.Name := 'Calibri';
    Font.CharSet := RUSSIAN_CHARSET;
    Font.Style := [fsBold];
    Font.Size := 12;
    BorderStyle := bsNone;
    ScrollBars := TScrollStyle (ssNone);
    WordWrap := TRUE;
    ReadOnly := TRUE;
end;

procedure TMessageText.Change;
var
    LineHeight : Integer;
    DC         : HDC;
    SaveFont   : HFont;
    Metrics    : TTextMetric;
begin
    inherited Change;
    DC := GetDC (Handle);
    SaveFont := SelectObject (DC,Font.Handle);
    GetTextMetrics (DC, Metrics);
    SelectObject (DC, SaveFont);
    ReleaseDC (Handle,DC);
    LineHeight := Metrics.tmHeight;
    OwnParent.Height := ( Lines.Count + 1 ) * LineHeight +
                        (OwnParent.Height - Height);
end;

class procedure TTmpMessage._raise (anArgs: array of const;
                                    const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TTmpMessage._raise (anArgs: array of const;
                                    anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TTmpMessage.Create (anOwner: TWinControl;
                                const anIndex: WORD);
var
    I : Integer;
begin
    try
        Created := FALSE;
        inherited Create (anOwner);
        Color := $00211A18;
        Parent := anOwner;
        Align := alTop;
        PopupMenu := MainForm.mnMessagesPopup;
        OnMouseWheel := MainForm.OnMouseWheel;
        f_Data := CreateMessageNode;
        f_Loaded := FALSE;
        { создаем текстовое поле }
        FedMsg := TMessageText.Create (FpnlMsg,'edMsg');
        FedMsg.Align := alClient;
        FedMsg.AlignWithMargins := TRUE;
        FedMsg.Margins.Top := 42;
        FedMsg.Margins.Left := 86;
        FedMsg.Margins.Right := 8;
        FedMsg.Margins.Bottom := 0;
        FedMsg.OnMouseWheel := MainForm.OnMouseWheel;
        { кнопки }
        FbtEdit.OnMouseMove := MainForm.OnButtonMove;
        FbtDelete.OnMouseMove := MainForm.OnButtonMove;
        FbtQuote.OnMouseMove := MainForm.OnButtonMove;
        { сохраняем укзатели }
        f_Adapter := FrameAdapter;
        f_pnlMsg := FpnlMsg;
        f_lbCaption := FlbCaption;
        f_edMsg := FedMsg;
        f_lbAuthor := FlbAuthor;
        f_imgAuthor := FimgAuthor;
        f_imgClip := FimgClip;
        f_imgBullet := FimgBullet;
        f_pnlButtons := FpnlButtons;
        f_btEdit := FbtEdit;
        f_btDelete := FbtDelete;
        f_btQuote := FbtQuote;
        { перименовываем компоненты в соответствии с индексом шаблона }
        f_Index := anIndex;
        for I := 0 to ComponentCount - 1 do
            Components [I].Name := Format ('%s_%d',[ Components [I].Name, anIndex ]);
        Name := Format ('%s_%d',[Name,anIndex]);
        Created := TRUE;
    except on E: Exception do
        _raise (['Create',ERR_TTMPMESSAGE_CREATE,E],
                ['{C1CBB232-2D3D-40D4-AC5D-CFF2D3360BEC}']);
    end;
end;

destructor TTmpMessage.Destroy;
begin
    try
        FreeMessageNode (f_Data);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TTMPMESSAGE_DESTROY,E],
                ['{D7969E9B-95C3-4DA9-87F4-8292B8EF921D}']);
    end;
end;

procedure TTmpMessage.FrameResize (Sender: TObject);
begin
    try
        if Assigned (Data) and Created then
        begin
            f_edMsg.Change;
        end;
    except on E: Exception do
        _raise (['FrameResize',ERR_TTMPMESSAGE_ON_RESIZE,E],
                ['{5F8FDF73-6840-4918-B71B-98C6F7609B81}']);
    end;
end;

procedure TTmpMessage.FbtEditClick (Sender: TObject);
begin
    if MainForm.actEditMessage.Enabled then
        MainForm.actEditMessage.Execute;
end;

procedure TTmpMessage.FbtDeleteClick (Sender: TObject);
begin
    if MainForm.actDeleteMessage.Enabled then
        MainForm.actDeleteMessage.Execute;
end;

procedure TTmpMessage.FbtQuoteClick (Sender: TObject);
begin
    MainForm.actQuoteMessage.Update;
    if MainForm.actQuoteMessage.Enabled then
        MainForm.actQuoteMessage.Execute;
end;

procedure TTmpMessage.Load;
var
    Author : TUser;
    BBCode : String;
begin
    try
        if Assigned (Data) and Created and not Loaded then
        begin
            { заголовок }
            f_lbCaption.Caption := Format ('%s : %s',
                                           [ PMessageNode (Data)^.Subject,
                                             _DateTimeToStr (PMessageNode (Data)^.TimeStampModify) ]);
            { текст }
            f_edMsg.Clear;
            BBCode := StrPas ( PMessageNode (Data)^.Text );
            try
                MarkQuotes (BBCode,clQuote);
                InsertBBCode (f_edMsg,BBCode);
                InsertQuotes (f_edMsg,MainForm.imgQuotes,clQuote);
                InsertSmiles (f_edMsg,SMILES,MainForm.GetImgSmiles);
                InsertSmiles (f_edMsg,SMILES_ADVANCED,MainForm.imgSmilesAdvanced);
            finally
                _FillChar ( BBCode, Length (BBCode), $00 );
            end;
            //f_edMsg.DefAttributes.Color := clBlack;
            //f_edMsg.DefAttributes.Name := 'Arial';
            f_edMsg.DefAttributes.CharSet := RUSSIAN_CHARSET;
            //f_edMsg.DefAttributes.Style := [fsBold];
            //f_edMsg.DefAttributes.Size := 10;
            //f_edMsg.DefAttributes.Protected := TRUE;
            f_edMsg.PopupMenu := MainForm.mnTextPopup;
            f_edMsg.OnMouseDown := MainForm.OnRichEditMouseDown;
            { автор }
            f_lbAuthor.Caption := PMessageNode (Data)^.Author;
            f_imgAuthor.Picture.Bitmap.FreeImage;
            if ( PMessageNode (Data)^.IDAuthor > 0 ) then
            begin
                Author := TUser.Load ( MainForm.DB, PMessageNode (Data)^.IDAuthor ) as TUser;
                try
                    if Assigned (Author) and
                       Assigned (Author.Pic) and
                       Assigned (Author.Pic.Picture) then
                        f_imgAuthor.Picture.Assign (Author.Pic.Picture);
                finally
                    FreeAndNil (Author);
                end;
            end;
            { кнопки }
            f_btEdit.Visible   := ( PMessageNode (Data)^.IDAuthor = USER_ID );
            f_btDelete.Visible := ( PMessageNode (Data)^.IDAuthor = USER_ID );
            { иконки сообщения }
            f_imgClip.Visible   := ( Index mod 2 <> 0 );
            f_imgBullet.Visible := ( Index mod 2 = 0 );
            Loaded := TRUE;
        end;
    except on E: Exception do
        _raise (['Load',ERR_TTMPMESSAGE_LOAD,E],
                ['{A0EEDDDF-501D-432A-B624-AA5D4250B9B1}']);
    end;
end;

procedure TTmpMessage.FpnlMsgPaint (Sender: TObject; Canvas: TCanvas);
begin
    Load;
end;

procedure TTmpMessage.FrameMouseActivate (Sender: TObject; Button: TMouseButton;
                                          Shift: TShiftState; X, Y, HitTest: Integer;
                                          var MouseActivate: TMouseActivate);
begin
    try
        if Assigned (Data) then
        begin
            MainForm.CurrMsgID := PMessageNode (Data)^.ID;
            MainForm.CurrMsgKeyHash := PMessageNode (Data)^.KeyHash;
            MainForm.CurrMsgTmp := Self;
            MainForm.edURL.Text := Format ('crypto://categorie/%s#%s/',
                                           [ MainForm.CurrCtgKeyHash,
                                             MainForm.CurrMsgKeyHash]);
        end;
    except on E: Exception do
        _raise (['FrameMouseActivate',ERR_TTMPMESSAGE_ON_ACTIVATE,E],
                ['{9BCD887F-F501-4004-B327-2BA07269909B}']);
    end
end;



end.
