unit uMetaDialog;
{******************************************************************************}
{*  Meta Dialog Unit                                                          *}
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
    sPanel, sButton, sBitBtn, sLabel, sEdit, sRichEdit, sMemo, Mask, sMaskEdit,
    sSpinEdit, sTooledit, sCustomComboEdit, sCurrEdit, sCurrencyEdit, sCheckBox,
    Kernel, ProtoClasses,
    {$IF not Defined (MINI_CRYPTO) }
    CryptoClasses,
    {$IFEND}
    DialogClasses,
    uProtoDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

type
{$M+}
    TDialogControl = class;
    TDialogControls = class;
    TMetaContent = class;
    TMetaDialog = class;
{$M-}

{$I 'TDialogControl.int.inc'}
{$I 'TDialogControls.int.inc'}

{ мета-диалоговое окно }
{$M+}
    CMetaDialog = class of TMetaDialog;
    PMetaDialog = ^TMetaDialog;
    TMetaDialog = class (TProtoDialog)
    protected
        function GetValueAt (anIndex: Integer) : Variant; virtual;
        function GetValueOf (aName: String) : Variant; virtual;
        procedure SetValueAt (anIndex: Integer; aValue: Variant); virtual;
        procedure SetValueOf (aName: String; aValue: Variant); virtual;
    public
        procedure GetData (anArgs: array of const;
                           aButtons: array of const;
                           aContent: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    public
        constructor Create (anArgs: array of const;
                            aButtons: array of const;
                            aContent: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const;
                             aButtons: array of const;
                             aContent: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const;
                                aButtons: array of const;
                                aContent: array of const) : Boolean; overload; virtual;
    public
        property ValueAt [anIndex: Integer]: Variant read GetValueAt write SetValueAt;
        property ValueOf [aName: String]: Variant read GetValueOf write SetValueOf;
    end;
{$M-}

{ содержимое }
{$M+}
    CMetaContent = class of TMetaContent;
    PMetaContent = ^TMetaContent;
    TMetaContent = class (TFrame)
        pnlBackGround: TsPanel;
        pnlLabels: TsPanel;
        pnlControls: TsPanel;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Fields: TDialogFields;
        f_Controls: TDialogControls;
    protected
        procedure OnFrameResize (Sender: TObject); virtual;
        procedure OnFrameKeyPress (Sender: TObject;
                                   var Key: Char); virtual;
    protected
        function GetValueAt (anIndex: Integer) : Variant; virtual;
        function GetValueOf (aName: String) : Variant; virtual;
        procedure SetValueAt (anIndex: Integer; aValue: Variant); virtual;
        procedure SetValueOf (aName: String; aValue: Variant); virtual;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anOwner: TProtoDialog;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
    public
        property Fields: TDialogFields read f_Fields write f_Fields;
        property Items: TDialogControls read f_Controls write f_Controls;
        property Values [anIndex: Integer]: Variant read GetValueAt write SetValueAt;
        property ValueOf [aName: String]: Variant read GetValueOf write SetValueOf;
    end;
{$M-}

{$I 'TDialogControl.err.inc'}
{$I 'TDialogControls.err.inc'}

resourcestring
    ERR_TMETACONTENT_CREATE    = 'Ошибка создания!';
    ERR_TMETACONTENT_DESTROY   = 'Ошибка уничтожения!';
    ERR_TMETACONTENT_GET_VALUE = 'Ошибка чтения поля!';
    ERR_TMETACONTENT_SET_VALUE = 'Ошибка записи поля!';
    ERR_TMETACONTENT_GET_DATA  = 'Ошибка чтения данных!';
    ERR_TMETACONTENT_SET_DATA  = 'Ошибка записи данных!';
    ERR_TMETACONTENT_ON_RESIZE = 'Ошибка изменения размера!';

resourcestring
    ERR_TMETADIALOG_GET_VALUE  = 'Ошибка чтения поля!';
    ERR_TMETADIALOG_SET_VALUE  = 'Ошибка записи поля!';

implementation

{$R *.dfm}

{$I 'TDialogControl.imp.inc'}
{$I 'TDialogControls.imp.inc'}

class procedure TMetaContent._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TMetaContent._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TMetaContent.Create (anOwner: TProtoDialog;
                                 anArgs: array of const);
begin
    try
        inherited Create (anOwner);
        OnResize := OnFrameResize;
        OnKeyPress := OnFrameKeyPress;
        Height := 8;
        { параметры - поля }
        f_Fields := TDialogFields.Create (anArgs);
        { создаем элементы управления }
        pnlControls.Align := alNone;
        f_Controls := TDialogControls.Create (pnlControls,
                                              pnlLabels,
                                              Fields,
                                              anOwner.mnTextPopup);
        Height := pnlControls.Height;
        pnlControls.Align := alClient;
    except on E: Exception do
        _raise (['Create',ERR_TMETACONTENT_CREATE,E],
                ['{5C67198E-24A6-4FDE-BBB7-2E70F7D39ECC}']);
    end;
end;

destructor TMetaContent.Destroy;
begin
    try
        f_Controls.FreeAll;
        FreeAndNil (f_Controls);
        f_Fields.FreeAll;
        FreeAndNil (f_Fields);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TMETACONTENT_DESTROY,E],
                ['{FD2DABFB-D690-4C12-815F-8B2AFE2CB93C}']);
    end;
end;

procedure TMetaContent.OnFrameResize (Sender: TObject);
var
    I : Integer;
begin
    try
        if Assigned (f_Controls) then
            for I := 0 to f_Controls.Count - 1 do
                f_Controls.ItemAt [I].Control.Width := pnlControls.Width - 16;
    except on E: Exception do
        _raise (['OnFrameResize',ERR_TMETACONTENT_ON_RESIZE,E],
                ['{90496CEA-7C09-45CB-8699-95691A16E0A6}']);
    end;
end;

procedure TMetaContent.OnFrameKeyPress (Sender: TObject;
                                        var Key: Char);
begin
    if Assigned (Owner) and ( Owner is TProtoDialog ) then
        TProtoDialog (Owner).OnKeyPress (Sender,Key);
end;

procedure TMetaContent.GetData (anArgs: array of const);
var
    I : Integer;
begin
    try
        for I := 0 to Fields.Count - 1 do
            SetValueAt ( I, Fields.ItemAt [I].Value );

        for I := 0 to Items.Count - 1 do
        begin
            if ( Items.ItemAt [I].Control is TCustomEdit ) then
                TEdit (Items.ItemAt [I].Control).OnKeyPress := OnKeyPress
            else if ( Items.ItemAt [I].Control is TCustomComboBox ) then
                TComboBox (Items.ItemAt [I].Control).OnKeyPress := OnKeyPress;
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TMETACONTENT_GET_DATA,E],
                ['{8600F7CC-1596-4C18-BD47-7F773825F1F2}']);
    end;
end;

procedure TMetaContent.SetData (anIndex: Integer);
var
    I : Integer;
begin
    try
        for I := 0 to Fields.Count - 1 do
            Fields.ItemAt [I].Value := GetValueAt (I);
    except on E: Exception do
        _raise (['SetData',ERR_TMETACONTENT_SET_DATA,E],
                ['{8EDB286D-E981-445C-9AC4-1594EE40EFA4}']);
    end;
end;

function TMetaContent.GetValueAt (anIndex: Integer) : Variant;
begin
    Result := NULL;
    try
        Result := f_Controls.ValueAt [anIndex];
    except on E: Exception do
        _raise ([ Format ('GetValueAt (''%d'')',[anIndex]), ERR_TMETACONTENT_GET_VALUE,E ],
                ['{4BF57198-38D5-4298-915F-0F4FA65CF9AB}']);
    end;
end;

function TMetaContent.GetValueOf (aName: String) : Variant;
begin
    Result := NULL;
    try
        Result := f_Controls.ItemOf [aName].Value;
    except on E: Exception do
        _raise ([ Format ('GetValueOf (''%s'')',[aName]), ERR_TMETACONTENT_GET_VALUE,E ],
                ['{8787F290-2272-4B09-8695-B8BD81A62D9E}']);
    end;
end;

procedure TMetaContent.SetValueAt (anIndex: Integer; aValue: Variant);
begin
    try
        f_Controls.ValueAt [anIndex] := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueAt (''%d'')',[anIndex]), ERR_TMETACONTENT_SET_VALUE,E ],
                ['{9053325D-F4C2-40BC-B556-372D5041A9F2}']);
    end;
end;

procedure TMetaContent.SetValueOf (aName: String; aValue: Variant);
begin
    try
        f_Controls.ItemOf [aName].Value := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueOf (''%s'')',[aName]), ERR_TMETACONTENT_SET_VALUE,E ],
                ['{EFFFCBBF-B6FA-46E4-8D94-7A70F0A119C0}']);
    end;
end;

constructor TMetaDialog.Create (anArgs: array of const;
                                aButtons: array of const;
                                aContent: array of const);
begin
    try
        inherited Create (anArgs,aButtons,NIL);
        Content := TMetaContent.Create (self,aContent);
        minHeight := 60 + TMetaContent (Content).Height + 8 + pnlButtons.Height;
        Height := minHeight;
        with TMetaContent (Content) do
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
                ['{2A34EFF8-E7E3-458C-A2A5-F4A4032BE0A4}']);
    end;
end;

destructor TMetaDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{9D451E39-81AA-4D35-8F03-C4BA0E1225B8}']);
    end;
end;

function TMetaDialog.GetValueAt (anIndex: Integer) : Variant;
begin
    Result := NULL;
    try
        with TMetaContent (Content) do
            Result := Values [anIndex];
    except on E: Exception do
        _raise ([ Format ('GetValueAt (''%d'')',[anIndex]), ERR_TMETADIALOG_GET_VALUE,E ],
                ['{FD43C0BF-6516-4A56-A1D2-038DC8818FA9}']);
    end;
end;

function TMetaDialog.GetValueOf (aName: String) : Variant;
begin
    Result := NULL;
    try
        with TMetaContent (Content) do
            Result := ValueOf [aName];
    except on E: Exception do
        _raise ([ Format ('GetValueOf (''%s'')',[aName]), ERR_TMETADIALOG_GET_VALUE,E ],
                ['{ED2271B3-7AE5-4F7F-8F4E-4FAC61197FAB}']);
    end;
end;

procedure TMetaDialog.SetValueAt (anIndex: Integer; aValue: Variant);
begin
    try
        with TMetaContent (Content) do
            Values [anIndex] := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueAt (''%d'')',[anIndex]), ERR_TMETADIALOG_SET_VALUE,E ],
                ['{2048A890-BAAC-4870-A53B-68744EFF10D4}']);
    end;
end;

procedure TMetaDialog.SetValueOf (aName: String; aValue: Variant);
begin
    try
        with TMetaContent (Content) do
            ValueOf [aName] := aValue;
    except on E: Exception do
        _raise ([ Format ('SetValueOf (''%s'')',[aName]), ERR_TMETADIALOG_SET_VALUE,E ],
                ['{A3F959A6-E9BB-43F1-93DB-7E066BE50DA9}']);
    end;
end;

procedure TMetaDialog.GetData (anArgs: array of const;
                               aButtons: array of const;
                               aContent: array of const);
begin
    try
        inherited GetData (anArgs,aButtons);
        with TMetaContent (Content) do
            GetData (aContent);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{64A3CD8B-6DBE-4CB7-A714-3B2EC21F1DA5}']);
    end;
end;

procedure TMetaDialog.SetData (anIndex: Integer);
begin
    try
        with TMetaContent (Content) do
            SetData (anIndex);
        inherited SetData (anIndex);
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{158EC6C5-B6B5-4C50-8FB3-ED663C9AA401}']);
    end;
end;

class function TMetaDialog.Open (anArgs: array of const;
                                 aButtons: array of const;
                                 aContent: array of const) : Integer;
begin
    try
        with Create (anArgs,aButtons,aContent) do
        try
            GetData (anArgs,aButtons,aContent);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{CC339815-4470-49BC-A72D-C0A4199678A8}']);
    end;
end;

class function TMetaDialog.Execute (anArgs: array of const;
                                    aButtons: array of const;
                                    aContent: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs,aButtons,aContent) <> mrNone );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{A8577C62-2FA0-45D0-87FC-F707FE2F996E}']);
    end;
end;


end.
