unit sComboEdit;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, acntUtils, ComCtrls, ToolWin, ExtCtrls, sCustomComboEdit, sConst;

type

  TsComboEdit = class(TsCustomComboEdit)
{$IFNDEF NOTFORHELP}
  public
    constructor Create(AOwner:TComponent); override;
    procedure Loaded; override;
    procedure AfterConstruction; override;
  published
    property Alignment;
    property CharCase;
    property ClickKey;
    property DirectInput;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;

    property AutoSelect;
    property HelpContext;
    property PasswordChar;
    property Hint;

    property DragCursor;
    property OnEnter;
    property OnExit;
{$ENDIF} // NOTFORHELP
  end;

implementation

uses sGlyphUtils;

procedure TsComboEdit.AfterConstruction;
begin
  inherited;
end;

constructor TsComboEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboEdit;
  FDefBmpID := iBTN_ELLIPSIS;
end;

procedure TsComboEdit.Loaded;
begin
  inherited;
end;

end.
