unit sFileCtrl;
{$I sDefs.inc}

{$R-,T-,H+,X+}

interface

uses Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  Menus, StdCtrls, Buttons, sComboBox, sComboBoxes, ShlObj, ActiveX;

type
  TsFilterComboBox = class(TsCustomComboBox)
{$IFNDEF NOTFORHELP}
  private
    FFilter: string;
    function IsFilterStored: Boolean;
    function GetMask: string;
    procedure SetFilter(const NewFilter: string);
  protected
    procedure Change; override;
    procedure CreateWnd; override;
    procedure Click; override;
    procedure BuildList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//    function FullPaint : boolean; override;
  public
    MaskList: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Mask: string read GetMask;
    property Text;
  published
    property Anchors;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeName;
    property ImeMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property Filter: string read FFilter write SetFilter stored IsFilterStored;
  end;
{
  TsDriveComboBoxEx = class(TsCustomComboBoxEx)
  private
    function GetPath: string;
    procedure SetPath(const Value: string);
  public
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure BuildList;
    procedure ShowFolder(folder: IShellFolder);
  published
    property Path : string read GetPath write SetPath;
  end;

var
  Level: Integer;
  pMalloc: IMalloc;
  hr: HRESULT;
}
implementation

uses Consts, Dialogs, acntUtils, sVCLUtils, sMessages;

{ TsFilterComboBox }
{
function GetShellObjectName(pidl: PItemIDList; const Value: STRRET): string;
begin
  with Value do case uType of
    STRRET_CSTR: Result := PChar(@cStr[0]);
    STRRET_WSTR: begin
      Result := pOleStr;
      pMalloc.Free( pOleStr );
    end;
    STRRET_OFFSET: Result := PChar( LongWord(pidl) + uOffset );
  end;
end;
}
procedure TsFilterComboBox.BuildList;
var
  AFilter, MaskName, Mask: string;
  BarPos: Integer;
begin
  Clear;
  MaskList.Clear;
  AFilter := Filter;
  BarPos := AnsiPos('|', AFilter);
  while BarPos <> 0 do begin
    MaskName := Copy(AFilter, 1, BarPos - 1);
    Delete(AFilter, 1, BarPos);
    BarPos := AnsiPos('|', AFilter);
    if BarPos > 0 then begin
      Mask := Copy(AFilter, 1, BarPos - 1);
      Delete(AFilter, 1, BarPos);
    end
    else begin
      Mask := AFilter;
      AFilter := '';
    end;
    Items.Add(MaskName);
    MaskList.Add(Mask);
    BarPos := AnsiPos('|', AFilter);
  end;
  ItemIndex := 0;
end;

procedure TsFilterComboBox.Change;
begin
  inherited Change;
end;

procedure TsFilterComboBox.Click;
begin
  inherited Click;
  Change;
end;

constructor TsFilterComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  FFilter := SDefaultFilter;
  MaskList := TStringList.Create;
end;

procedure TsFilterComboBox.CreateWnd;
begin
  inherited CreateWnd;
  BuildList;
end;

destructor TsFilterComboBox.Destroy;
begin
  MaskList.Free;
  inherited Destroy;
end;

function TsFilterComboBox.GetMask: string;
begin
  if ItemIndex < 0 then ItemIndex := Items.Count - 1;
  if ItemIndex >= 0 then begin
    Result := MaskList[ItemIndex];
  end
  else begin
    Result := '*.*';
  end;
end;

function TsFilterComboBox.IsFilterStored: Boolean;
begin
  Result := SDefaultFilter <> FFilter;
end;

procedure TsFilterComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TsFilterComboBox.SetFilter(const NewFilter: string);
begin
  if AnsiCompareFileName(NewFilter, FFilter) <> 0 then begin
    FFilter := NewFilter;
    if HandleAllocated then BuildList;
    if not (csLoading in ComponentState) then Change;
  end;
end;

(*
{ TsDriveComboBoxEx }

procedure TsDriveComboBoxEx.BuildList;
var
  desktop: IShellFolder;
  mkid: SHITEMID;
  pidlItself: PItemIDList;
  Value: STRRET;
begin
//  Items.Clear;
  Level := 0;
  mkid.cb := 0;
  pidlItself := @mkid;
   // This pidl now points to an empty Identifier List.
   // It is points to owner folder itself.
   // NB: It works only for a root folder!

  hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);//COINIT_MULTITHREADED);
  if Succeeded(hr) then try
    hr := SHGetMalloc(pMalloc);
    if Succeeded(hr) then try
      hr := SHGetDesktopFolder(desktop);
      if Succeeded(hr) then try
        hr := desktop.GetDisplayNameOf(pidlItself, SHGDN_NORMAL or SHGDN_INCLUDE_NONFILESYS, Value);
        if Succeeded(hr) then Items.Add(GetShellObjectName(pidlItself, Value));
        ShowFolder(desktop);
      finally
        desktop := nil;
      end;
    finally
      pMalloc := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TsDriveComboBoxEx.CreateWnd;
begin
  inherited;
end;

function TsDriveComboBoxEx.GetPath: string;
begin
  if ItemIndex < 0 then Result := ''
  else begin
    Result := ItemsEx[ItemIndex].Caption
  end;
end;

procedure TsDriveComboBoxEx.Loaded;
begin
  inherited;
  BuildList;
end;

procedure TsDriveComboBoxEx.SetPath(const Value: string);
begin

end;

procedure TsDriveComboBoxEx.ShowFolder(folder: IShellFolder);
var
  pidlChild: PItemIDList;
  Value: STRRET;
  Iterator: IEnumIDList;
  celtFetched: ULONG;
  child: IShellFolder;
begin
  Inc(Level); if Level < 3 then begin
    hr := Folder.EnumObjects(0, SHCONTF_FOLDERS, Iterator);
    if Succeeded(hr) then try
      while True do begin
        hr := Iterator.Next(1, pidlChild, celtFetched);
        if hr <> NOERROR then Break;
        try
          hr := Folder.GetDisplayNameOf(pidlChild, SHGDN_INFOLDER{ or SHGDN_INCLUDE_NONFILESYS}, Value);
          if Succeeded(hr) then begin
//            hr := Folder.BindToObject(pidlChild, nil, IID_IShellFolder, Pointer(child));
//            if not Succeeded(hr) then Continue;
            hr := Folder.BindToObject(pidlChild, nil, IID_IShellFolder, Pointer(child));
            if Succeeded(hr) then try
              if pidlChild.
              with ItemsEx.Add do begin
                Caption := AddChar(' ', '', Level * 4) + GetShellObjectName(pidlChild, Value);
                Items.Add(Caption);
              end;
              ShowFolder(child);
            finally
              child := nil;
            end
            else Continue;
          end;
        finally
          pMalloc.Free(pidlChild);
        end;
      end;
    finally
      Iterator := nil;
    end;
  end;
  Dec(Level);
end;
*)
end.
