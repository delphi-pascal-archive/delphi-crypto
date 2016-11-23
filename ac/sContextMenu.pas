unit sContextMenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, Graphics,
  sSkinProvider, sSkinManager, Menus, sSkinMenus;

type

  TsPopupList = class(TPopupList)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
  end;

  TsMenuItem = class(TMenuItem)
  public
    UID : Cardinal;
  end;

  TsContextMenu = class(TPopupMenu)
  public
    function FindItemByUid(UID : Cardinal) : TsMenuItem;
  end;

  procedure HookContextMenu;
  procedure ProcessContextMenu(var Message : TMessage);

var
 HookHandle: DWORD = 0;
 ContextMenu : TsContextMenu;

implementation

uses sStyleSimply, sMaskData, sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, acntUtils, sSKinProps;

function ContextMenuHookProc(Code: Integer; WParam: Integer; LParam: PCWPStruct): Integer; stdcall;
var
  Menu: HMENU;
  ItemCount, I: Integer;

  procedure SetOwnerDraw(Menu : HMENU; Item : integer);
  var
    MenuItemInfo: TMenuItemInfo;
    Buffer: array[0..79] of Char;

  begin
    MenuItemInfo.cbSize := 44; // Required for Windows 95
    MenuItemInfo.fMask := MIIM_TYPE;
    MenuItemInfo.dwTypeData := Buffer;
    MenuItemInfo.cch := SizeOf(Buffer);
    if GetMenuItemInfo(Menu, Item, True, MenuItemInfo) then begin
      MenuItemInfo.fType := MenuItemInfo.fType and not MFT_STRING;
      MenuItemInfo.fType := MenuItemInfo.fType or MFT_OWNERDRAW;
      SetMenuItemInfo(Menu, Item, True, MenuItemInfo);
    end;
  end;

  procedure AddContextMenu(Item : TMenuItem; Menu : HMENU; Index : integer);
  var
    MenuItemInfo: TMenuItemInfo;
    Buffer: array[0..79] of Char;
    MI : TsMenuItem;
    Buffer2: array[0..255] of Char;
    Caption : string;
    ItemCount, I: Integer;

  begin
    MenuItemInfo.cbSize := 44; // Required for Windows 95
    MenuItemInfo.fMask := MIIM_TYPE or MIIM_ID or MIIM_STATE or MIIM_SUBMENU;
    MenuItemInfo.dwTypeData := Buffer;
    MenuItemInfo.cch := SizeOf(Buffer);
    if GetMenuItemInfo(Menu, Index, True, MenuItemInfo) then begin
      MI := TsMenuItem.Create(ContextMenu);
      i := GetMenuString(Menu, Index, Buffer2, SizeOf(Buffer2), MF_BYPOSITION);
      SetString(Caption,Buffer2,i);
      if (MenuItemInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR then
        MI.Caption := cLineCaption
      else begin
        MI.Caption := Caption;

//      MI.Enabled := (MenuItemInfo.fState and MFS_ENABLED) = MFS_ENABLED;
      MI.Enabled := (MenuItemInfo.fState and (MFS_DISABLED or MFS_GRAYED)) = 0;
      MI.Default := (MenuItemInfo.fState and MFS_DEFAULT) = MFS_DEFAULT;
      MI.RadioItem := (MenuItemInfo.fState and MFT_RADIOCHECK) = MFT_RADIOCHECK;
      MI.Checked := (MenuItemInfo.fState and MFS_CHECKED) = MFS_CHECKED;

      end;
      MI.UID := MenuItemInfo.wID;
      Item.Add(MI);

      if MenuItemInfo.hSubMenu > 0 then begin
        ItemCount := GetMenuItemCount(MenuItemInfo.hSubMenu);
        for I := 0 to ItemCount - 1 do
          AddContextMenu(MI, MenuItemInfo.hSubMenu, I);
      end;
//      MenuItemInfo.fType := MenuItemInfo.fType and not MFT_STRING;
//      MenuItemInfo.fType := MenuItemInfo.fType or MFT_OWNERDRAW;
//      SetMenuItemInfo(Menu, Item, True, MenuItemInfo);
    end;
  end;

begin
  if (Code = HC_ACTION) then
    case LPARAM.message of
      WM_MENUSELECT:
      begin
        Menu := LParam^.lParam; // Вот он хэндл меню
        if ContextMenu = nil then ContextMenu := TsContextMenu.Create(Application);
        ContextMenu.Items.Clear;
        ItemCount := GetMenuItemCount(Menu);
        for I := 0 to ItemCount - 1 do
          AddContextMenu(ContextMenu.Items, Menu, I);

        DefaultManager.SkinableMenus.HookPopupMenu(ContextMenu,true);

        for I := 0 to ItemCount - 1 do
          SetOwnerDraw(Menu, I);
 //         EnableMenuItem(Menu, GetMenuItemID(Menu, I), MF_ENABLED);
        UnhookWindowsHookEx(HookHandle);
        HookHandle := 0;
      end;
      $01E2: // это типа заставляем реагировать на WM_MENUSELECT до появления окна...
        SendMessage(LPARAM.HWND, $1E5, 0, 0);
    end;
  ContextMenuHookProc := CallNextHookEx(HookHandle, Code, WPARAM, DWORD(LPARAM));
end;

procedure HookContextMenu;
begin
  if HookHandle <> 0 then UnhookWindowsHookEx(HookHandle);
  HookHandle := SetWindowsHookEx(WH_CALLWNDPROC, @ContextMenuHookProc, 0, GetCurrentThreadId);
end;

procedure ProcessContextMenu(var Message : TMessage);
begin
  TsPopupList(PopupList).WndProc(Message);
end;

{ TsPopupList }

procedure TsPopupList.WndProc(var Message: TMessage);
var
  I, Item: Integer;
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  ContextID: Integer;
  Canvas: TCanvas;
  SaveIndex: Integer;
  DC: HDC;
begin
  case Message.Msg of
    WM_CONTEXTMENU:
      begin
        inherited;
      end;
    WM_INITMENU:
      begin
        inherited;
      end;
    WM_DRAWITEM:
      with PDrawItemStruct(Message.LParam)^ do
      begin
        for I := 0 to Count - 1 do
        begin
          if TComponent(Items[I]) is TsContextMenu then
            MenuItem := TsContextMenu(Items[I]).FindItemByUid(itemID)
          else
            MenuItem := TPopupMenu(Items[I]).FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            Canvas := TControlCanvas.Create;
            with Canvas do
            try
              SaveIndex := SaveDC(hDC);
              try
                Handle := hDC;
                Font := Screen.MenuFont;
                DrawMenuItem(MenuItem, Canvas, rcItem, TOwnerDrawState(LongRec(itemState).Lo));
              finally
                Handle := 0;
                RestoreDC(hDC, SaveIndex);
              end;
            finally
              Canvas.Free;
            end;
            Exit;
          end;
        end;
      end;
    WM_MEASUREITEM:
      with PMeasureItemStruct(Message.LParam)^ do
      begin

        for I := 0 to Count - 1 do
        begin
          if TComponent(Items[I]) is TsContextMenu then
            MenuItem := TsContextMenu(Items[I]).FindItemByUid(itemID)
          else
            MenuItem := TPopupMenu(Items[I]).FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            DC := GetWindowDC(Window);
            try
              Canvas := TControlCanvas.Create;
              with Canvas do
              try
                SaveIndex := SaveDC(DC);
                try
                  Handle := DC;
                  Font := Screen.MenuFont;
                  TsMenuItem(MenuItem).MeasureItem(Canvas, Integer(itemWidth),
                    Integer(itemHeight));
                finally
                  Handle := 0;
                  RestoreDC(DC, SaveIndex);
                end;
              finally
                Canvas.Free;
              end;
            finally
              ReleaseDC(Window, DC);
            end;
            Exit;
          end;
        end;
      end;
  else
    inherited;
  end;

end;

{ TsContextMenu }

function TsContextMenu.FindItemByUid(UID: Cardinal): TsMenuItem;
var
  FoundItem: TsMenuItem;

  function Find(Item: TMenuItem): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if (Item is TsMenuItem) and (TsMenuItem(Item).UID = UID) then
    begin
      FoundItem := Item as TsMenuItem;
      Result := True;
      Exit;
    end
    else
      for I := 0 to Item.Count - 1 do
        if Find(Item[I]) then
        begin
          Result := True;
          Exit;
        end;
  end;

begin
  FoundItem := nil;
  Find(Items);
  Result := FoundItem;
end;

initialization
  PopupList.Free;
  PopupList := TsPopupList.Create;

finalization

end.
