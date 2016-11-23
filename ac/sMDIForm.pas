unit sMDIForm;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sSkinProvider, sCommonData, Menus, acSBUtils;

type
  TsMDIForm = class(TPersistent)
  public
    FForm: TCustomForm;
    ListSW : TacScrollWnd;
    SkinProvider : TsSkinProvider;

    procedure ConnectToClient;
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function MakeChildIconItem : TMenuItem;
    procedure UpdateMDIIconItem;
    procedure RefreshMDIScrolls;
    procedure DestroyScrolls;

    procedure RestoreClick(Sender: TObject);
    procedure MinClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  end;

implementation

uses math, sStrings, acntUtils, sMessages {$IFDEF LOGGED}, sDebugMsgs{$ENDIF}, sSkinProps;

{ TsMDIForm }

procedure TsMDIForm.ConnectToClient;
begin
  if not (csDesigning in FForm.ComponentState) and (ListSW = nil) then RefreshMDIScrolls;
end;

constructor TsMDIForm.Create(AOwner: TPersistent);
begin
  inherited Create;
  SkinProvider := TsSkinProvider(AOwner);
  if SkinProvider.Form <> nil then begin
    FForm := SkinProvider.Form;
    FForm.HandleNeeded;
  end;
end;

destructor TsMDIForm.Destroy;
begin
  DestroyScrolls;
  inherited Destroy;
end;

function TsMDIForm.MakeChildIconItem: TMenuItem;
begin
  Result := nil;
end;

procedure TsMDIForm.CloseClick(Sender: TObject);
begin
  Sendmessage(TForm(SkinProvider.Form).ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

procedure TsMDIForm.MinClick(Sender: TObject);
begin
  Sendmessage(TForm(SkinProvider.Form).ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TsMDIForm.RestoreClick(Sender: TObject);
begin
  Sendmessage(TForm(SkinProvider.Form).ActiveMDIChild.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TsMDIForm.UpdateMDIIconItem;
begin
  SkinProvider.RepaintMenu
end;

procedure TsMDIForm.RefreshMDIScrolls;
begin
  if SkinProvider.SkinData.Skinned then begin
    DestroyScrolls;
    ListSW := TacMDIWnd.Create(TForm(FForm).ClientHandle, nil, SkinProvider.SkinData.SkinManager, s_CheckBox);
    TacMDIWnd(ListSW).FForm := TForm(FForm);
    TacMDIWnd(ListSW).MDISkinData := SkinProvider.SkinData;
    TacMDIWnd(ListSW).SkinProvider := SkinProvider;
  end
  else begin
    DestroyScrolls;
  end;
end;

procedure TsMDIForm.DestroyScrolls;
begin
  if ListSW <> nil then begin
    FreeAndNil(ListSW.SkinData);
    FreeAndNil(ListSW);
  end;
end;

end.

