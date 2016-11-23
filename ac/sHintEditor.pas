unit sHintEditor;

{$I sDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, sConst, ExtCtrls, sGraphUtils, acntUtils, ImgList,
  Consts, ComStrs, CommCtrl, menus, sHintDesigner,
  {$IFDEF DELPHI5}dsgnintf, {$ELSE}DesignEditors, DesignIntf, VCLEditors, {$ENDIF}sHintManager;

type
  TsHintEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TsHintsDesigner = class(TComponentProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  { Open window with editor of hint design }
procedure RunHintsDesigner(sHintManager : TsHintManager);
procedure Register;

implementation

{ TsHintEditor }

procedure RunHintsDesigner(sHintManager : TsHintManager);
begin
  CreateEditorForm;
  HintDesigner.sHintManager1 := sHintManager;
  if csDesigning in sHintManager.ComponentState then Manager := sHintManager;
  OpenDesigner;
  KillForm;
end;

procedure TsHintEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: RunHintsDesigner(TsHintManager(Component));
  end;
  if Designer <> nil then Designer.Modified;
end;

function TsHintEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  Result := '&Hints Designer...';
    1:  Result := '-';
  end;
end;

function TsHintEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  RegisterComponentEditor(TsHintManager, TsHintEditor);
  RegisterPropertyEditor(TypeInfo(TsHintKind), TsHintManager, 'HintKind', TsHintsDesigner);
end;

{ TsHintsDesigner }

procedure TsHintsDesigner.Edit;
begin
  inherited;
  RunHintsDesigner(TsHintManager(GetComponent(0)));
  if Designer <> nil then Designer.Modified;
end;

function TsHintsDesigner.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;

function TsHintsDesigner.GetValue: string;
begin
  Result := 'Show designer...';
end;

end.
