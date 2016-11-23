unit acRootEdit;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, acShellCtrls, {$IFDEF DELPHI6UP}DesignEditors, DesignIntf, VCLEditors,{$ELSE}dsgnintf,{$ENDIF}
  sSkinProvider, sDialogs, sEdit,
  sRadioButton, sComboBox, sGroupBox, sButton, Buttons, sBitBtn, Mask,
  sMaskEdit, sCustomComboEdit, sTooledit;

type
  TacRootPathEditDlg = class(TForm)
    Button1: TsBitBtn;
    Button2: TsBitBtn;
    rbUseFolder: TsRadioButton;
    GroupBox1: TsGroupBox;
    cbFolderType: TsComboBox;
    GroupBox2: TsGroupBox;
    OpenDialog1: TsOpenDialog;
    sSkinProvider1: TsSkinProvider;
    rbUsePath: TsRadioButton;
    ePath: TsDirectoryEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure rbUseFolderClick(Sender: TObject);
    procedure rbUsePathClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure UpdateState;
  public
  end;

  TacRootProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TacRootEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{$R *.dfm}

uses TypInfo, FileCtrl, sStyleSimply;

resourcestring
//  SPickRootPath = 'Please select a root path';
  SEdiTacRoot = 'E&dit Root';

const
  NTFolders = [rfCommonDesktopDirectory, rfCommonPrograms, rfCommonStartMenu,
               rfCommonStartup];

function PathIsCSIDL(Value: string): Boolean;
begin
  Result := GetEnumValue(TypeInfo(TacRootFolder), Value) >= 0;
end;

function RootPathEditor(const Value : string): string;
var
  rpe : TacRootPathEditDlg;
begin
  Result := Value;
  rpe := TacRootPathEditDlg.Create(Application);
  with rpe do try
    rbUseFolder.Checked := PathIsCSIDL(Result);
    rbUsePath.Checked := not rbUseFolder.Checked;
    if not PathIsCSIDL(Result) then begin
      cbFolderType.ItemIndex := 0;
      ePath.Text := Result;
    end
    else cbFolderType.ItemIndex := cbFolderType.Items.IndexOf(Result);

    UpdateState;
    ShowModal;
    if ModalResult = mrOK then begin
      if rbUsePath.Checked then Result := ePath.Text else Result := cbFolderType.Items[cbFolderType.ItemIndex];
    end;
  finally
    Free;
  end;
end;

procedure TacRootProperty.Edit;
begin
  SetStrValue(RootPathEditor(GetStrValue));
end;

function TacRootProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TacRootPathEditDlg.FormCreate(Sender: TObject);
var
  FT: TacRootFolder;
begin
  for FT := Low(TacRootFolder) to High(TacRootFolder) do
    if not ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (FT in NTFolders)) then
      cbFolderType.Items.Add(GetEnumName(TypeInfo(TacRootFolder), Ord(FT)));
  cbFolderType.ItemIndex := 0;
end;

procedure TacRootPathEditDlg.UpdateState;
begin
  cbFolderType.Enabled := rbUseFolder.Checked;
  ePath.Enabled := not rbUseFolder.Checked;
//  btnBrowse.Enabled := ePath.Enabled;
end;

procedure TacRootPathEditDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TacRootPathEditDlg.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TacRootPathEditDlg.rbUseFolderClick(Sender: TObject);
begin
  rbUsePath.Checked := not rbUseFolder.Checked;
  UpdateState;
end;

procedure TacRootPathEditDlg.rbUsePathClick(Sender: TObject);
begin
  rbUseFolder.Checked := not rbUsePath.Checked;
  UpdateState;
end;

procedure TacRootPathEditDlg.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TacRootPathEditDlg.btnBrowseClick(Sender: TObject);
begin
end;

{ TacRootEditor }

procedure TacRootEditor.ExecuteVerb(Index: Integer);
  procedure EdiTacRoot;
  const
    SRoot = 'root';
  var
    Path : string;
  begin
    Path := RootPathEditor(GetPropValue(Component, SRoot, True));
    SetPropValue(Component, SRoot, Path);
    Designer.Modified;
  end;
begin
  case Index of
    0 : EdiTacRoot;
  end;
end;

function TacRootEditor.GetVerb(Index: Integer): String;
begin
  case Index of
  0 : Result := SEdiTacRoot;
  end;
end;

function TacRootEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
