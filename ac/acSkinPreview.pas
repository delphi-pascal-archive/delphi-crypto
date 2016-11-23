unit acSkinPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, sBitBtn, ComCtrls, sStatusBar, sEdit,
  ExtCtrls, sPanel, sSkinProvider, sSkinManager, sCheckBox, sGroupBox;

type
  TFormSkinPreview = class(TForm)
    sEdit1: TsEdit;
    sStatusBar1: TsStatusBar;
    sPanel1: TsPanel;
    sSkinProvider1: TsSkinProvider;
    sGroupBox1: TsGroupBox;
    sCheckBox1: TsCheckBox;
    sCheckBox2: TsCheckBox;
    sPanel2: TsPanel;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    PreviewManager: TsSkinManager;
  end;

var
  FormSkinPreview: TFormSkinPreview;

implementation

{$R *.dfm}

end.
