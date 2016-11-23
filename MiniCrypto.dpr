program MiniCrypto;

{%File 'std.inc'}
{%File 'png\pngextra.pas'}
{%File 'png\pngimage.pas'}
{%File 'png\pnglang.pas'}
{%File 'ac\acPNG.pas'}
{%File 'crypto\Crypto.pas'}
{%File 'crypto\TKey.int.inc'}
{%File 'crypto\TKey.imp.inc'}
{%File 'crypto\TRandomType.int.inc'}
{%File 'crypto\TRandomType.err.inc'}
{%File 'crypto\TRandomType.imp.inc'}
{%File 'crypto\PRNG.int.inc'}
{%File 'crypto\PRNG.err.inc'}
{%File 'crypto\PRNG.imp.inc'}
{%File 'crypto\PRNG.project.inc'}
{%File 'crypto\PRNG.uses.inc'}
{%File 'crypto\TPKCryptoType.int.inc'}
{%File 'crypto\TPKCryptoType.err.inc'}
{%File 'crypto\TPKCryptoType.imp.inc'}
{%File 'crypto\RSA.int.inc'}
{%File 'crypto\RSA.err.inc'}
{%File 'crypto\RSA.imp.inc'}
{%File 'crypto\RSA.test.int.inc'}
{%File 'crypto\RSA.test.imp.inc'}
{%File 'crypto\RSA.project.inc'}
{%File 'crypto\RSA.uses.inc'}
{%File 'crypto\TCryptoType.int.inc'}
{%File 'crypto\TCryptoType.err.inc'}
{%File 'crypto\TCryptoType.imp.inc'}
{%File 'crypto\TCryptoMode.int.inc'}
{%File 'crypto\TCryptoMode.err.inc'}
{%File 'crypto\TCryptoMode.imp.inc'}
{%File 'crypto\AES.int.inc'}
{%File 'crypto\AES.err.inc'}
{%File 'crypto\AES.imp.inc'}
{%File 'crypto\AES.test.int.inc'}
{%File 'crypto\AES.test.imp.inc'}
{%File 'crypto\AES.project.inc'}
{%File 'crypto\AES.uses.inc'}
{%File 'crypto\Anubis.int.inc'}
{%File 'crypto\Anubis.err.inc'}
{%File 'crypto\Anubis.imp.inc'}
{%File 'crypto\Anubis.test.int.inc'}
{%File 'crypto\Anubis.test.imp.inc'}
{%File 'crypto\Anubis.project.inc'}
{%File 'crypto\Anubis.uses.inc'}
{%File 'crypto\Serpent.int.inc'}
{%File 'crypto\Serpent.err.inc'}
{%File 'crypto\Serpent.imp.inc'}
{%File 'crypto\Serpent.test.int.inc'}
{%File 'crypto\Serpent.test.imp.inc'}
{%File 'crypto\Serpent.project.inc'}
{%File 'crypto\Serpent.uses.inc'}
{%File 'crypto\Shacal.int.inc'}
{%File 'crypto\Shacal.err.inc'}
{%File 'crypto\Shacal.imp.inc'}
{%File 'crypto\Shacal.test.int.inc'}
{%File 'crypto\Shacal.test.imp.inc'}
{%File 'crypto\Shacal.project.inc'}
{%File 'crypto\Shacal.uses.inc'}
{%File 'crypto\BlowFish.int.inc'}
{%File 'crypto\BlowFish.err.inc'}
{%File 'crypto\BlowFish.imp.inc'}
{%File 'crypto\BlowFish.test.int.inc'}
{%File 'crypto\BlowFish.test.imp.inc'}
{%File 'crypto\BlowFish.project.inc'}
{%File 'crypto\BlowFish.uses.inc'}
{%File 'crypto\TwoFish.int.inc'}
{%File 'crypto\TwoFish.err.inc'}
{%File 'crypto\TwoFish.imp.inc'}
{%File 'crypto\TwoFish.test.int.inc'}
{%File 'crypto\TwoFish.test.imp.inc'}
{%File 'crypto\TwoFish.project.inc'}
{%File 'crypto\TwoFish.uses.inc'}
{%File 'crypto\THashType.int.inc'}
{%File 'crypto\THashType.err.inc'}
{%File 'crypto\THashType.imp.inc'}
{%File 'crypto\SHA.int.inc'}
{%File 'crypto\SHA.err.inc'}
{%File 'crypto\SHA.imp.inc'}
{%File 'crypto\SHA.test.int.inc'}
{%File 'crypto\SHA.test.imp.inc'}
{%File 'crypto\Tiger.int.inc'}
{%File 'crypto\Tiger.err.inc'}
{%File 'crypto\Tiger.imp.inc'}
{%File 'crypto\Tiger.test.int.inc'}
{%File 'crypto\Tiger.test.imp.inc'}
{%File 'crypto\MD.int.inc'}
{%File 'crypto\MD.err.inc'}
{%File 'crypto\MD.imp.inc'}
{%File 'crypto\MD.test.int.inc'}
{%File 'crypto\MD.test.imp.inc'}
{%File 'crypto\RipeMD.int.inc'}
{%File 'crypto\RipeMD.err.inc'}
{%File 'crypto\RipeMD.imp.inc'}
{%File 'crypto\RipeMD.test.int.inc'}
{%File 'crypto\RipeMD.test.imp.inc'}
{%File 'Hex.int.inc'}
{%File 'Hex.err.inc'}
{%File 'Hex.imp.inc'}
{%File 'kernel\Kernel.pas'}
{%File 'kernel\TItems.int.inc'}
{%File 'kernel\TItems.err.inc'}
{%File 'kernel\TItems.imp.inc'}
{%File 'kernel\ProtoClasses.pas'}
{%File 'kernel\TProtoProperty.int.inc'}
{%File 'kernel\TProtoProperty.err.inc'}
{%File 'kernel\TProtoProperty.imp.inc'}
{%File 'kernel\TProtoProperties.int.inc'}
{%File 'kernel\TProtoProperties.err.inc'}
{%File 'kernel\TProtoProperties.imp.inc'}
{%File 'kernel\TProtoObject.int.inc'}
{%File 'kernel\TProtoObject.err.inc'}
{%File 'kernel\TProtoObject.imp.inc'}
{%File 'kernel\TProtoObjects.int.inc'}
{%File 'kernel\TProtoObjects.err.inc'}
{%File 'kernel\TProtoObjects.imp.inc'}
{%File 'dialogs\DialogClasses.pas'}
{%File 'dialogs\uProtoDialog.pas'}
{%File 'dialogs\TDialogButton.int.inc'}
{%File 'dialogs\TDialogButton.err.inc'}
{%File 'dialogs\TDialogButton.imp.inc'}
{%File 'dialogs\TDialogButtons.int.inc'}
{%File 'dialogs\TDialogButtons.err.inc'}
{%File 'dialogs\TDialogButtons.imp.inc'}
{%File 'dialogs\uMetaDialog.pas'}
{%File 'dialogs\TDialogField.err.inc'}
{%File 'dialogs\TDialogField.int.inc'}
{%File 'dialogs\TDialogField.imp.inc'}
{%File 'dialogs\TDialogFields.err.inc'}
{%File 'dialogs\TDialogFields.int.inc'}
{%File 'dialogs\TDialogFields.imp.inc'}
{%File 'dialogs\TDialogControl.int.inc'}
{%File 'dialogs\TDialogControl.err.inc'}
{%File 'dialogs\TDialogControl.imp.inc'}
{%File 'dialogs\TDialogControls.int.inc'}
{%File 'dialogs\TDialogControls.err.inc'}
{%File 'dialogs\TDialogControls.imp.inc'}
{%File 'dialogs\uTextDialog.pas'}
{%File 'dialogs\uExceptionDialog.pas'}
{%File 'dialogs\uRandomTestDialog.pas'}
{%File 'dialogs\uAsymmetricTestDialog.pas'}
{%File 'dialogs\uSymmetricTestDialog.pas'}
{%File 'dialogs\uHashTestDialog.pas'}

uses
  Windows,
  SysUtils,
  Forms,
  {$I 'png/png.project.inc'}
  {$I 'ac/ac.project.inc'}
  {$I 'gzlib/gzlib.project.inc'}
  {$I 'crypto/Crypto.project.inc'}
  {$I 'kernel/Kernel.project.inc'}
  {$I 'dialogs/Dialogs.project.inc'}
  Utils       in 'Utils.pas',
  Strings     in 'Strings.pas',
  BBCode      in 'BBCode.pas',
  VarRecs     in 'VarRecs.pas',
  Versions    in 'Versions.pas',
  EClasses    in 'EClasses.pas',
  uMiniCrypto in 'uMiniCrypto.pas' {MainForm};

{$R *.res}

begin
    //ReportMemoryLeaksOnShutdown := TRUE;
    SetThreadLocale (1049); { RUSSIAN_CHARSET }
    Application.OnException := TExceptionDialog.OpenExceptionDialog;
    Application.Initialize;
    Application.Title := 'mini-crypto';
    Application.CreateForm (TfmMiniCrypto, MainForm);
    Application.Run;
end.
