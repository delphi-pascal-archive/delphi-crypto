program Main;

{%File 'std.inc'}
{%File 'Hex.int.inc'}
{%File 'Hex.err.inc'}
{%File 'Hex.imp.inc'}
{%File 'gzlib\gZLibEx.pas'}
{%File 'gzlib\gZLibExApi.pas'}
{%File 'gzlib\gZLibExGZ.pas'}
{%File 'png\pngextra.pas'}
{%File 'png\pngimage.pas'}
{%File 'png\pnglang.pas'}
{%File 'ac\acPNG.pas'}
{%File 'HTTP.inc'}
{%File 'xml\XMLUtils.pas'}
{%File 'xml\XMLParser.pas'}
{%File 'xml\XMLDOM.pas'}
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
{%File 'kernel\CryptoSystem.pas'}
{%File 'kernel\TCrypto.int.inc'}
{%File 'kernel\TCrypto.err.inc'}
{%File 'kernel\TCrypto.imp.inc'}
{%File 'kernel\CryptoClasses.pas'}
{%File 'kernel\TCryptoProperty.int.inc'}
{%File 'kernel\TCryptoProperty.err.inc'}
{%File 'kernel\TCryptoProperty.imp.inc'}
{%File 'kernel\TCryptoObject.int.inc'}
{%File 'kernel\TCryptoObject.err.inc'}
{%File 'kernel\TCryptoObject.imp.inc'}
{%File 'kernel\TCryptoObjects.int.inc'}
{%File 'kernel\TCryptoObjects.err.inc'}
{%File 'kernel\TCryptoObjects.imp.inc'}
{%File 'kernel\MetaClasses.pas'}
{%File 'kernel\TMetaProperty.int.inc'}
{%File 'kernel\TMetaProperty.err.inc'}
{%File 'kernel\TMetaProperty.imp.inc'}
{%File 'kernel\TMetaObject.int.inc'}
{%File 'kernel\TMetaObject.err.inc'}
{%File 'kernel\TMetaObject.imp.inc'}
{%File 'kernel\TMetaObjects.int.inc'}
{%File 'kernel\TMetaObjects.err.inc'}
{%File 'kernel\TMetaObjects.imp.inc'}
{%File 'kernel\ParaClasses.pas'}
{%File 'kernel\TParaObject.int.inc'}
{%File 'kernel\TParaObject.err.inc'}
{%File 'kernel\TParaObject.imp.inc'}
{%File 'kernel\TParaObjects.int.inc'}
{%File 'kernel\TParaObjects.err.inc'}
{%File 'kernel\TParaObjects.imp.inc'}
{%File 'kernel\HypoClasses.pas'}
{%File 'kernel\THypoObject.int.inc'}
{%File 'kernel\THypoObject.err.inc'}
{%File 'kernel\THypoObject.imp.inc'}
{%File 'kernel\THypoObjects.int.inc'}
{%File 'kernel\THypoObjects.err.inc'}
{%File 'kernel\THypoObjects.imp.inc'}
{%File 'kernel\HyperClasses.pas'}
{%File 'kernel\THyperObject.int.inc'}
{%File 'kernel\THyperObject.err.inc'}
{%File 'kernel\THyperObject.imp.inc'}
{%File 'kernel\THyperObjects.int.inc'}
{%File 'kernel\THyperObjects.err.inc'}
{%File 'kernel\THyperObjects.imp.inc'}
{%File 'kernel\HashTable.pas'}
{%File 'kernel\THashItem.int.inc'}
{%File 'kernel\THashItem.err.inc'}
{%File 'kernel\THashItem.imp.inc'}
{%File 'kernel\THashList.int.inc'}
{%File 'kernel\THashList.err.inc'}
{%File 'kernel\THashList.imp.inc'}
{%File 'engine\Engine.pas'}
{%File 'engine\TPic.int.inc'}
{%File 'engine\TPic.err.inc'}
{%File 'engine\TPic.imp.inc'}
{%File 'engine\TPics.int.inc'}
{%File 'engine\TPics.err.inc'}
{%File 'engine\TPics.imp.inc'}
{%File 'engine\TUser.int.inc'}
{%File 'engine\TUser.err.inc'}
{%File 'engine\TUser.imp.inc'}
{%File 'engine\TUsers.int.inc'}
{%File 'engine\TUsers.err.inc'}
{%File 'engine\TUsers.imp.inc'}
{%File 'engine\UsersList.int.inc'}
{%File 'engine\UsersList.err.inc'}
{%File 'engine\UsersList.imp.inc'}
{%File 'engine\TMessageType.int.inc'}
{%File 'engine\TMessageType.err.inc'}
{%File 'engine\TMessageType.imp.inc'}
{%File 'engine\TMessageStatus.int.inc'}
{%File 'engine\TMessageStatus.err.inc'}
{%File 'engine\TMessageStatus.imp.inc'}
{%File 'engine\TMessage.int.inc'}
{%File 'engine\TMessage.err.inc'}
{%File 'engine\TMessage.imp.inc'}
{%File 'engine\TMessages.int.inc'}
{%File 'engine\TMessages.err.inc'}
{%File 'engine\TMessages.imp.inc'}
{%File 'engine\MessagesList.int.inc'}
{%File 'engine\MessagesList.err.inc'}
{%File 'engine\MessagesList.imp.inc'}
{%File 'engine\TCategorieType.int.inc'}
{%File 'engine\TCategorieType.err.inc'}
{%File 'engine\TCategorieType.imp.inc'}
{%File 'engine\TCategorieStatus.int.inc'}
{%File 'engine\TCategorieStatus.err.inc'}
{%File 'engine\TCategorieStatus.imp.inc'}
{%File 'engine\TCategorie.int.inc'}
{%File 'engine\TCategorie.err.inc'}
{%File 'engine\TCategorie.imp.inc'}
{%File 'engine\TCategories.int.inc'}
{%File 'engine\TCategories.err.inc'}
{%File 'engine\TCategories.imp.inc'}
{%File 'engine\CategoriesTree.int.inc'}
{%File 'engine\CategoriesTree.err.inc'}
{%File 'engine\CategoriesTree.imp.inc'}
{%File 'engine\TKeyWord.int.inc'}
{%File 'engine\TKeyWord.err.inc'}
{%File 'engine\TKeyWord.imp.inc'}
{%File 'engine\TKeyWords.int.inc'}
{%File 'engine\TKeyWords.err.inc'}
{%File 'engine\TKeyWords.imp.inc'}
{%File 'engine\TPackageType.int.inc'}
{%File 'engine\TPackageType.err.inc'}
{%File 'engine\TPackageType.imp.inc'}
{%File 'engine\TPackageStatus.int.inc'}
{%File 'engine\TPackageStatus.err.inc'}
{%File 'engine\TPackageStatus.imp.inc'}
{%File 'engine\TPackage.int.inc'}
{%File 'engine\TPackage.err.inc'}
{%File 'engine\TPackage.imp.inc'}
{%File 'engine\TPackages.int.inc'}
{%File 'engine\TPackages.err.inc'}
{%File 'engine\TPackages.imp.inc'}
{%File 'dialogs\DialogClasses.pas'}
{%File 'dialogs\uProtoDialog.pas'}
{%File 'dialogs\TDialogButton.int.inc'}
{%File 'dialogs\TDialogButton.err.inc'}
{%File 'dialogs\TDialogButton.imp.inc'}
{%File 'dialogs\TDialogButtons.int.inc'}
{%File 'dialogs\TDialogButtons.err.inc'}
{%File 'dialogs\TDialogButtons.imp.inc'}
{%File 'dialogs\uTextDialog.pas'}
{%File 'dialogs\uExceptionDialog.pas'}
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
{%File 'dialogs\uWizardDialog.pas'}
{%File 'dialogs\uColorDialog.pas'}
{%File 'dialogs\uIconDialog.pas'}
{%File 'dialogs\uSmileDialog.pas'}
{%File 'dialogs\uLoginDialog.pas'}
{%File 'dialogs\uRegisterDialog.pas'}
{%File 'dialogs\uRandomTestDialog.pas'}
{%File 'dialogs\uAsymmetricTestDialog.pas'}
{%File 'dialogs\uSymmetricTestDialog.pas'}
{%File 'dialogs\uHashTestDialog.pas'}
{%File 'dialogs\uCategorieDialog.pas'}
{%File 'dialogs\uMessageDialog.pas'}
{%File 'dialogs\uUserDialog.pas'}

{$I 'std.inc'}

uses
    Windows,
    SysUtils,
    Forms,
    {$I 'png/png.project.inc'}
    {$I 'ac/ac.project.inc'}
    {$I 'gzlib/gzlib.project.inc'}
    {$I 'synapse/Synapse.project.inc'}
    {$I 'xml/xml.project.inc'}
    {$I 'crypto/Crypto.project.inc'}
    {$I 'kernel/Kernel.project.inc'}
    {$I 'dialogs/Dialogs.project.inc'}
    {$I 'engine/Engine.project.inc'}
    SQLite3              in 'SQLite3.pas',
    SQLite3DLL           in 'sqlite3dll.pas',
    SQLiteTable3         in 'SQLiteTable3.pas',
    Utils                in 'Utils.pas',
    Strings              in 'Strings.pas',
    Fonts                in 'Fonts.pas',
    BBCode               in 'BBCode.pas',
    VarRecs              in 'VarRecs.pas',
    Versions             in 'Versions.pas',
    EClasses             in 'EClasses.pas',
    DLLThreads           in 'DLLThreads.pas',
    uUsersLoader         in 'uUsersLoader.pas',
    uCategoriesLoader    in 'uCategoriesLoader.pas',
    uTmpMessage          in 'uTmpMessage.pas',
    uMessagesLoader      in 'uMessagesLoader.pas',
    uKeyWordsLoader      in 'uKeyWordsLoader.pas',
    uPackagesScanner     in 'uPackagesScanner.pas',
    uPackagesConstructor in 'uPackagesConstructor.pas',
    {$IFDEF HTTP}
    HTTPServer           in 'HTTPServer.pas',
    HTTPClient           in 'HTTPClient.pas',
    uHTTPServer          in 'uHTTPServer.pas',
    uHTTPClient          in 'uHTTPClient.pas',
    {$ENDIF HTTP}
    {$IFDEF SMTP_POP3}
    SMTPClient           in 'SMTPClient.pas',
    POP3Client           in 'POP3Client.pas',
    uSMTPClient          in 'uSMTPClient.pas',
    uPOP3Client          in 'uPOP3Client.pas',
    {$ENDIF SMTP_POP3}
    uMain                in 'uMain.pas' {MainForm};

{$R *.res}

begin
    //ReportMemoryLeaksOnShutdown := TRUE;
    SetThreadLocale (1049); { RUSSIAN_CHARSET }
    Application.OnException := TExceptionDialog.OpenExceptionDialog;
    RegisterFonts ( ExtractFilePath (Application.ExeName) + 'Fonts' ); { import fonts }
    Application.Initialize;
    if Assigned (User) then
    begin
        Application.CreateForm (TfmMain,MainForm);
        if Assigned (MainForm) then
            MainForm.GetData;
    end;
    Application.Run;
end.
