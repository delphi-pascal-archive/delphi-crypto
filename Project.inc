{******************************************************************************}
{*  Project.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
    {$I 'png/png.project.inc'}
    {$I 'ac/ac.project.inc'}
    {$I 'crypto/Crypto.project.inc'}
    {$I 'kernel/Kernel.project.inc'}
    {$I 'dialogs/Dialogs.project.inc'}
    {$I 'engine/Engine.project.inc'}
    SQLite3           in 'SQLite3.pas',
    SQLite3DLL        in 'sqlite3dll.pas',
    SQLiteTable3      in 'SQLiteTable3.pas',
    Utils             in 'Utils.pas',
    Strings           in 'Strings.pas',
    BBCode            in 'BBCode.pas',
    VarRecs           in 'VarRecs.pas',
    Versions          in 'Versions.pas',
    EClasses          in 'EClasses.pas',
    DLLThreads        in 'DLLThreads.pas',
    uUsersLoader      in 'uUsersLoader.pas',
    uCategoriesLoader in 'uCategoriesLoader.pas',
    uTmpMessage       in 'uTmpMessage.pas',
    uMessagesLoader   in 'uMessagesLoader.pas',
