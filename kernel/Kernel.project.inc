{******************************************************************************}
{*  Kernel.project.inc                                                        *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
    Kernel        in 'kernel/Kernel.pas',
    ProtoClasses  in 'kernel/ProtoClasses.pas',
{$IFNDEF MINI_CRYPTO}
    CryptoSystem  in 'kernel/CryptoSystem.pas',
    CryptoClasses in 'kernel/CryptoClasses.pas',
    MetaClasses   in 'kernel/MetaClasses.pas',
    ParaClasses   in 'kernel/ParaClasses.pas',
    HypoClasses   in 'kernel/HypoClasses.pas',
    HyperClasses  in 'kernel/HyperClasses.pas',
    HashTable     in 'kernel/HashTable.pas',
{$ENDIF MINI_CRYPTO}
