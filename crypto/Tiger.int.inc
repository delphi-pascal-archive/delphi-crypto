{******************************************************************************}
{*  Tiger.int.inc                                                             *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PTigerCtx = ^TTigerCtx;
    TTigerCtx = record
        State         : array [0..2] of Int64;
        Length        : Int64;
        CurrentLength : Int64;
        Buffer        : array [0..63] of Byte;
    end;

procedure TigerInit (var ctx: TTigerCtx);
procedure TigerUpdate (var ctx: TTigerCtx; aBuffer: Pointer; aLength: LongWord; aPASSES: LongWord);
function TigerFinal (var ctx: TTigerCtx; aSize: WORD; aPASSES: LongWord) : String;

function Tiger128 (const aValue: String) : String; overload;
function Tiger128 (const aValue: PChar) : String; overload;
function Tiger128 (const aStream: TStream;
                   const aBlockSize: LongWord = 1024) : String; overload;
function Tiger128File (const aFileName: String;
                       const aBlockSize: LongWord = 1024) : String;

function Tiger160 (const aValue: String) : String; overload;
function Tiger160 (const aValue: PChar) : String; overload;
function Tiger160 (const aStream: TStream;
                   const aBlockSize: LongWord = 1024) : String; overload;
function Tiger160File (const aFileName: String;
                       const aBlockSize: LongWord = 1024) : String;

function Tiger192 (const aValue: String) : String; overload;
function Tiger192 (const aValue: PChar) : String; overload;
function Tiger192 (const aStream: TStream;
                   const aBlockSize: LongWord = 1024) : String; overload;
function Tiger192File (const aFileName: String;
                       const aBlockSize: LongWord = 1024) : String;

function TTH128 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function TTH128 (const aFileName: String;
                 const aBlockSize: LongWord = 1024) : String; overload;

function TTH160 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function TTH160 (const aFileName: String;
                 const aBlockSize: LongWord = 1024) : String; overload;

function TTH192 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function TTH192 (const aFileName: String;
                 const aBlockSize: LongWord = 1024) : String; overload;
