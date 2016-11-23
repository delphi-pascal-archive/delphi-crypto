{******************************************************************************}
{*  SHA.int.inc                                                               *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
    PSHA512Ctx = ^TSHA512Ctx;
    TSHA512Ctx = record
        State         : array [0..7] of Int64;
        Length        : Int64;
        CurrentLength : Int64;
        Buffer        : array [0..127] of Byte;
    end;

    PSHA256Ctx = ^TSHA256Ctx;
    TSHA256Ctx = record
        State         : array [0..7] of LongWord;
        Length        : Int64;
        CurrentLength : Int64;
        Buffer        : array [0..63] of Byte;
    end;

procedure SHA384Init (var ctx: TSHA512Ctx);
procedure SHA512Init (var ctx: TSHA512Ctx);
procedure SHA512Update (var ctx: TSHA512Ctx; aBuffer: Pointer; aLength: Int64);
function SHA512Final (var ctx: TSHA512Ctx; aSize: Word) : String;

procedure SHA1Init (var ctx: TSHA256Ctx);
procedure SHA256Init (var ctx: TSHA256Ctx);
procedure SHA256Update (var ctx: TSHA256Ctx; aBuffer: Pointer; aLength: Int64; aSize: Word);
function SHA256Final (var ctx: TSHA256Ctx; aSize: Word) : String;

function SHA1 (const aValue: String) : String; overload;
function SHA1 (const aValue: PChar) : String; overload;
function SHA1 (const aStream: TStream;
               const aBlockSize: LongWord = 1024) : String; overload;
function SHA1File (const aFileName: String;
                   const aBlockSize: LongWord = 1024) : String;

function SHA256 (const aValue: String) : String; overload;
function SHA256 (const aValue: PChar) : String; overload;
function SHA256 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function SHA256File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;

function SHA384 (const aValue: String) : String; overload;
function SHA384 (const aValue: PChar) : String; overload;
function SHA384 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function SHA384File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;

function SHA512 (const aValue: String) : String; overload;
function SHA512 (const aValue: PChar) : String; overload;
function SHA512 (const aStream: TStream;
                 const aBlockSize: LongWord = 1024) : String; overload;
function SHA512File (const aFileName: String;
                     const aBlockSize: LongWord = 1024) : String;