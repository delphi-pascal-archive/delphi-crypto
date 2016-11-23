{******************************************************************************}
{*  MessagesList.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ MessagesList }
function CreateMessageNode : PMessageNode;
begin
    Result := AllocMem ( SizeOf (TMessageNode) );
    Result^.ID := 0;
    Result^.IDCategorie := 0;
    Result^.IDAuthor := 0;
    Result^.IDType := 0;
    Result^.IDStatus := 0;
    Result^.IDPic := 0;
    Result^.IndexPic := 0;
    Result^.Subject := '';
    Result^.Text := AllocMem ( SizeOf (Char) * MAX_MESSAGE_LENGTH + 1 );
    Result^.Author := '';
    Result^.TimeStampCreate := 0.0;
    Result^.TimeStampModify := 0.0;
    Result^.TimeStampPublic := 0.0;
    Result^.KeyHash := '';
    Result^.DataHash := '';
end;

procedure FreeMessageNode (aValue: PMessageNode);
begin
    if Assigned (aValue) then
    try
        aValue^.ID := 0;
        aValue^.IDCategorie := 0;
        aValue^.IDAuthor := 0;
        aValue^.IDType := 0;
        aValue^.IDStatus := 0;
        aValue^.IDPic := 0;
        aValue^.IndexPic := 0;
        _FillChar ( aValue^.Subject, Length (aValue^.Subject), $00 );
        ZeroMemory ( aValue^.Text, MAX_MESSAGE_LENGTH );
        Dispose (aValue^.Text);
        _FillChar ( aValue^.Author, Length (aValue^.Author), $00 );
        aValue^.TimeStampCreate := 0.0;
        aValue^.TimeStampModify := 0.0;
        aValue^.TimeStampPublic := 0.0;
        _FillChar ( aValue^.KeyHash, Length (aValue^.KeyHash), $00 );
        _FillChar ( aValue^.DataHash, Length (aValue^.DataHash), $00 );
    finally
        Dispose (aValue);
        aValue := NIL;
    end;
end;
