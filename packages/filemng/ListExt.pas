unit ListExt;
{==================================================================

 Component TListExt  /  Version 1.0  / 03.1999
 =============================================

    Description:
    ============
    My own version of managing sorted lists.

    Author:
    =======
    (c) Ingo Eckel
    Sodener Weg 38
    65812 Bad Soden
    Germany

 ==================================================================}
  

{==================================================================}
interface
{==================================================================}

Uses Classes,
     SysUtils;

Const InitSize   = 500;
      ExtendSize = 500;
      FLess      = -1;
      FEqual     = 0;
      FGreater   = 1;

Type IntType     = Integer;

     TLxDeleteEvent = Procedure(Sender : TObject; Var P : Pointer; Size : Integer) Of Object;

{==================================================================}
     TListExt = Class(TInterfacedobject)
{==================================================================}
     Private
{==================================================================}
       FCount   : IntType;
       FData    : Array of Pointer;
       FSorted  : Boolean;
       fOnDelete: TLxDeleteEvent;
       FItemSize: IntType;
       MaxCount : IntType;

       Function  GetItem  ( I : IntType) : Pointer;
       Procedure FreeItem ( I : IntType);


{==================================================================}
     Public
{==================================================================}
       property data[i : IntType] : Pointer Read GetItem; default;
       Property Sorted : Boolean            Read fSorted;
       Property Count : IntType Read FCount;
       Property ItemSize : IntType Read FItemSize;
       Constructor Create(ItemSize : IntType);
       Procedure Free;
       Procedure Clear;
       Procedure Add(P : Pointer);
       Function  IndexOf(P : Pointer) : IntType;
       Procedure Sort(Compare : TListSortCompare);
       Function  Find(P : Pointer; Compare : TListSortCompare) : Integer;
       Function  FindSequential(P : Pointer; Compare : TListSortCompare) : Integer;
       Function  First : Pointer;
       Function  Last  : Pointer;
       procedure Delete(I: IntType);

{==================================================================}
     Published
{==================================================================}
       Property OnDelete: TLxDeleteEvent Read  fOnDelete
                                         Write fOnDelete;
     End;

{==================================================================}
implementation
{==================================================================}

uses
  Math;

Constructor TListExt.Create(ItemSize : IntType);
Var i : IntType;
Begin
  IF ItemSize < 0 Then
  Raise ERangeError.CreateFmt('TListExt: negative itemsize: %u',[ItemSize]);
  Inherited Create;
  FCount := 0;
  MaxCount := InitSize;
  FSorted  := TRUE;
  FItemSize := ItemSize;
  SetLength(FData, MaxCount+1);
  For i := 0 To MaxCount Do
  FData[i] := NIL;
End; {Create}


Procedure TListExt.Free;
Begin
  Clear;
  FData := NIL;
  Inherited Free;
End;


Procedure TListExt.Add(P : Pointer);

Begin
  IF Fcount = MaxCount Then
  Begin
    INC(MaxCount, ExtendSize);
    SetLength(FData, MaxCount + 1);
  End;


  IF FCount >= MaxCount Then
  Raise ERangeError.CreateFmt('TListExt: buffer overflow: %u',[Fcount]);

  INC (FCount);
  FData[Pred(FCount)] := P;
  FSorted := FALSE;
End; {Add}


Function TListExt.IndexOf(P : Pointer) : IntType;
Var i : IntType;
Begin
  Result := 0;
  IF Not Assigned(P) Then Exit;
  For i := 0 To FCount Do
  IF P = FData[i] Then
  Begin
    Result := i;
    Exit;
  End;
End; {IndexOf}


Procedure TListExt.FreeItem(I : IntType);
Begin
  Begin
    IF Assigned(FData[i]) Then
    Begin
      IF Assigned(fOnDelete) Then
      Begin
        fOnDelete(Self, FData[i], FItemSize);
        IF Assigned(FData[i]) Then
        FreeMem(FData[i], FItemSize);
      End
      Else
      FreeMem(FData[i], FItemSize);
      FData[i] := NIL;
    End;
  End;
End; {FreeItem}


Procedure TListExt.Clear;
Var i : IntType;
Begin
  For i := 0 To Pred(FCount) Do
  Begin
    IF Assigned(FData[i]) Then
      FreeItem(i)
    Else
      Break;
  End;
  FCount := 0;
  FSorted  := TRUE;
  MaxCount := InitSize;
  SetLength(FData, MaxCount + 1);
End; {Clear}


Function TListExt.GetItem(I : IntType) : Pointer;
Begin
  IF (i >= FCount) Then
  Begin
    Raise ERangeError.CreateFmt('TListExt: index out of range: %u',[i]);
    Result := NIL;
    Exit;
  End;
  Result := FData[i];
End; {GetItem}


Function TListExt.First : Pointer;
Begin
  Result := NIL;
  IF Count > 0 Then
  Result := FData[0];
End; {First}


Function TListExt.Last  : Pointer;
Begin
  Result := NIL;
  IF Count > 0 Then
  Result := FData[Pred(FCount)];
End; {Last}



Procedure TListExt.Delete(i : IntType);
Begin
  IF (FCount = 0) Or (i > Pred(FCount)) Then Exit;
  FreeItem(i);

  IF FCount - Succ(i) > 0 Then
    Move(FData[Succ(i)], FData[i], ( FCount - Succ(i) ) * SizeOf(Pointer));

  Dec(FCount);
  FData[FCount] := NIL;
End; {Delete}


Function TListExt.Find(P : Pointer; Compare : TListSortCompare) : Integer;
var nResult   : integer;
    nLow      : integer;
    nHigh     : integer;
    nCompare  : integer;
    nCheckPos : integer;

Begin
  Result := -1;
  IF Not Assigned(P) Or (FCount < 1) Then
  Exit;
  IF not Sorted Then
  Sort(Compare);

  nLow    := 0;
  nHigh   := Count - 1;
  nResult := - 1;
  { Perform a binary search:}
  while (nResult = -1) and (nLow <= nHigh) do
  begin
      nCheckPos := (nLow + nHigh) div 2;
      nCompare := Compare(P, FData[nCheckPos]);
      if (nCompare = fLess) Then nHigh := nCheckPos - 1 { less than }
      else if (nCompare = fGreater) then nLow := nCheckPos + 1 { greater than }
           else nResult := nCheckPos; { equal to }
  end;
  Result := nResult;
End; {Find}


Function TListExt.FindSequential(P : Pointer; Compare : TListSortCompare) : Integer;
Var i : Integer;

Begin
  Result := -1;
  IF Not Assigned(P) Then
  Exit;

  IF Sorted Then
  Result := Find(P, Compare)
  Else
  Begin
    For i := 0 To Pred(FCount) Do
    Begin
      IF Compare(P, FData[i]) = 0 Then
      Begin
        Result := i;
        Exit;
      End;
    End;
  End;
End; {FindSequential}


Procedure TListExt.Sort(Compare : TListSortCompare);

PROCEDURE quicksort(VAR a: Array of Pointer; LO,HI: IntType);

PROCEDURE sort(L,R: IntType);
VAR i,j : IntType;
    x,y : Pointer;

BEGIN
  i := L;
  j := R;
  x := a[(L+R) DIV 2];
  REPEAT
    WHILE Compare(a[i], x) = fLess DO
    i := i + 1;
    WHILE Compare(x, a[j]) = FLess DO
      j := j - 1;
    IF i <= j THEN
    BEGIN
      y:=a[i];
      a[i]:=a[j];
      a[j]:=y;
      i:=i+1;
      j:=j-1;
    END;
  UNTIL i>j;
  IF L<j THEN sort(L,j);
  IF i<R THEN sort(i,R);
END; (* Sort *)

BEGIN
  sort(LO,HI);
END; (* QuickSort *)

Begin
  IF (Self.FCount > 1) And Not Sorted Then
    QuickSort(Self.FData, 0,  Pred(Self.FCount));
  FSorted := TRUE;
End; {Sort}

end.
