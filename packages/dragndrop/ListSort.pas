unit ListSort;

{
  Description
  ===========
    Contains the classes TSortedList and TBatchWork. You will find more
    details in the help-files.


  Disclaimer
  ==========
    The author disclaims all warranties, expressed or implied, including,
    without limitation, the warranties of merchantability and of fitness
    for any purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this component/units.


  Restrictions on Using the Unit / Component
  ==========================================
    This archive and its contents is copyright 1998,99 by Dieter Steinwedel.
    ALL RIGHTS ARE RESERVED BY DIETER STEINWEDEL. You are allowed to use it
    freely subject to the following restrictions:

    • You are not allowed delete or alter the author's name and
      copyright in any manner

    • You are not allowed to publish a copy, modified version or
      compilation neither for payment in any kind nor freely if the
      author has not agreed

    • You are allowed to create a link to the download in the WWW

    These restrictions and terms apply to you as long as you use this
    archive. I won't change these conditions when I have published the
    archive. But I reserve the right to alter these conditions for a
    newer archive version. The archive version is the archive's date.
    Changes can found on my homepage.


  Contact
  =======
    homepage: http://godard.oec.uni-osnabrueck.de/student_home/dsteinwe/delphi/DietersDelphiSite.htm
}
interface

uses Classes, SysUtils;
type
   TCompareFunction = function (Sender: TObject; Item1, Item2: Pointer): integer of object;
   TOnListEvent = procedure (Sender: TObject; Item: Pointer) of object;
   TBatchControlEvent = procedure(Sender: TObject; Item:pointer) of object;

   TSortedList = class(TList)
   private
      FCompare:TCompareFunction;
      FOnAdd:TOnListEvent;
      FOnModify:TOnListEvent;
      FOnErase:TOnListEvent;
      FAllowDuplicates: boolean;
   public
      constructor Create;
      function Add(Item: Pointer): integer;
      procedure Clear; {MP}reintroduce;{/MP}
      procedure Delete(Index:integer);
      function FindObject(Item : Pointer) : integer;
      function Insert(Index: Integer; Item: Pointer): integer;
      function Rearrange(Item: Pointer):integer;
      procedure Remove(Item:Pointer);
      procedure Sort;
      property AllowDuplicates: boolean read FAllowDuplicates write FAllowDuplicates;
      property OnAdd: TOnListEvent read FOnAdd write FOnAdd;
      property OnCompare: TCompareFunction read FCompare write FCompare;
      property OnModify: TOnListEvent read FOnModify write FOnModify;
      property OnErase: TOnListEvent read FOnErase write FOnErase;
   end;

   TBatchControl = class(TComponent)
  private
    FOnProcess: TBatchControlEvent;
    FList: TSortedList;
  public
    property List: TSortedList read FList write FList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property OnProcess: TBatchControlEvent read FOnProcess write FOnProcess;
  end;

procedure Register;

implementation

procedure Register;
begin
  {MP}RegisterComponents({'Misc'}'DragDrop', [TBatchControl]);
end;

{ SortedList ------------------------------------------------------ }

constructor TSortedList.Create;
begin
     inherited Create;
     FAllowDuplicates := true;
end;

function TSortedList.Add(Item : Pointer) : integer;
var
   nCount  : integer;
   bFound  : Boolean;
begin
     nCount := 0;
     bFound := False;
     if Assigned(FCompare) then
     begin
          { search the list of objects until we find the
            correct position for the new object we are adding }
          while (not bFound) and (nCount < Count) do
          begin
               if (FCompare(self,Items[nCount],Item) >= 0) then bFound := True
               else inc(nCount);
          end;
          if bFound then
          begin
               if FAllowDuplicates or (FCompare(self,Items[nCount],Item)<>0) then
               begin
                    inherited Insert(nCount,Item);
                    Result := nCount;
               end
               else Result := -1;
          end else Result := inherited Add(Item);
     end
     else Result:=inherited Add(Item);
     if Assigned(FOnAdd) then FOnAdd(self,Item);
end;

function TSortedList.Insert(Index: Integer; Item: Pointer):integer;
var
   nCount  : integer;
   bFound  : Boolean;
begin
     nCount := 0;
     bFound := False;
     if Assigned(FCompare) then
     begin
          { search the list of objects until we find the
            correct position for the new object we are adding }
          while (not bFound) and (nCount < Count) do
          begin
               if (FCompare(self,Items[nCount],Item) >= 0) then bFound := True
               else inc(nCount);
          end;
          if bFound then
          begin
               if FAllowDuplicates or (FCompare(self,Items[nCount],Item)<>0) then
               begin
                    inherited Insert(nCount,Item);
                    Result := nCount;
               end
               else Result := -1;
          end else Result := inherited Add(Item);
     end
     else
     begin
          inherited Insert(Index,Item);
          Result:=Index;
     end;
     if Assigned(FOnAdd) then FOnAdd(self,Item);
end;

function TSortedList.FindObject(Item : Pointer) : integer;
{ Find the object using the compare method and
  a binary chop search }
var
   nResult   : integer;
   nLow      : integer;
   nHigh     : integer;
   nCompare  : integer;
   nCheckPos : integer;
begin
     nLow := 0;
     nHigh := Count-1;
     nResult := -1;
     { keep searching until found or no more items to search }
     while (nResult = -1) and (nLow <= nHigh) do
     begin
          nCheckPos := (nLow + nHigh) div 2;
          nCompare := FCompare(self, Item,Items[nCheckPos]);
          if (nCompare = -1) then nHigh := nCheckPos - 1 { less than }
          else if (nCompare = 1) then nLow := nCheckPos + 1 { greater than }
               else nResult := nCheckPos; { equal to }
     end;
     FindObject := nResult;
end;

procedure TSortedList.Sort;

   procedure QuickSort(ILo, IHi:integer);
   var Lo, Hi:integer;
       MidItem: pointer;
   begin
        Lo:=ILo;
        Hi:=IHi;
        MidItem:=Items[(Lo+Hi) div 2];
        repeat
              while FCompare(self,Items[Lo],MidItem)=-1 do inc(Lo);
              while FCompare(self,Items[Hi],MidItem)=1 do dec(Hi);
              if Lo<=Hi then
              begin
                   Exchange(Lo,Hi);
                   inc(Lo);
                   dec(Hi);
              end;
        until Lo>Hi;
        if Hi>ILo then QuickSort(ILo,Hi);
        if Lo<IHi then Quicksort(Lo,IHi);
   end;

var i:integer;
begin
     if Assigned(FCompare) then
     begin
          if Count>0 then
          begin
               Quicksort(0,Count-1);
               if (FAllowDuplicates=false) and (count>=2) then
               begin
                    for i:=count-1 downto 1 do
                        if FCompare(self,Items[i],Items[i-1])=0 then
                           delete(i);
               end;
          end;
     end
     else raise Exception.Create('Compare methode is not assigned!')



end;

procedure TSortedList.Clear;
begin
     while Count>0 do Delete(Count-1);
end;

function TSortedList.Rearrange(Item: Pointer):integer;
begin
     Remove(Item);
     Result:=Add(Item);
     if Assigned(FOnModify) then FOnModify(self,Item);
end;

procedure TSortedList.Delete(Index:integer);
begin
     if Assigned(FOnErase) then FOnErase(self,Items[Index]);
     inherited Delete(Index);
end;

procedure TSortedList.Remove(Item:Pointer);
begin
     if Assigned(FOnErase) then FOnErase(self,Item);
     inherited Remove(Item);
end;

{ BatchControl ------------------------------------------------------ }

constructor TBatchControl.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FList:=TSortedList.Create;
end;

destructor TBatchControl.Destroy;
begin
     FList.free;
     inherited Destroy;
end;

procedure TBatchControl.Execute;
begin
     if Assigned(FOnProcess) then
     begin
          while (FList.Count>0) do
          begin
               FOnProcess(self,FList.Items[FList.Count-1]);
               if (FList.Items[FList.Count-1]<>nil) then
                  FList.Delete(FList.Count-1);
          end;
     end
     else raise Exception.Create('No OnProcess-method defined!');
end;

end.
