unit AssociatedStatusBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

const
  DefaultFileInfoFormat = '%s of %s in %s of %s';

type
  TCustomAssociatedStatusBar = class;
  TStatusFileInfo = record
    FilesCount: Integer;
    SelectedCount: Integer;
    FilesSize: Int64;
    SelectedSize: Int64;
  end;
  TFormatFileInfoEvent = procedure(Sender: TCustomAssociatedStatusBar;
    const FileInfo: TStatusFileInfo; var Text: string) of object;

  TCustomAssociatedStatusBar = class(TStatusBar)
  private
    FFileInfo: TStatusFileInfo;
    FFileInfoFormat: string;
    FFileInfoPanel: Integer;
    FFocusControl: TWinControl;
    FOnFormatFileInfo: TFormatFileInfoEvent;
    procedure SetFileInfo(Value: TStatusFileInfo);
    procedure SetFileInfoFormat(Value: string);
    procedure SetFileInfoPanel(Value: Integer);
    procedure SetFocusControl(Value: TWinControl);
    function StoreFileInfoFormat: Boolean;
  protected
    procedure Click; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateData;
  public
    constructor Create(AOwner: TComponent); override;
    property FileInfo: TStatusFileInfo read FFileInfo write SetFileInfo;
    property FileInfoFormat: string read FFileInfoFormat write SetFileInfoFormat
      stored StoreFileInfoFormat;
    property FileInfoPanel: Integer read FFileInfoPanel write SetFileInfoPanel default 0;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property OnFormatFileInfo: TFormatFileInfoEvent read FOnFormatFileInfo write FOnFormatFileInfo;
    property SimplePanel default False;
  end;

type
  TAssociatedStatusBar = class(TCustomAssociatedStatusBar)
  published
    property FocusControl;
    property FileInfoFormat;
    property FileInfoPanel;
    property OnFormatFileInfo;
  end;

procedure Register;

function FormatBytes(Bytes: Int64): string;

const
  FormatBytesAbove: Int64 = Int64(100*1024);

implementation

function FormatBytes(Bytes: Int64): string;
begin
  if Bytes < FormatBytesAbove then
      Result := FormatFloat('#,##0 "B"', Bytes)
    else
  if Bytes < Int64(100*1024*1024) then
      Result := FormatFloat('#,##0 "KB"', Bytes div 1024)
    else
      Result := FormatFloat('#,##0 "MB"', Bytes div (1024*1024))
end;

procedure Register;
begin
  RegisterComponents('Martin', [TAssociatedStatusBar]);
end;

constructor TCustomAssociatedStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  FFocusControl := nil;
  FOnFormatFileInfo := nil;
  SimplePanel := False;
  Panels.Add;
  FFileInfoPanel := 0;
  FFileInfoFormat := DefaultFileInfoFormat;
  UpdateData;
end; { Create }

procedure TCustomAssociatedStatusBar.SetFileInfo(Value: TStatusFileInfo);
begin
  if not CompareMem(@FFileInfo, @Value, SizeOf(FFileInfo)) then
  begin
    FFileInfo := Value;
    UpdateData;
  end;
end; { SetFileInfo }

procedure TCustomAssociatedStatusBar.SetFileInfoFormat(Value: string);
begin
  if FFileInfoFormat <> Value then 
  begin
    FFileInfoFormat := Value;
    UpdateData;
  end;
end; { SetFileInfoFormat }

procedure TCustomAssociatedStatusBar.SetFileInfoPanel(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value >= Panels.Count then Value := Panels.Count - 1;  
  if FFileInfoPanel <> Value then
  begin
    if FFileInfoPanel in [0..Panels.Count-1] then
      Panels[FFileInfoPanel].Text := '';
    FFileInfoPanel := Value;
    UpdateData;
  end;
end; { SetFileInfoPanel }

procedure TCustomAssociatedStatusBar.SetFocusControl(Value: TWinControl);
begin
  if FocusControl <> Value then
    FFocusControl := Value;
end; { SetFocusControl }

procedure TCustomAssociatedStatusBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FocusControl) then
    FocusControl := nil;
  inherited;
end; { Notification }

procedure TCustomAssociatedStatusBar.Click;
begin
  inherited;
  if Assigned(FocusControl) then FocusControl.SetFocus;
end; { Click }

procedure TCustomAssociatedStatusBar.UpdateData;
var
  Text: string;
begin
  if FileInfoPanel in [0..Panels.Count-1] then
    with FileInfo do
    begin
      Text := Format(FileInfoFormat,
        [FormatBytes(SelectedSize),
         FormatBytes(FilesSize),
         FormatFloat('#,##0', SelectedCount),
         FormatFloat('#,##0', FilesCount)]);
      if Assigned(OnFormatFileInfo) then
        OnFormatFileInfo(Self, FileInfo, Text);
      Panels[FileInfoPanel].Text := Text;
    end;
end; { UpdateData }

function TCustomAssociatedStatusBar.StoreFileInfoFormat: Boolean;
begin
  Result := (FileInfoFormat <> DefaultFileInfoFormat);
end; { StoreFileInfoFormat }

end.
