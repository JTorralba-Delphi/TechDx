unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation;

type
  TTabForm = class(TForm)
    GestureManager: TGestureManager;
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl: TTabControl;
    TabItem_Client: TTabItem;
    TabItem_Server: TTabItem;
    TabItem_ANIALI: TTabItem;
    TabItem_ProQA: TTabItem;

    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

  private

  public

  end;

var
  TabForm: TTabForm;

implementation

{$R *.fmx}

procedure TTabForm.FormCreate(Sender: TObject);
begin
  Application.Title := 'Technician Diagnostics';
  TabControl.ActiveTab := TabItem_Client;
end;

procedure TTabForm.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount-1] then
        TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
        TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

end.
