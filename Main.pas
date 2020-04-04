unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Edit,
  FMX.Layouts, FMX.ListBox;

type
  TTabForm_Main = class(TForm)
    GestureManager_Main: TGestureManager;
    ToolBar_Main: TToolBar;
    Label_Main: TLabel;
    TabControl_Main: TTabControl;

    TabItem_Client: TTabItem;
    Button_Client_Remote: TButton;
    Edit_Client_Remote_IP: TEdit;
    ListBox_Client: TListBox;
    Edit_Client_Message: TEdit;
    Button_Client_Send: TButton;

    TabItem_Server: TTabItem;
    TabItem_ANIALI: TTabItem;
    TabItem_ProQA: TTabItem;
    StyleBook_Main: TStyleBook;
    Edit_Client_Remote_Port: TEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure Style_Test(Sender: TObject);

  private
  public
end;

var TabForm_Main: TTabForm_Main;

implementation

{$R *.fmx}

procedure TTabForm_Main.FormCreate(Sender: TObject);
begin
  Application.Title := 'Technician Diagnostics';
  TabControl_Main.ActiveTab := TabItem_Client;
end;

procedure TTabForm_Main.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[TabControl_Main.TabCount-1] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControl_Main.ActiveTab <> TabControl_Main.Tabs[0] then
        TabControl_Main.ActiveTab := TabControl_Main.Tabs[TabControl_Main.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

procedure TTabForm_Main.Style_Test(Sender: TObject);
var
  LBItem: TListBoxItem;
begin
  LBItem := TListBoxItem.Create(ListBox_Client);
  LBItem.Parent := ListBox_Client;
  LBItem.StyleLookup := 'Default';
  LBItem.StylesData['Message'] := 'Default';
  ListBox_Client.AddObject(LBItem);

  LBItem := TListBoxItem.Create(ListBox_Client);
  LBItem.Parent := ListBox_Client;
  LBItem.StyleLookup := 'Error';
  LBItem.StylesData['Message'] := 'Error';
  ListBox_Client.AddObject(LBItem);

  LBItem := TListBoxItem.Create(ListBox_Client);
  LBItem.Parent := ListBox_Client;
  LBItem.StyleLookup := 'TX';
  LBItem.StylesData['Message'] := 'TX';
  ListBox_Client.AddObject(LBItem);

  LBItem := TListBoxItem.Create(ListBox_Client);
  LBItem.Parent := ListBox_Client;
  LBItem.StyleLookup := 'RX';
  LBItem.StylesData['Message'] := 'RX';
  ListBox_Client.AddObject(LBItem);
end;

end.
