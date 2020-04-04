unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Edit,
  FMX.Layouts, FMX.ListBox, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient;

type
  TTabForm_Main = class(TForm)
    GestureManager_Main: TGestureManager;
    ToolBar_Main: TToolBar;
    Label_Main: TLabel;
    TabControl_Main: TTabControl;

    TabItem_Client: TTabItem;
    Button_Client_Connect: TButton;
    Edit_Client_Remote_IP: TEdit;
    ListBox_Client: TListBox;
    Edit_Client_Message: TEdit;
    Button_Client_Send: TButton;

    TabItem_Server: TTabItem;
    TabItem_ANIALI: TTabItem;
    TabItem_ProQA: TTabItem;
    StyleBook_Main: TStyleBook;
    Edit_Client_Remote_Port: TEdit;
    TCPClient_Main: TIdTCPClient;

    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure Style_Test(Sender: TObject);

    procedure Button_Client_Connect_OnClick(Sender: TObject);

    Procedure ListBox_Client_Log(Sender: TObject; Message_Type: String; Message :String);
    procedure Button_Client_Send_OnClick(Sender: TObject);
    procedure TCPClient_Main_OnConnected(Sender: TObject);
    procedure TCPClient_Main_OnDisconnected(Sender: TObject);


  private
  public
end;

var TabForm_Main: TTabForm_Main;
var Client_Connected: Boolean;

implementation

{$R *.fmx}

procedure TTabForm_Main.FormCreate(Sender: TObject);
begin
  Application.Title := 'Technician Diagnostics';
  TabControl_Main.ActiveTab := TabItem_Client;
  Client_Connected := False;
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

procedure TTabForm_Main.TCPClient_Main_OnConnected(Sender: TObject);
begin
  ListBox_Client_Log(Sender, 'Default', 'Client connected to server.');
end;

procedure TTabForm_Main.TCPClient_Main_OnDisconnected(Sender: TObject);
begin
  ListBox_Client_Log(Sender, 'Default', 'Client disconnected from server.');
end;

procedure TTabForm_Main.Button_Client_Connect_OnClick(Sender: TObject);
begin
  Client_Connected := not Client_Connected;
  if (Client_Connected)
  then
    begin
      try
        TCPClient_Main.Host := Edit_Client_Remote_IP.Text;
        TCPClient_Main.Port := Edit_Client_Remote_Port.Text.ToInteger;
        TCPClient_Main.Connect();
        Edit_Client_Message.Enabled := True;
        Button_Client_Send.Enabled := True;
        Button_Client_Connect.Text := 'Disconnect';
      except
        Client_Connected := False;
        ShowMessage('Connect failed.');
      end;
    end
  else
    begin
      try
        TCPClient_Main.Disconnect();
      finally
        Edit_Client_Message.Enabled := False;
        Button_Client_Send.Enabled := False;
        Button_Client_Connect.Text := 'Connect';
      end
    end
end;

procedure TTabForm_Main.ListBox_Client_Log(Sender: TObject; Message_Type: String; Message: String);
var
  Item: TListBoxItem;
begin
  Item := TListBoxItem.Create(ListBox_Client);
  Item.Parent := ListBox_Client;
  Item.StyleLookup := Message_Type;
  Item.StylesData['Message'] := Message;
  ListBox_Client.AddObject(Item);
end;

procedure TTabForm_Main.Button_Client_Send_OnClick(Sender: TObject);
begin
  try
    TCPClient_Main.Socket.WriteLn(Edit_Client_Message.Text);
    ListBox_Client_Log(Sender, 'TX', Edit_Client_Message.Text);
  except
    Button_Client_Connect_OnClick(Sender);
    ShowMessage('Send failed.');
  end
end;

end.
