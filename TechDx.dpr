program TechDx;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {TabForm_Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabForm_Main, TabForm_Main);
  Application.Run;
end.
