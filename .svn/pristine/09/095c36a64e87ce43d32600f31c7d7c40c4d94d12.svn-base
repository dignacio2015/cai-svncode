unit oldcode;

{$mode delphi}

interface

uses
  Classes, SysUtils;

implementation

{
function TMiningAgent.TestaAcao(pCurrentState:array of byte;NAcao:word):single;
begin
pCurrentState[0]:=NAcao+1;
PredictNextState(pCurrentState,NAcao);
if LearningAndPredict.FCache.ValidEntry(pCurrentState)
   then TestaAcao:=1
   else TestaAcao:=-1;
end;
function TForm1.DS:extended; // Distancias
var J:integer;
    R:extended;
begin
R:=0;
D:=PP.GetRelativePositions(0);
for J:=0 to 3 do  // etrada da rede neural
    begin
    //R:=R+sqrt(D[J].X*D[J].X+D[J].Y*D[J].Y);
    R:=R+sqrt(sqr(D[J].X)+sqr(D[J].Y));
    end;
DS:=R;
end;

procedure TForm1.ZeraEstados;
begin
end;


function TForm1.Testa2N:extended;
begin
Testa2N:=-Testa2;
end;



}


end.

