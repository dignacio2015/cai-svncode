program testcnnalgo;
(*
 Coded by Joao Paulo Schwarz Schuler.
 // https://sourceforge.net/p/cai
 This command line tool runs the CAI Convolutional Neural Network with
 CIFAR10 files.

 In the case that your processor supports AVX instructions, uncomment
 {$DEFINE AVX} or {$DEFINE AVX2} defines. Also have a look at AVX512 define.

 Look at TTestCNNAlgo.WriteHelp; for more info.
*)
{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  uconvolutionneuralnetwork,
  uvolume,
  Math,
  ucifar10;

const csLogEvery =
{$IFDEF Release}
  1000
{$ELSE}
  100
{$ENDIF}
;

type
  { TTestCNNAlgo }
  TTestCNNAlgo = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure RunAlgo(iAlgo: integer; fLearningRate, fInertia, fTarget: single);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { testcnnalgo }
  procedure TTestCNNAlgo.DoRun;
  var
    Algo, LearningRate, Inertia, Target: string;
    iAlgo: integer;
    fLearningRate, fInertia, fTarget: single;
  begin
    // quick check parameters
    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
    end;

    fLearningRate := 0.01;
    if HasOption('l', 'learningrate') then
    begin
      LearningRate := GetOptionValue('l', 'learningrate');
      fLearningRate := StrToFloat(LearningRate);
    end;

    fInertia := 0.9;
    if HasOption('i', 'inertia') then
    begin
      Inertia := GetOptionValue('i', 'inertia');
      fInertia := StrToFloat(Inertia);
    end;

    fTarget := 0.8;
    if HasOption('t', 'target') then
    begin
      Target := GetOptionValue('t', 'target');
      fTarget := StrToFloat(Target);
    end;

    if HasOption('a', 'algo') then
    begin
      Algo := GetOptionValue('a', 'algo');
      Writeln('Running algorithm:[',Algo,']');
      iAlgo := StrToInt(Algo);

      if (iAlgo > 0) and (iAlgo < 14) then
      begin
        RunAlgo(iAlgo, fLearningRate, fInertia, fTarget);
      end
      else
      begin
        WriteLn('Bad algorithm number:',iAlgo);
      end;
    end
    else
    begin
      {$IFDEF Release}
      WriteHelp;
      Write('Press ENTER to quit.');
      ReadLn();
      {$ELSE}
      iAlgo := 5;
      RunAlgo(iAlgo, fLearningRate, fInertia, fTarget);
      {$ENDIF}
    end;

    Terminate;
    Exit;
  end;

  procedure TTestCNNAlgo.RunAlgo(iAlgo: integer; fLearningRate, fInertia, fTarget: single);
  var
    NN: TNNet;
    I: integer;
    ImgVolumes, ImgTestVolumes: TNNetVolumeList;
    ImgInput, ImgInputCp: TNNetVolume;
    Volume: TNNetVolume;
    pOutput, vOutput: TNNetVolume;
    hit, miss: integer;
    NumClasses: integer;
    ErrorSum, LastError: TNeuralFloat;
    startTime, totalTimeSeconds: double;
    globalStartTime: double;
    CurrentRate: double;
    globalLoopCnt: integer;
    imgIdx: integer;
    CurrentLearningRate: TNeuralFloat;
    fileName, fileNameBase, FileNameCSV: string;
    TA,TB,TC: TNNetLayer;
    CVSError, CVSLoss, CVSRate,
    CVSTestError, CVSTestLoss, CVSTestRate: TNeuralFloat;
    CSVFile: TextFile;
    CropSizeX, CropSizeY: integer;
  begin
    if not CheckCIFARFile() then exit;
    WriteLn('Creating Neural Network...');
    ImgVolumes := TNNetVolumeList.Create();
    ImgTestVolumes := TNNetVolumeList.Create();
    ImgInput := TNNetVolume.Create();
    ImgInputCp := TNNetVolume.Create();
    NumClasses  := 10;
    CurrentRate := 0;
    CVSLoss     := 0;
    CVSRate     := 0;
    CurrentRate := 0;

    hit := 0;
    miss := 0;
    NN := TNNet.Create();
    fileNameBase := 'autosave-neuralnetwork_a'+IntToStr(iAlgo);

    WriteLn('File name is: ',fileNameBase);

    FileNameCSV := fileNameBase + '.csv';

    FileName := fileNameBase + '.nn';

    AssignFile(CSVFile, FileNameCSV);
    ReWrite(CSVFile);
    writeln(CSVFile, 'training rate,training loss,training error,testing rate,testing loss,testing error');

    case iAlgo of
      1:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(128, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(128, 5, 0, 0));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(64));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
      2:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(64));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
      3:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 0));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
      4:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 3, 1, 2));
        NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(32, 3, 1, 2));
        NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 1));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
      5:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 4));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
        fLearningRate := Min(fLearningRate, 0.001);
      end;
      6:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 1));
        NN.AddLayer(TNNetMaxPool.Create(4));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
        fLearningRate := Min(fLearningRate, 0.001);
      end;
      7:
      begin
        //RELU + LOCAL CONNECT TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 1));
        NN.AddLayer(TNNetMaxPool.Create(4));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 3, 1, 1));
        NN.AddLayer(TNNetLocalConnectReLU.Create(10, 1, 0, 1));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
        fLearningRate := Min(fLearningRate, 0.001);
      end;
      8:
      begin
        //RELU TESTING
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(20, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(20, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(160));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
      9:
      begin
        // Hiperbolic Tangent TESTING I
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolution.Create(64, 3, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(10));
        NN.AddLayer(TNNetConvolution.Create(64, 3, 0, 0));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(256));
        NN.AddLayer(TNNetLayerFullConnect.Create(NumClasses));
      end;
      10:
      begin
        // Hiperbolic Tangent TESTING II
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolution.Create(16, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolution.Create(32, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolution.Create(32, 5, 0, 0));
        NN.AddLayer(TNNetLayerFullConnect.Create(32));
        NN.AddLayer(TNNetLayerFullConnect.Create(NumClasses));
      end;
      11:
      begin
        // Hiperbolic Tangent TESTING III
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolution.Create(16, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolution.Create(20, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolution.Create(20, 5, 2, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetLayerFullConnect.Create(NumClasses));
      end;
      12:
      begin
        //Reshape, Identity, LocalConnect testing
        NN.AddLayer(TNNetInput.Create(32, 32, 3));
        NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(128, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetLocalConnectReLU.Create(128, 5, 0, 0));
        NN.AddLayer(TNNetReshape.Create(128,1,1));
        NN.AddLayer(TNNetIdentity.Create());
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(64));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());

        // This algorith requires special inertia
        fInertia := Max(fInertia, 0.999);
      end;
      13:
      begin
        // Network that splits into 2 branches and then later is concatenated
        TA := NN.AddLayer(TNNetInput.Create(32, 32, 3));

        // First branch starting from TA (5x5 features)
        NN.AddLayerAfter(TNNetConvolutionReLU.Create(16, 5, 0, 0),TA);
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        TB := NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));

        // Another branch starting from TA (3x3 features)
        NN.AddLayerAfter(TNNetConvolutionReLU.Create(16, 3, 0, 0),TA);
        NN.AddLayer(TNNetMaxPool.Create(2));
        NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));
        NN.AddLayer(TNNetMaxPool.Create(2));
        TC := NN.AddLayer(TNNetConvolutionReLU.Create(64, 6, 0, 0));

        // Concats both branches so the NN has only one end.
        NN.AddLayer(TNNetConcat.Create([TB,TC]));
        NN.AddLayer(TNNetLayerFullConnectReLU.Create(64));
        NN.AddLayer(TNNetFullConnectLinear.Create(NumClasses));
        NN.AddLayer(TNNetSoftMax.Create());
      end;
    end;
    WriteLn('Learning rate set to: [',fLearningRate:7:5,']');
    WriteLn('Inertia set to: [',fInertia:7:5,']');
    WriteLn('Target set to: [',fTarget:7:5,']');

    pOutput := TNNetVolume.Create(NumClasses, 1, 1);
    vOutput := TNNetVolume.Create(NumClasses, 1, 1);

    CurrentLearningRate := fLearningRate;
    NN.SetLearningRate(CurrentLearningRate, fInertia);

    //--------------------------------------------------------------------
    // creates required volumes to store images
    for I := 0 to 49999 do
    begin
      Volume := TNNetVolume.Create();
      ImgVolumes.Add(Volume);
    end;

    for I := 0 to 9999 do
    begin
      Volume := TNNetVolume.Create();
      ImgTestVolumes.Add(Volume);
    end;
    //--------------------------------------------------------------------

    loadCifar10Dataset(ImgTestVolumes, 'test_batch.bin', 0);
    loadCifar10Dataset(ImgVolumes, 1, 0, 0);
    loadCifar10Dataset(ImgVolumes, 2, 10000, 0);
    loadCifar10Dataset(ImgVolumes, 3, 20000, 0);
    loadCifar10Dataset(ImgVolumes, 4, 30000, 0);
    loadCifar10Dataset(ImgVolumes, 5, 40000, 0);

    WriteLn('Neural Network will minimize error with:');
    WriteLn(' Layers: ', NN.CountLayers());
    WriteLn(' Neurons:', NN.CountNeurons());
    WriteLn(' Weights:' ,NN.CountWeights());
    NN.DebugWeights();
    NN.DebugStructure();
    writeln('Computing...');
    globalStartTime := Now();
    globalLoopCnt := 0;
    while (CurrentRate < fTarget) do
    begin
      hit := 0;
      miss := 0;
      ErrorSum := 0;
      LastError := 0;
      startTime := Now();
      for I := 0 to ImgVolumes.Count - 1 (* Min(K*10,ImgVolumes.Count - 1) *) do
      begin
        //-- CAREFUL
        imgIdx := Random(ImgVolumes.Count);

        ImgInput.Copy(ImgVolumes[ImgIdx]);

        // Data Augmentation
        CropSizeX := random(9);
        CropSizeY := random(9);

        ImgInputCp.CopyCropping(ImgVolumes[ImgIdx], random(CropSizeX), random(CropSizeY),ImgVolumes[ImgIdx].SizeX-CropSizeX, ImgVolumes[ImgIdx].SizeY-CropSizeY);
        ImgInput.CopyResizing(ImgInputCp, ImgVolumes[ImgIdx].SizeX, ImgVolumes[ImgIdx].SizeY);

        if Random(1000) > 500 then
        begin
          ImgInput.FlipX();
        end;

        if (Random(1000) > 750) then
        begin
          ImgInput.MakeGray(0);
        end;

        //ImgInput.ShiftRight( Random(128) * 3 ); //Shifts right and down
        //ImgInput.Add( (Random(10)-5)/256 );

        ImgInput.Tag := ImgVolumes[ImgIdx].Tag;
        NN.Compute(ImgInput);
        NN.GetOutput(pOutput);
        case iAlgo of
          9,10,11: vOutput.SetClassForHiperbolicTangent(ImgInput.Tag);
          1,2,3,4,5,6,7,8,12,13: vOutput.SetClassForSoftMax(ImgInput.Tag);
        end;
        NN.Backpropagate(vOutput);
        ErrorSum += vOutput.SumDiff(pOutput);
        //-- CAREFUL

        if pOutput.GetClass() = ImgInput.Tag
        then Inc(Hit)
        else Inc(Miss);

        if (I > 0) then CurrentRate := Hit / (Hit + Miss);

        if (Hit > 0) and (I > 0) and (I mod csLogEvery = 0) then
        begin
          totalTimeSeconds := (Now() - startTime) * 24 * 60 * 60;
          WriteLn
          (
            globalLoopCnt*ImgVolumes.Count + I, ' Examples seen. Training accuracy:', Hit / (Hit + Miss): 6: 4,
            ' Partial error:', (ErrorSum - LastError): 10: 5,
            ' Times - Step:', totalTimeSeconds: 5: 2, 's',
            ' Forward:', (NN.ForwardTime * 24 * 60 * 60):5:2,'s',
            ' Backward:', (NN.BackwardTime * 24 * 60 * 60):5:2,'s'
          );
          NN.ClearTime();
          startTime := Now();
          CVSError := (ErrorSum-LastError);
          LastError := ErrorSum;
        end;
      end;

      Inc(globalLoopCnt);
      if (Hit > 0) then
      begin
        WriteLn
        (
          '---- ', globalLoopCnt * ImgVolumes.Count,
          ' Examples seen. Training accuracy:', CurrentRate : 6: 4,
          ' Error: ', ErrorSum: 10: 5,
          ' Total time: ', (((Now() - globalStartTime)) * 24 * 60): 6: 2, 'min'
        );
        NN.DebugWeights();

        CVSLoss  := (0/(Hit+Miss));
        CVSRate  := CurrentRate;

        if (fLearningRate * (1 - CurrentRate)) < CurrentLearningRate then
        begin
          CurrentLearningRate := fLearningRate * (1 - CurrentRate);
          NN.SetLearningRate(CurrentLearningRate, fInertia);
          WriteLn('Learning rate set to: [',CurrentLearningRate:7:5,']');
        end
        else
        begin
          WriteLn('Current learning rate is: [',CurrentLearningRate:7:5,']');
        end;

        NN.EnableDropouts(false);
        TestBatch(NN, ImgTestVolumes, 0, CVSTestRate, CVSTestLoss, CVSTestError);
        NN.EnableDropouts(true);

        WriteLn
        (
          ' Training Accuracy:', CVSRate:6:4,' Test Accuracy:', CVSTestRate:6:4,
          ' Training Error:', CVSError:10:5,' Test Error:', CVSTestError:10:5,
          ' Training Loss: ', CVSLoss:7:5,' Test Loss:', CVSTestLoss:7:5,
          ' Total time: ', (((Now() - globalStartTime)) * 24 * 60): 6: 2, 'min'
        );

        if globalLoopCnt mod 5 = 0 then
        begin
          WriteLn(' Saving neural network to file:' + FileName);
          NN.SaveToFile(FileName);

          writeln
          (
            CSVFile,
            CVSRate:6:4,',',
            CVSLoss:7:5,',',
            CVSError:10:5,',',
            CVSTestRate:6:4,',',
            CVSTestLoss:7:5,',',
            CVSTestError:10:5
          );

          CloseFile(CSVFile);
          AssignFile(CSVFile, FileNameCSV);
          Append(CSVFile);
        end;
      end;

      NN.ClearTime();
    end;

    WriteLn
    (
      'FINISHED ', I,
      ' Training accuracy:', CurrentRate : 6: 4,
      ' Error: ', ErrorSum: 10: 5,
      ' Total Time: ', (((Now() - globalStartTime)) * 24): 6: 2, ' hours.'
    );

    CloseFile(CSVFile);
    ImgInputCp.Free;
    ImgInput.Free;
    NN.Free;
    vOutput.Free;
    pOutput.Free;
    ImgTestVolumes.Free;
    ImgVolumes.Free;
  end;

  constructor TTestCNNAlgo.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TTestCNNAlgo.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TTestCNNAlgo.WriteHelp;
  begin
    WriteLn
    (
      'CIFAR-10 Classification Example by Joao Paulo Schwarz Schuler',sLineBreak,
      'Command Line Example: cifar10 -a 1', sLineBreak,
      ' -h : displays this help. ', sLineBreak,
      ' -l : defines learing rate. Default is -l 0.01. ', sLineBreak,
      ' -i : defines inertia. Default is -i 0.9.', sLineBreak,
      ' -a : defines the algorithm or neural network structure. Follows examples:', sLineBreak,
      ' -a 1 means:', sLineBreak,
      '   NN.AddLayer(TNNetInput.Create(32, 32, 3)); ', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0)); ', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2)); ', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(128, 5, 0, 0)); ', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2)); ', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(128, 5, 0, 0)); ', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(64)); ', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(NumClasses)); ', sLineBreak,
      ' -a 2 means:', sLineBreak,
      '   NN.AddLayer(TNNetInput.Create(32, 32, 3));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(64, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(64));', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(NumClasses));', sLineBreak,
      ' -a 3 means:', sLineBreak,
      '   NN.AddLayer(TNNetInput.Create(32, 32, 3));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(16, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetMaxPool.Create(2));', sLineBreak,
      '   NN.AddLayer(TNNetConvolutionReLU.Create(32, 5, 0, 0));', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(32));', sLineBreak,
      '   NN.AddLayer(TNNetLayerFullConnectReLU.Create(NumClasses));', sLineBreak,
      ' Algorithms 1,2 and 3 use ReLU. Algorithms ',
      ' 9,10 and 11 use hiperbolic tangents.',sLineBreak,
      ' ReLU implementations are a lot faster to run.',sLineBreak,
      ' If you are unsure about what algorithm to run, try typing "cifar10 -a 1"',sLineBreak,
      ' You can find other algoriths looking at testcnnalgo.lpr source code:',sLineBreak,
      ' https://sourceforge.net/p/cai/svncode/HEAD/tree/trunk/lazarus/testcnnalgo/testcnnalgo.lpr',sLineBreak,
      ' More info at:',sLineBreak,
      '   https://sourceforge.net/projects/cai/'
    );
  end;

var
  Application: TTestCNNAlgo;
begin
  Application := TTestCNNAlgo.Create(nil);
  Application.Title:='CIFAR-10 Classification Example';
  Application.Run;
  Application.Free;
end.
