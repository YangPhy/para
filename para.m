Print[Style["para: ", "Text", Bold], 
 Style["A Mathematica package for inputting Standard Model parameters ", 
  "Text"]]
Print["by Yang Ma (March 2020)"];
Print["If you use para in your research, please cite"];
Print [Style[" \[Bullet] T. Han, Y. Ma, and K. Xie, Phys.Rev.D 103 (2021) 3, L031301, arXiv:2007.14300.","Text"]];
Print [Style[" \[Bullet] T. Han, A. K. Leibovich, Y. Ma and X.-Z. Tan, JHEP 08 (2022) 073, arXiv:2202.08273.","Text"]];

(* plot settings *)
SetOptions[{Plot, LogPlot, LogLogPlot, LogLinearPlot,ContourPlot,ListContourPlot,Histogram}, 
  AspectRatio -> 0.6, Frame -> True, ImageSize -> 600,
  LabelStyle -> {FontSize -> 22, FontFamily -> "Times", Black}, 
  FrameStyle -> Directive[Black, 22]];
SetOptions[{ListPlot, ListStepPlot, ListLogPlot, ListLogLogPlot, ListLogLinearPlot},
   AspectRatio -> 0.6, Frame -> True, ImageSize -> 600, 
  Joined -> True,
  LabelStyle -> {FontSize -> 22, FontFamily -> "Times", Black}, 
  FrameStyle -> Directive[Black, 22]];
RGB[r_, g_, b_] := RGBColor[r/255, g/255, b/255];
CMYK[c_, m_, y_, k_] := CMYKColor[c/255, m/255, y/255, k/255];
hexToRGB[hex_] := 
 RGBColor @@ (N[FromDigits[StringJoin[#], 12]/255] & /@ 
    Partition[Characters[hex], 2])
{c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11} = {RGB[228, 26, 28], 
   RGB[77, 175, 74], RGB[55, 126, 184], RGB[255, 127, 0], 
   RGB[152, 78, 163], Black, Blue, RGB[41, 143, 255], 
   RGB[128, 0, 128], RGB[166,86,40], Magenta // Lighter};

(* tab manipulations *)
tabRatio[tab1_, tab2_] := 
  Table[{tab1[[n, 1]], 
    If[tab2[[n, 2]] == 0, 0, tab1[[n, 2]]/tab2[[n, 2]]]}, {n, 1, 
    tab1 // Length}];
tabSum2[tab1_, tab2_] := 
  Table[{tab1[[n, 1]], tab1[[n, 2]] + tab2[[n, 2]]}, {n, 1, 
    tab1 // Length}];
tabDiff[tab1_, tab2_] := 
  Table[{tab1[[n, 1]], tab1[[n, 2]] - tab2[[n, 2]]}, {n, 1, 
    tab1 // Length}];
tabSum3[tab1_, tab2_, tab3_] := 
  Table[{tab1[[n, 1]], 
    tab1[[n, 2]] + tab2[[n, 2]] + tab3[[n, 2]]}, {n, 1, 
    tab1 // Length}];
tabAvg2[tab1_, tab2_] := 
  Table[{tab1[[n, 1]], (tab1[[n, 2]] + tab2[[n, 2]])/2}, {n, 1, 
    tab1 // Length}];
tabHalf[tab_] := 
  Table[{tab[[n, 1]], tab[[n, 2]]/2}, {n, 1, tab // Length}];
tabSquare[tab_] := 
  Table[{tab[[n, 1]], tab[[n, 2]]^2}, {n, 1, tab // Length}];
GeV2TeV[tab_] := 
  Table[{tab[[n, 1]]/1000, tab[[n, 2]]}, {n, 1, tab // Length}];
pb2fb[tab_] := 
  Table[{tab[[n, 1]], 1000 tab[[n, 2]]}, {n, 1, tab // Length}];
fb2pb[tab_] := 
  Table[{tab[[n, 1]], tab[[n, 2]]/1000}, {n, 1, tab // Length}];

(* SM parameters *)
SMpara1 = {CW -> MW/MZ, SW -> Sqrt[1 - MW^2/MZ^2], CA -> 3, vev->1/ Sqrt[Sqrt[2]*GF]};
SMpara2 = {EL -> Sqrt[4 \[Pi] \[Alpha]E] , 
   MW -> Sqrt[
    MZ^2/2 + Sqrt[MZ^4/4 - (\[Alpha]E \[Pi] MZ^2)/(GF Sqrt[2])]]};
SMpara = {EL -> Sqrt[4 \[Pi] \[Alpha]E] , g -> EL/SW, CW -> MW/MZ, 
   SW -> Sqrt[1 - MW^2/MZ^2], 
   MW -> Sqrt[
    MZ^2/2 + Sqrt[MZ^4/4 - (\[Alpha]E \[Pi] MZ^2)/(GF Sqrt[2])]]};
SMnum = N[{\[Alpha]E -> 2/265, GF -> 116639 10^-10, Mt -> 172,MB->47/10,MC->3/2,
     MA -> 0, MZ -> 91188 10^-3, MH -> 125, MTA->1777/1000, MM -> 10566 10^-5,
    ME -> 511 10^-6, GammaZ -> 24414 10^-4, GammaW -> 20476 10^-4,GammaT-> 150834 10^-5,GammaH-> 575309 10^-8,Vcb-> 41 10^-3}, 50];
SMinput = 
  Join[{g -> EL/SW} /. SMpara1 /. SMpara2 /. SMnum, 
   SMpara1 /. SMpara2 /. SMnum, SMpara2 /. SMnum, SMnum];
SMinput2=SMinput/. {CW -> cw, SW -> sw, MW -> mw, MB -> mb, MC -> mc, MA -> ma, 
  MZ -> mz, MH -> mh, MM -> mm, ME -> me}
FormCalcSM = {MC2 -> mc^2, Alfa -> \[Alpha]E, Alfa2 -> \[Alpha]E^2, 
  MZ2 -> mz^2,MW2-> mw^2,SW2 -> sw^2,CW2 -> cw^2}

(* QCD parameters *)
QCDpara = {CF -> (3^2 - 1)/(2 3), CA -> 3};
\[Alpha]s0[mu_, nf_] := 
 Module[{\[CapitalLambda]QCD, t, b0, b01}, \[CapitalLambda]QCD = 
   Piecewise[{{1467/10000, nf == 3}, {1218/10000, 
      nf == 4}, {883/10000, nf == 5}, {429/10000, nf == 6}}]; 
  t = 2 Log[mu/\[CapitalLambda]QCD]; b0 = 11 - 2/3 nf; 
  b01 = b0/(4 Pi); 1/(b01 t)]

\[Alpha]s0[mu_] := 
 Module[{MC, MB, Mt}, MC = 3/2; MB = 49/10; 
  Mt = 175; (N[#1, 50] &)[
   Piecewise[{{\[Alpha]s0[mu, 3], 
      mu >= 1 && mu < MC}, {\[Alpha]s0[mu, 4], 
      mu >= MC && mu < MB}, {\[Alpha]s0[mu, 5], 
      mu >= MB && mu < Mt}, {\[Alpha]s0[mu, 6], 
      mu >= Mt}, {Print[
       "Warning: it is dangerous to use the value of \[Alpha]s(Q) \
when Q < 1 GeV"], mu < 1}}]]]

\[Alpha]s1[mu_, nf_] := 
 Module[{\[CapitalLambda]QCD, t, b0, b1, b01, 
   b11}, \[CapitalLambda]QCD = 
   Piecewise[{{3843/10000, nf == 3}, {3306/10000, 
      nf == 4}, {2275/10000, nf == 5}, {920/10000, nf == 6}}]; 
  t = 2 Log[mu/\[CapitalLambda]QCD]; b0 = 11 - 2/3 nf; 
  b1 = 102 - (38 nf)/3; b01 = b0/(4 Pi); b11 = b1/(4 Pi)^2; 
  1/(b01 t) (1 - b11/b01^2 Log[t]/t)]

\[Alpha]s1[mu_] := 
 Module[{MC, MB, Mt}, MC = 3/2; MB = 49/10; 
  Mt = 175; (N[#1, 50] &)[
   Piecewise[{{\[Alpha]s1[mu, 3], 
      mu >= 1 && mu < MC}, {\[Alpha]s1[mu, 4], 
      mu >= MC && mu < MB}, {\[Alpha]s1[mu, 5], 
      mu >= MB && mu < Mt}, {\[Alpha]s1[mu, 6], 
      mu >= Mt}, {Print[
       "Warning: it is dangerous to use the value of \[Alpha]s(Q) \
when Q < 1 GeV"], mu < 1}}]]]
  
\[Alpha]s[mu_] := 
 Module[{mc = 1.5, mb = 4.9, mt = 175, 
   mZ = 91.1876, \[Alpha]sZ = 0.1181, nf, NC, CA, TF, 
   CF, \[Alpha]sT, \[Alpha]sB, \[Alpha]sC, \[Beta]0, \[Beta]1, b0, 
   b1}, NC = 3; CA = NC; TF = 1/2; 
  CF = (NC^2 - 1)/(2 NC); \[Beta]0 = ( 11 CA - 4 TF nf)/
   3; \[Beta]1 = (34 CA^2/3  - 20 (CA TF nf)/3 - 4 CF TF nf); 
  b0 = \[Beta]0/(2 \[Pi]); 
  b1 = \[Beta]1/(
   8 \[Pi]^2); \[Alpha]sT = (1/\[Alpha]sZ + b0 Log[mt/mZ] + 
      b1/b0 Log[1 + (b0 Log[mt/mZ])/(1/\[Alpha]sZ + b1/b0)])^-1 /. 
    nf -> 5; \[Alpha]sB = (1/\[Alpha]sZ + b0 Log[mb/mZ] + 
      b1/b0 Log[1 + (b0 Log[mb/mZ])/(1/\[Alpha]sZ + b1/b0)])^-1 /. 
    nf -> 5; \[Alpha]sC =  (1/\[Alpha]sB + b0 Log[mc/mb] + 
      b1/b0 Log[1 + (b0 Log[mc/mb])/(1/\[Alpha]sB + b1/b0)])^-1 /. 
    nf -> 4; 
  Piecewise[{{(1/\[Alpha]sT  + b0 Log[mu/mt] + 
        b1/b0 Log[1 + (b0 Log[mu/mt])/(1/\[Alpha]sT + b1/b0)])^-1 /. 
      nf -> 6, 
     mu >= mt}, {(1/\[Alpha]sZ + b0 Log[mu/mZ] + 
        b1/b0 Log[1 + (b0 Log[mu/mZ])/(1/\[Alpha]sZ + b1/b0)])^-1 /. 
      nf -> 5, 
     mb <= mu < 
      mt}, {( 1/\[Alpha]sB + b0 Log[mu/mb] + 
        b1/b0 Log[1 + (b0 Log[mu/mb])/(1/\[Alpha]sB + b1/b0)])^-1 /. 
      nf -> 4, 
     mc <= mu < 
      mb}, { (1/\[Alpha]sC + b0 Log[mu/mc] + 
        b1/b0 Log[1 + (b0 Log[mu/mc])/(1/\[Alpha]sB + b1/b0)])^-1 /. 
      nf -> 3, mu < mc}}]]

(* Gauge coupling running *)      
gISM2[t_, i_] := 
 Module[{MZ, \[Alpha]Z, \[Alpha]sZ, sW2Z, g12Z, g22Z, g32Z, gi2Z, b0},
   MZ = 911876/10000; \[Alpha]Z = 100/12891; \[Alpha]sZ = 1181/10000;
  sW2Z = 23121/100000; g12Z = 5/3* 4 \[Pi] \[Alpha]Z/(1 - sW2Z);
  g22Z = 4 \[Pi] \[Alpha]Z/sW2Z; g32Z = 4 \[Pi] \[Alpha]sZ;
  gi2Z = {g12Z, g22Z, g32Z}; b0 = {41/10, -19/6, -7};
  gi2Z[[i]]/(1 + gi2Z[[i]] b0[[i]]/(8 \[Pi]^2) (Log[MZ] - t))]
gI2HDM2[t_, i_] := 
 Module[{MZ, \[Alpha]Z, \[Alpha]sZ, sW2Z, g12Z, g22Z, g32Z, gi2Z, b0},
   MZ = 911876/10000; \[Alpha]Z = 100/12891; \[Alpha]sZ = 1181/10000;
  sW2Z = 23121/100000; g12Z = 5/3* 4 \[Pi] \[Alpha]Z/(1 - sW2Z);
  g22Z = 4 \[Pi] \[Alpha]Z/sW2Z; g32Z = 4 \[Pi] \[Alpha]sZ;
  gi2Z = {g12Z, g22Z, g32Z}; b0 = {21/5, -3, -7};
  gi2Z[[i]]/(1 + gi2Z[[i]] b0[[i]]/(8 \[Pi]^2) (Log[MZ] - t))]
gIMSSM2MZ[t_, i_] := 
 Module[{MZ, \[Alpha]Z, \[Alpha]sZ, sW2Z, g12Z, g22Z, g32Z, gi2Z, b0},
   MZ = 911876/10000; \[Alpha]Z = 100/12891; \[Alpha]sZ = 1181/10000;
  sW2Z = 23121/100000; g12Z = 4 \[Pi] \[Alpha]Z/(1 - sW2Z);
  g22Z = 4 \[Pi] \[Alpha]Z/sW2Z; g32Z = 4 \[Pi] \[Alpha]sZ;
  gi2Z = {g12Z, g22Z, g32Z}; b0 = {33/5, 1, -3};
  gi2Z[[i]]/(1 + gi2Z[[i]] b0[[i]]/(8 \[Pi]^2) (Log[MZ] - t))]
gIMSSM2[t_, i_, \[Mu]0_: 500] := 
 Module[{MZ, t0 = Log[\[Mu]0], gi20, b0}, 
  gi20 = {gISM2[t0, 1], gISM2[t0, 2], gISM2[t0, 3]};
  b0 = {33/5, 1, -3}; 
  Piecewise[{{gi20[[i]]/(1 + gi20[[i]] b0[[i]]/(8 \[Pi]^2) (t0 - t)), 
     t > t0}}, gISM2[t, i]]]

gISM[q_, i_] := Sqrt[gISM2[Log[q], i]]
gI2HDM[q_, i_] := Sqrt[gI2HDM2[Log[q], i]]
gIMSSM[q_, i_, \[Mu]0_: 500] := Sqrt[gIMSSM2[Log[q], i, \[Mu]0]]

(* Running mass for charm quark and bottom quark *)
LOmcMS4[mu_] := 
  Module[{mc3GeV, 
    nf, \[Beta]0, \[Beta]1, \[Beta]2, \[Gamma]0, \[Gamma]1, \[Gamma]2,
     c, c0, c1, c2, b1, b2, x, x0, cx, cx0}, nf = 4; 
   mc3GeV = 1012/1000; \[Beta]0 = 1/4 (11 - (2 nf)/3); \[Beta]1 = 
    1/16 (102 - (38 nf)/3); \[Beta]2 = 
    1/64 (2857/2 - (5033 nf)/18 + 325/54 nf^2); 
   b1 = \[Beta]1/\[Beta]0; 
   b2 = \[Beta]2/\[Beta]0; \[Gamma]0 = 1; \[Gamma]1 = 
    1/16 (202/3 - (20 nf)/9); \[Gamma]2 = 
    1/64 (1249 + (-2216/27 - 160/3 Zeta[3]) nf - (140 nf^2)/81); 
   c0 = \[Gamma]0/\[Beta]0; c1 = \[Gamma]1/\[Beta]0; 
   c2 = \[Gamma]2/\[Beta]0; x0 = \[Alpha]s0[3, 4]/\[Pi]; 
   x = \[Alpha]s0[mu, 4]/\[Pi]; cx0 = x0^c0 ; cx = x^c0 ; 
   mc3GeV cx/cx0];
LOmcMS5[mu_] := 
  Module[{mcMB, MB, 
    nf, \[Beta]0, \[Beta]1, \[Beta]2, \[Gamma]0, \[Gamma]1, \[Gamma]2,
     c, c0, c1, c2, b1, b2, x, x0, cx, cx0}, nf = 5; MB = 49/10; 
   mcMB = LOmcMS4[MB]; \[Beta]0 = 1/4 (11 - (2 nf)/3); \[Beta]1 = 
    1/16 (102 - (38 nf)/3); \[Beta]2 = 
    1/64 (2857/2 - (5033 nf)/18 + 325/54 nf^2); 
   b1 = \[Beta]1/\[Beta]0; 
   b2 = \[Beta]2/\[Beta]0; \[Gamma]0 = 1; \[Gamma]1 = 
    1/16 (202/3 - (20 nf)/9); \[Gamma]2 = 
    1/64 (1249 + (-2216/27 - 160/3 Zeta[3]) nf - (140 nf^2)/81); 
   c0 = \[Gamma]0/\[Beta]0; c1 = \[Gamma]1/\[Beta]0; 
   c2 = \[Gamma]2/\[Beta]0; x0 = \[Alpha]s0[MB, 5]/\[Pi]; 
   x = \[Alpha]s0[mu, 5]/\[Pi]; cx0 = x0^c0 ; cx = x^c0 ; mcMB cx/cx0];
LOmcMS6[mu_] := 
  Module[{mcMt, Mt, 
    nf, \[Beta]0, \[Beta]1, \[Beta]2, \[Gamma]0, \[Gamma]1, \[Gamma]2,
     c, c0, c1, c2, b1, b2, x, x0, cx, cx0}, nf = 6; Mt = 175; 
   mcMt = LOmcMS5[Mt]; \[Beta]0 = 1/4 (11 - (2 nf)/3); \[Beta]1 = 
    1/16 (102 - (38 nf)/3); \[Beta]2 = 
    1/64 (2857/2 - (5033 nf)/18 + 325/54 nf^2); 
   b1 = \[Beta]1/\[Beta]0; 
   b2 = \[Beta]2/\[Beta]0; \[Gamma]0 = 1; \[Gamma]1 = 
    1/16 (202/3 - (20 nf)/9); \[Gamma]2 = 
    1/64 (1249 + (-2216/27 - 160/3 Zeta[3]) nf - (140 nf^2)/81); 
   c0 = \[Gamma]0/\[Beta]0; c1 = \[Gamma]1/\[Beta]0; 
   c2 = \[Gamma]2/\[Beta]0; x0 = \[Alpha]s0[Mt, 6]/\[Pi]; 
   x = \[Alpha]s0[mu, 6]/\[Pi]; cx0 = x0^c0 ; cx = x^c0 ; mcMt cx/cx0];
LOmcMS[mu_] := 
  Module[{MC, MB, Mt}, MC = 3/2; MB = 49/10; Mt = 175; 
   Piecewise[{{LOmcMS4[mu], mu >= MC && mu < MB}, {LOmcMS5[mu], 
       mu >= MB && mu < Mt}, {LOmcMS6[mu], 
       mu >= Mt}, {Print[
        "Warning: we do not consider the running mass \
\!\(\*SubscriptBox[\(m\), \(c\)]\)(Q) for Q < 1.5 GeV"], mu < MC}}] //
     N[#, 50] &];
     
LOmbMS5[mu_] := 
  Module[{mb10GeV, MB, 
    nf, \[Beta]0, \[Beta]1, \[Beta]2, \[Gamma]0, \[Gamma]1, \[Gamma]2,
     c, c0, c1, c2, b1, b2, x, x0, cx, cx0}, nf = 5; MB = 49/10; 
   mb10GeV = 3631/1000; \[Beta]0 = 1/4 (11 - (2 nf)/3); \[Beta]1 = 
    1/16 (102 - (38 nf)/3); \[Beta]2 = 
    1/64 (2857/2 - (5033 nf)/18 + 325/54 nf^2); 
   b1 = \[Beta]1/\[Beta]0; 
   b2 = \[Beta]2/\[Beta]0; \[Gamma]0 = 1; \[Gamma]1 = 
    1/16 (202/3 - (20 nf)/9); \[Gamma]2 = 
    1/64 (1249 + (-2216/27 - 160/3 Zeta[3]) nf - (140 nf^2)/81); 
   c0 = \[Gamma]0/\[Beta]0; c1 = \[Gamma]1/\[Beta]0; 
   c2 = \[Gamma]2/\[Beta]0; x0 = \[Alpha]s0[10, 5]/\[Pi]; 
   x = \[Alpha]s0[mu, 5]/\[Pi]; cx0 = x0^c0 ; cx = x^c0 ; 
   mb10GeV cx/cx0];
LOmbMS6[mu_] := 
  Module[{mbMt, Mt, 
    nf, \[Beta]0, \[Beta]1, \[Beta]2, \[Gamma]0, \[Gamma]1, \[Gamma]2,
     c, c0, c1, c2, b1, b2, x, x0, cx, cx0}, nf = 6; Mt = 175; 
   mbMt = LOmbMS5[Mt]; \[Beta]0 = 1/4 (11 - (2 nf)/3); \[Beta]1 = 
    1/16 (102 - (38 nf)/3); \[Beta]2 = 
    1/64 (2857/2 - (5033 nf)/18 + 325/54 nf^2); 
   b1 = \[Beta]1/\[Beta]0; 
   b2 = \[Beta]2/\[Beta]0; \[Gamma]0 = 1; \[Gamma]1 = 
    1/16 (202/3 - (20 nf)/9); \[Gamma]2 = 
    1/64 (1249 + (-2216/27 - 160/3 Zeta[3]) nf - (140 nf^2)/81); 
   c0 = \[Gamma]0/\[Beta]0; c1 = \[Gamma]1/\[Beta]0; 
   c2 = \[Gamma]2/\[Beta]0; x0 = \[Alpha]s0[Mt, 6]/\[Pi]; 
   x = \[Alpha]s0[mu, 6]/\[Pi]; cx0 = x0^c0 ; cx = x^c0 ; mbMt cx/cx0];
LOmbMS[mu_] := 
  Module[{MB, Mt}, MB = 49/10; Mt = 175; 
   Piecewise[{{LOmbMS5[mu], mu >= MB && mu < Mt}, {LOmbMS6[mu], 
       mu >= Mt}, {Print[
        "Warning: we do not consider the running mass \
\!\(\*SubscriptBox[\(m\), \(b\)]\)(Q) for Q < 4.9 GeV"], mu < MB}}] //
     N[#, 50] &];
