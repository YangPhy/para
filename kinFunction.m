(* ::Package:: *)

Print[Style["kinFunction: ", "Text", Bold],
 Style["A Mathematica package for the kinematic functions at colliders ",
  "Text"]]
Print["by Yang Ma (October 2021)"];
Print["If you use para in your research, please cite"];
Print [Style[" \[Bullet] T. Han, A. K. Leibovich, Y. Ma and X.-Z. Tan, JHEP 08 (2022) 073, arXiv:2202.08273.","Text"]];

(* Functions to get the 4 - momentum in a two - body system *)
E1[m1_, m2_, s_] := (m1^2 - m2^2 + s)/(2 Sqrt[s])
Lmd[x_, y_, z_] := (x - y - z)^2 - 4 y z
(*beta[s_,m1_,m2_]:=Sqrt[Lmd[1,m1^2/s,m2^2/s]]*)
beta[s_, m1_, m2_] := Sqrt[m1^4 + (m2^2 - s)^2 - 2 m1^2 (m2^2 + s)]/s
(*P1[s_,m1_,m2_]:=beta[s,m1,m2] Sqrt[s]/2*)
P1[s_, m1_, m2_] :=
 1/2 Sqrt[(m1^4 + (m2^2 - s)^2 - 2 m1^2 (m2^2 + s))/s]
cos[x1_] := 2 x1 - 1;
sin[x1_] := 2 Sqrt[x1 (1 - x1)] (* sin\[Theta]=Sqrt[1-cos^2\[Theta]] *)
phi[x2_] := 2 \[Pi] x2;
Momentum1[s_, m1_, m2_, x1_, x2_] := {E1[m1, m2, s],
  P1[s, m1, m2] sin[x1] Cos[phi[x2]], P1[s, m1, m2] sin[x1] Sin[phi[x2]],
  P1[s, m1, m2] cos[x1]}
getMomentum1[s_, m1_, m2_, x1x2_] :=
 Momentum1[s, m1, m2, #1, #2] & @@ x1x2
getMomentum2[s_, m1_, m2_, p1_] := {E1[m2, m1, s], -p1[[2 ;;]]} //
  Flatten

(* Link 2 two-body systems*)
M2Low[m2_, m3_] := (m2 + m3)^2;
M2High[sqrts_, m1_] := (sqrts - m1)^2;
M2[x_, M2min_, M2max_] := M2min + x (M2max - M2min);
getM2[sqrts_, m1_, m2_, m3_, x3_] :=
 M2[x3, M2Low[m2, m3], M2High[sqrts, m1]]
jacobianM2[sqrts_, m1_, m2_, m3_] := (
 M2High[sqrts, m1] - M2Low[m2, m3])/(2 \[Pi])

(* Functions to deal with vectors *)
Clear[Dot4];
getP3[P4_] := P4[[2 ;;]]
Dot4[x_ /; Length[x] == 4, y_ /; Length[y] == 4] :=
  x[[1]] y[[1]] - Sum[x[[i]] y[[i]], {i, 2, 4}];
Eps4[p1_ /; Length[p1] == 4, p2_ /; Length[p2] == 4,
   p3_ /; Length[p3] == 4,
   p4_ /; Length[p4] == 4] := -Det[{p1, p2, p3, p4} // Transpose];
ProjectToxyz[p4_, xhat_, yhat_, zhat_] := 
 Module[{p3 = getP3[p4], p0 = p4[[1]]}, {p0, p3 . xhat, p3 . yhat, 
   p3 . zhat}]

(* Functions to define physical observables *)
getP4reverse[P4_] := {{P4[[1]]}, -P4[[2 ;; All]]} // Flatten
getBeta[P4_] := getP3[P4]/P4[[1]]
getPT[P4_] := Sqrt[P4[[2]]^2 + P4[[3]]^2];
getTheta[P4_] :=
 Module[{p = P4, temp1, temp2},
  temp1 = Max[-1, p[[4]]/Sqrt[Sum[p[[i]]^2, {i, 2, 4}]]];
  temp2 = Min[1, temp1]; ArcCos[temp2]]
getEta[P4_] := -Log[Tan[getTheta[P4]/2]];
getPhi[p_] := 
 Module[{p1 = p[[2]], p2 = p[[3]], phi}, 
  phi = Piecewise[{{ArcTan[p2/p1], p1 > 0}, {ArcTan[p2/p1] + \[Pi], 
      p1 < 0}, {\[Pi]/2, p2 >= 0}, {-(\[Pi]/2), p2 < 0}}]; 
  Piecewise[{{phi + 2 \[Pi], phi < 0}}, phi]]


getDeltaPhi[P1_, P2_] :=
  Module[{p1 = P1, p2 = P2, Denom, temp1, temp2},
   Denom = getPT[p1] getPT[p2];
   temp1 = Max[-1, 1/Denom (P1[[2]] P2[[2]] + P1[[3]] P2[[3]])];
   temp2 = Min[1, temp1]; ArcCos[temp2]];
getDeltaR[P1_, P2_] := Sqrt[
  getDeltaPhi[P1, P2]^2 + (getEta[P1] - getEta[P2])^2];

(* Lorentz boost *)
LTBoostMatrixV[vx_, vy_, vz_] :=
 Module[{v = Sqrt[vx^2 + vy^2 + vz^2],
   v2 = vx^2 + vy^2 + vz^2, \[Gamma], \[Gamma]2}, \[Gamma] = 1/Sqrt[
   1 - v2]; \[Gamma]2 =
   Piecewise[{{(\[Gamma] - 1)/v2, v2 != 0}, {0,
      v2 == 0}}]; {{\[Gamma], vx \[Gamma], vy \[Gamma],
    vz \[Gamma]}, {vx \[Gamma], 1 + vx^2 \[Gamma]2, vx vy \[Gamma]2,
    vx vz \[Gamma]2}, {vy \[Gamma], vx vy \[Gamma]2,
    1 + vy^2 \[Gamma]2, vy vz \[Gamma]2}, {vz \[Gamma],
    vx vz \[Gamma]2, vy vz \[Gamma]2, 1 + vz^2 \[Gamma]2}}]
LTBoostMatrixP[P4_] := LTBoostMatrixV @@ getBeta[P4]
LTBoostP[P4_, P_] := LTBoostMatrixP[P4] . P
LTBoost
