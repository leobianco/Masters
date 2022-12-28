function val = fun_hl (x,y,lm)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_hl :
  % Renvoie la hessienne du lagrangien en xy et lm
  %
  % SYNOPSIS val = fun_hl (x,y,lm)
  %          
  % INPUT * x,y : coordonnées des points
  %          lm : multiplicateurs de Lagrange
  %
  % OUTPUT - val: matrice de taille [2Nb,2Nb]
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variable globale
  global Nb;
  
  diag = 2.*[lm(1:Nb-1,1)+lm(2:Nb,1);lm(1:Nb-1,1)+lm(2:Nb,1)];  % diagonale
  diag_inf = -2.*[[lm(2:Nb-1,1)];0;[lm(2:Nb-1,1);0]];  % sous-diagonale
  diag_sup = -2.*[0;lm(2:Nb-1,1);0;lm(2:Nb-1,1)];  % sur-diagonale
  
  val = spdiags([diag_inf diag diag_sup],[-1,0,1],2*Nb-2,2*Nb-2);

endfunction
