function val = fun_e (x,y)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_e :
  % Renvoie l'énergie en xy
  %
  % SYNOPSIS val = fun_e (x,y)
  %          
  % INPUT * x,y : coordonnées des points
  %               Les coordonnées x ne sont pas nécessaires.
  %
  % OUTPUT - val: scalaire
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nb; global L;
  
  % Calcul de e
  val = sum(L .* (y(2 : Nb+1) + y(1 : Nb))./2);
endfunction
