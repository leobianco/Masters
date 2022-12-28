function val = fun_ce (x,y)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_ce :
  % Renvoie les valeurs des contraintes sur la longueurs des barres en xy
  %
  % SYNOPSIS val = fun_ce (x,y)
  %          
  % INPUT * x,y : coordonnées des points
  %
  % OUTPUT - val: vecteur-colonne de taille [Nb,1]
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nb; global L;
  
  % Calcul de c
  val = (x(2 : Nb+1) - x(1 : Nb)).^2 + (y(2 : Nb +1) - y(1 : Nb)).^2 - L.^2;
endfunction
