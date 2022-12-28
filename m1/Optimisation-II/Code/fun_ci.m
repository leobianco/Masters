function val = fun_ci (x,y)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_ci :
  % Renvoie les valeurs des contraintes d'inégalités sur la longueur des barres en xy
  %
  % SYNOPSIS val = fun_ci (x,y)
  %          
  % INPUT * x,y : coordonnées des points
  %
  % OUTPUT - val: vecteur-colonne de taille [p * Nn,1]
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nb; global Nn; global L; global p; global R, global S;
  
  rep = [];
  unite = ones(Nn,1);
  
  % Calcul de c_i
  for j=1:p
    rep = [rep; R(j).*unite + S(j).*x(2:Nb)-y(2:Nb)];
  endfor
  val = rep;
endfunction