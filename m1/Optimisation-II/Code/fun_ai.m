function val = fun_ai (x,y)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_ai :
  % Renvoie la jacobienne des contraintes d'inégalités en xy
  %
  % SYNOPSIS val = fun_ai (x,y)
  %          
  % INPUT * x,y : coordonnées des points
  %
  % OUTPUT - val: matrice de taille [p * Nn,2Nn]
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nb; global Nn; global L; global p; global R, global S;
  
  % La matrice ai est composée de deux sous-matrices colonnes de taille (p * Nn,Nn)
  % que l'on concatène en ligne. 
  
  % La première sous-matrice est composée de p
  % sous-matrices de taille (Nn,Nn). Elles sont diagonales avec le coefficient
  % s_j sur la diagonale de  la j-ème sous-matrices.
  
  % La seconde sous-matrices est composée de p matrices identité multipliées 
  % par -1. Elles sont de tailles (Nn,Nn)
  
  % Ainsi, pour chaque itération j de la boucle, on calcule les deux matrices :
  % - la matrice diagonale s_j
  % - la matrice identité multipliée par -1
  % Puis on les concatène.
  col1 = [];
  col2 = [];
  
  for j=1:p
    col1 = [col1;S(j).*eye(Nn)];
    col2 = [col2;(-1).*eye(Nn)];
  endfor
  
  val = [col1,col2];
  
  
endfunction
