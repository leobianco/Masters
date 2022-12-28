function val = sigma(x,y, ref)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sigma :
% Renvoie vers les fonctions sigma_1 ou sigma_2 selon la reference ref, selon que le sommet (x,y)
% est dans le domaine 1 ou 2
%
% SYNOPSIS val = sigma(x,y, ref)
%          
% INPUT * x,y,ref : les 2 coordonnees du sommet et ref sa reference
%
% OUTPUT - val: valeur de la fonction sigma sur ce sommet.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ref==1
  val = sigma_1(x, y);
else
  val = sigma_2(x, y);
endif

endfunction