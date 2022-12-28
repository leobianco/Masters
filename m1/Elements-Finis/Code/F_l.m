function coor_T_l = F_l(x_elem, y_elem, B_l, S_l)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F_l :
% Evaluation de la fonction de passage du triangle de référence
% au triangle quelconque
%
% SYNOPSIS coor_T_l = F_l(x_elem, y_elem, B_l, S_l)
%          
% INPUT * x_elem, y_elem : les 2 coordonnees d'un sommet du triangle de référence.
%       * B_l, S_l : pour calculer la fonction F_l
%
% OUTPUT - val: coordonnées du sommet correspondant du triangle quelconque
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coor_T_l = B_l*[x_elem; y_elem] + S_l;

endfunction