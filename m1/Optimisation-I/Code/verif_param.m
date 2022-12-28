function indic_sortie = verif_param(indic, xy, lm)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % verif_param :
  % Renvoie la valeur de l'indice de sortie :
  %   - 0 si les paramètres sont corrects
  %   - 1 sinon
  %
  % SYNOPSIS indic_sortie = verif_param(indic, xy, lm)
  %          
  % INPUT * indic : indicateur d'entrée pilotant la simulation
  %            xy : coordonnées des points
  %            lm : multiplicateurs de Lagrange
  %
  % OUTPUT - val: scalaire
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nn; global Nb;
  
  %Vérification des paramètres :
  % - indic appartient à [1,2,4,5,6]
  % - xy est un vecteur-colonne de taille [2*Nn,1]
  % - lm est un vecteur-colonne de taille [Nb,1]
  
  if !ismember(indic,[1,2,4,5,6]) || (!isvector(xy) || !isequal(size(xy),[2*Nn,1])) || (!isvector(lm) || !isequal(size(lm),[Nb,1]))
    % Les paramètres ne sont pas corrects
    indic_sortie = 1;
  else
    % Les paramètres sont corrects
    indic_sortie = 0;
  endif
  
endfunction
