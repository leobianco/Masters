function status = verif_param_sqp(x, lme, lmi, tol1, tol2, tol3, maxit, verb, rl, deriv)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % verif_param :
  % Renvoie la valeur de l'indice de sortie :
  %   - 0 si les paramètres sont corrects
  %   - 1 sinon
  %
  % SYNOPSIS status = verif_param_sqp(x, lm, tol1, tol2, maxit)
  %          
  % INPUT * x : coordonnées des points
  %         lme et lmi : multiplicateurs de Lagrange
  %         tol1, tol2, tol3 : tolérances d'optimalité
  %         maxit : nombre maximal d'itérations autorisées
  %         verb : type de tableau dans le cas de Newton avec RL
  %         rl : choix de recherche linéaire ou pas
  %
  % OUTPUT - val: scalaire
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Variables globales
  global Nn; global Nb; global p;
  
  %Vérification des paramètres :
  % - x est un vecteur-colonne de taille [2*Nn,1]
  % - lm est un vecteur-colonne de taille [Nb,1]
  % - tol1 et tol2 sont dans ]0,1[
  % - maxit est un entier strictement positif
  
  if !(verb == 1 || verb == 2) || !(rl == 0 || rl == 1) || !(tol1 > 0 && tol1 < 1) || !(tol2 > 0 && tol2 < 1) || !(tol3 > 0) || !(maxit > 0 && mod(maxit,1)==0) || (!isvector(x) || !isequal(size(x),[2*Nn,1])) || (!isvector(lme) || !isequal(size(lme),[Nb,1])) || !(isequal(size(lmi),[p .* Nn,1]) || isequal(size(lmi),[0, 0])) || !(deriv == 1 || deriv == 2)
    % Les paramètres ne sont pas corrects
    status = 1;
  else
    % Les paramètres sont corrects
    status = 0;
  endif
  
endfunction
