function val = fun_terminaison (nb_iter,maxit)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_terminaison :
  % Renvoie l'entier correspondant à la terminaison de l'algorithme :
  %   - 0 si le seuil d'optimalité est atteint
  %   - 2 si le nombre maximum d'itérations est atteint
  %
  % SYNOPSIS val = fun_terminaison (nb_iter,maxit)
  %          
  % INPUT * nb_iter : nombre d'itérations effectuées
  %         maxit : nombre maximal d'itérations autorisées
  %
  % OUTPUT - val: scalaire
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if nb_iter < maxit
    val = 0;
  else
    val = 2;
  endif
    
    
endfunction