function [e, c, g, a, hl, indic_sortie] = chs(indic, xy, lm)
  
  %%%%%%%%%%%%%%%%%%%%%%%% VARIABLES GLOBALES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  global A; global B; global Nb; global N; global Nn; global L;
  
  %%%%%%%%%%%%%%%%%%%%%%%% VERIFICATIONS PARAMETRES %%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  indic_sortie = verif_param(indic, xy, lm);
  
  % Si les paramètres ne sont pas corrects, on arrête la fonction
  if indic_sortie == 1
    e=0;c=0;g=0;a=0;hl=0;
    return
  endif
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  % Coordonnées x et y de tous les points de la chaîne
  % Il y a un décalage +1 pour ces tableaux : x(j+1) = x_j et y(j+1) = y_j
  x = [0;xy(1:Nn);A]; 
  y = [0;xy(Nn + 1: 2 * Nn);B];
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SIMULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  switch indic
    case 1  
      %représentation de la chaîne
      fun_trace(x,y);
      
      %variables par défaut
      e=0;c=0;g=0;a=0;hl=0;
      
    case 2
      e = fun_e(x,y);
      c = fun_c(x,y);
      
      %variables par défaut
      g=0;a=0;hl=0;
      
    case 4
      e = fun_e(x,y);
      c = fun_c(x,y);
      g = fun_g(x,y);
      a = fun_a(x,y);
      
      %variable par défaut
      hl=0;
      
    case 5
      hl = fun_hl(x,y,lm);
      
      %variables par défaut
      e=0;c=0;g=0;a=0;
      
    case 6
      test_derivees(xy,x,y);
      
      %variables par défaut
      e=0;c=0;g=0;a=0;hl=0;
      
   endswitch

endfunction
