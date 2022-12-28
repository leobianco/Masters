function [e,ce,ci,g,ae,ai,hl,indic] = chs(indic,xy,lme,lmi)
  
  %%%%%%%%%%%%%%%%%%%%%%%% VARIABLES GLOBALES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  global A; global B; global Nb; global N; global Nn; global L; global p; global R, global S;
  
  %%%%%%%%%%%%%%%%%%%%%%%% VERIFICATIONS PARAMETRES %%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  indic_sortie = verif_param(indic, xy, lme);
  
  % Si les paramètres ne sont pas corrects, on arrête la fonction
  if indic_sortie == 1
    e=0;ce=0;ci=0;g=0;ae=0;ai=0;hl=0;
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
      figs = figure; hold on;
      % Dessin du plancher
      if !(p == 0) 
       % Limites de la figure
       global FIG_XMIN; FIG_XMIN = -2;
       global FIG_XMAX; FIG_XMAX = 2;
       global FIG_YMIN; FIG_YMIN = -2; 
       global FIG_YMAX; FIG_YMAX = 2; 

       floor_color = 0.81*[1 1 1];	% couleur du plancher
      
       xf = [FIG_XMIN;FIG_XMIN;FIG_XMAX;FIG_XMAX];
       for i=1:length(R)
         yf = [R(i)+S(i)*FIG_XMIN;FIG_YMIN;FIG_YMIN;R(i)+S(i)*FIG_XMAX];
         fill(xf,yf,floor_color,'EdgeColor',floor_color);
       end      
      
      endif % If dessin du plancher
      
      %représentation de la chaîne
      fun_trace(x,y);
      
      %variables par défaut
      e=0;ce=0;ci=0;g=0;ae=0;ai=0;hl=0;
      
    case 2
      e = fun_e(x,y);
      ce = fun_ce(x,y);
      ci = fun_ci(x,y);
      
      %variables par défaut
      g=0;ae=0;ai=0;hl=0;
      
    case 4
      e = fun_e(x,y);
      ce = fun_ce(x,y);
      ci = fun_ci(x,y);
      g = fun_g(x,y);
      ae = fun_ae(x,y);
      ai = fun_ai(x,y);
      
      %variable par défaut
      hl=0;
      
    case 5
      hl = fun_hl(x,y,lme);
      
      %variables par défaut
      e=0;ce=0;ci=0;g=0;ae=0;ai=0;
      
    case 6
      test_derivees(xy,x,y);
      
      %variables par défaut
      e=0;ce=0;ci=0;g=0;ae=0;ai=0;hl=0;
      
   endswitch

endfunction
