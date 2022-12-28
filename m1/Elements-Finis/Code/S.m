function val = S(x, y)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S :
% Renvoie la valeur du terme source de chaleur
%
% SYNOPSIS val = S(x, y)
%          
% INPUT * x,y : les 2 coordonnees du sommet ou calculer S
%
% OUTPUT - val: valeur de la fonction S sur ce sommet.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
  % Questions 1.16 et 1.17
  val = 600*exp(-((x-1)/0.8).^2 - ((y-1)/0.8).^2);
  
  % Question 2.5, validation du code
  %val = (1+2*pi^2)*cos(pi*x)*cos(pi*y);
  
  % Question 2.6
  %val = sin(pi*x/2)*sin(pi*y/2);

endfunction
