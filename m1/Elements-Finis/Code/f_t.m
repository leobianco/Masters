function val = f_t(x,y,t)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% f_t :
% Evaluation de la fonction second membre dans le cadre du problème instationnaire.
%
% SYNOPSIS val = f_t(x,y,t)
%          
% INPUT * x,y : les 2 coordonnees du point ou on veut evaluer la fonction.
%       *   t : le temps
%
% OUTPUT - val: valeur de la fonction sur ce point.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Original
val = 600*exp(-5*t)*exp(-((x-1)/0.8).^2-((y-1)/0.8).^2 );

% Autres tests
%val = 600*exp(-5*t)*exp(-((x-0.4)/0.8).^2-((y-0.4)/0.8).^2 );

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                     fin de la fonction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%2020
