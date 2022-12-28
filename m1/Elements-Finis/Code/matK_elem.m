function [Kel] = matK_elem(S1, S2, S3, ref)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mat_elem :
% calcul la matrices de raideur elementaire en P1 lagrange
%
% SYNOPSIS [Kel] = mat_elem(S1, S2, S3)
%          
% INPUT * S1, S2, S3 : les 2 coordonnees des 3 sommets du triangle 
%                      (vecteurs reels 1x2)
%
% OUTPUT - Kel matrice de raideur elementaire (matrice 3x3)
%
% NOTE (1) Utilisation d une quadrature a 3 point d ordre 2
%    
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% preliminaires, pour faciliter la lecture:
format long
x1 = S1(1); y1 = S1(2);
x2 = S2(1); y2 = S2(2);
x3 = S3(1); y3 = S3(2);

% les 3 normales a l'arete opposees (de la longueur de l'arete)
norm = zeros(3, 2);
norm(1, :) = [y2-y3, x3-x2];
norm(2, :) = [y3-y1, x1-x3];
norm(3, :) = [y1-y2, x2-x1];

% D est, au signe pres, deux fois l'aire du triangle
D = ((x2-x1)*(y3-y1) - (y2-y1)*(x3-x1));
if (abs(D) <= eps) 
  error('l aire d un triangle est nulle!!!'); 
end;

% Calcul de la transformation vers le triangle de reference
B_l = [(x2 - x1)  (x3 - x1); (y2 - y1)  (y3 - y1)];
% On observe que det(B) = D
S_l = [x1; y1];
grad_w = [[1; 0] [0; 1] [-1; -1]];
inverse_B_l_transpose = inv(B_l');

% Mise en place de la quadrature de Gauss-Legendre
s_0 = 1/6;
s_1 = 2/3;
point = [[s_0  s_0]; [s_1  s_0]; [s_0  s_1]];
poids = 1/6;

%%%%%%%%%%%%%%%%%%%%%%%

% calcul de la matrice de rigidité
% -------------------------------
Kel = zeros(3,3);
for i=1:3
  for j=1:3
    produit_scalaire_B_grad_Wij = ((inverse_B_l_transpose * grad_w(:, i))' 
                                  * (inverse_B_l_transpose * grad_w(:, j)));
    for q =1:3
      coor_non_elem = F_l(point(q, 1), point(q, 2), B_l, S_l);
	    Kel(i, j) = Kel(i, j) + (poids 
                               .* sigma(coor_non_elem(1), coor_non_elem(2), ref)
                               .* produit_scalaire_B_grad_Wij
                               .* abs(D)
                               );
                                                                 
    end;
  end; % j
end; % i
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                        fin de la routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%2020
